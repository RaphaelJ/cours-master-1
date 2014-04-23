#! /usr/bin/env python2

DEFAULT_CSV_FILE    = "top-1m.csv"
DEFAULT_N_SITES     = 50
DEFAULT_MAX_TTL     = 30
DEFAULT_TIMEOUT     = 2
DEFAULT_PARALLELISM = 1

import argparse
from itertools  import count, ifilter, islice, izip

from alexa      import parse_top_sites
from traceroute import traceroute
from util       import buffered_par_map, ilen, is_private_ip, percentage

def get_args_parser():
    parser = argparse.ArgumentParser(
        description="Execute TCP traceroutes to a given list of websites.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument(
        "--csv_file", metavar="csv_file", type=str, default=DEFAULT_CSV_FILE,
        help="CSV file containing the list of websites to probe."
    )
    parser.add_argument(
        "--n_sites", "-n", metavar="n_sites", type=int, default=DEFAULT_N_SITES,
        help="Maximum number of sites to probe."
    )
    parser.add_argument(
        "--max_ttl", metavar="max_ttl", type=int, default=DEFAULT_MAX_TTL,
        help="Largest TTL tried to reach a target."
    )
    parser.add_argument(
        "--timeout", "-t", metavar="timeout", type=int, default=DEFAULT_TIMEOUT,
        help="Timeout before a response to be discarded."
    )
    parser.add_argument(
        "--parallelism", "-p", metavar="parallelism", type=int,
        default=DEFAULT_PARALLELISM,
        help="How many traceroutes can be executed at the same time."
    )
    return parser

cli_args    = get_args_parser().parse_args()
csv_file    = cli_args.csv_file
n_sites     = cli_args.n_sites
max_ttl     = cli_args.max_ttl
timeout     = cli_args.timeout
parallelism = cli_args.parallelism

sites = islice(parse_top_sites(csv_file), n_sites)

# Number 
n_reachable = 0
unreachable = []

# Contains the number of reachable sites for a given number of hops.
# path_hops_hist[i] is the number of sites with an hop count equals to i.
path_hops_hist = [0] * (max_ttl + 1)

# Number of un-silent and silent routers, regarding the way they handle expired
# TTLs.
n_silent = 0

# Routers which respond with time-expired IMCP packets.
unsilent = []

### Probing

trace_site = lambda site: (site, traceroute(site, max_ttl, timeout))

for site, path in buffered_par_map(trace_site, sites, parallelism):
    if path == None:
        unreachable.append(site)
        print "Doesn't succeed to reach {0}.".format(site)
    else:
        n_reachable += 1
        print "Succeed to reach {0}.".format(site)

        for qry, resp in path:
            if resp == None:
                n_silent += 1
            else:
                unsilent.append(resp.src)

        path_hops_hist[len(path)] += 1

### Site stats

n_sites = n_reachable + len(unreachable)

sum_path_hops = sum(
    n_hops * n_sites for n_hops, n_sites in izip(count(0), path_hops_hist)
)
avg_path_hops = float(sum_path_hops) / float(n_reachable)

print "Probed sites: {0}.".format(n_sites)
print "Reachable sites: {0} ({1} %).".format(
    n_reachable, percentage(n_reachable, n_sites)
)

print "Unreachable sites ({0}) : {1}.".format(
    len(unreachable), ", ".join(unreachable)
)

print "Average path hops: {0}.".format(avg_path_hops)

### Routers stats

n_routers                 = n_silent + len(unsilent)
n_unsilent                = len(unsilent)
n_unique_unsilent         = len(set(unsilent))
private_unsilent          = filter(is_private_ip, unsilent)
n_private_unsilent        = len(private_unsilent)
n_unique_private_unsilent = len(set(private_unsilent))

print "Total routers: {0}.".format(n_routers)
print "Un-silent routers: {0} ({1}%, unique un-silent: {2}).".format(
    n_unsilent, percentage(n_unsilent, n_routers), n_unique_unsilent
)
print "Un-silent routers announcing a private IP: {0} ({1}%)".format(
    n_private_unsilent, percentage(n_private_unsilent, n_unsilent)
)
print "Unique un-silent routers announcing a private IP: {0} ({1}%)".format(
    n_unique_private_unsilent,
    percentage(n_unique_private_unsilent, n_unique_unsilent)
)
