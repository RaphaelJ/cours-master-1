#! /usr/bin/env python2

DEFAULT_CSV_FILE    = "top-1m.csv"
DEFAULT_N_SITES     = 50
DEFAULT_MAX_TTL     = 35
DEFAULT_RETRY       = 2
DEFAULT_TIMEOUT     = 10
DEFAULT_PARALLELISM = 1

import argparse
from itertools  import count, islice, izip

from alexa      import parse_top_sites
from traceroute import traceroute
from util       import buffered_par_map

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
        "--retry", "-r", metavar="retry", type=int, default=DEFAULT_RETRY,
        help="Number of time a probe will be resent with no received response."
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
retry       = cli_args.retry
timeout     = cli_args.timeout
parallelism = cli_args.parallelism

sites = islice(parse_top_sites(csv_file), n_sites)

# Number 
n_reachable = n_unreachable = 0

# Contains the number of reachable sites for a given number of hops.
# path_hops_hist[i] is the number of sites with an hop count equals to i.
path_hops_hist = [0] * (max_ttl + 1)

# Number of un-silent and silent routers, regarding the way they handle expired
# TTLs.
n_unsilent = n_silent = 0

trace_site = lambda site: traceroute(site, max_ttl, retry, timeout)

for path in buffered_par_map(trace_site, sites, parallelism):
    if path == None:
        n_unreachable += 1
    else:
        n_reachable += 1

        n_silent                  += path.n_silent
        n_unsilent                += path.n_unsilent
        path_hops_hist[len(path)] += 1

n_sites = n_reachable + n_unreachable
sum_path_hops = sum(
    n_hops * n_sites for n_hops, n_sites in izip(count(0), path_hops_hist)
)
avg_path_hops = float(sum_path_hops) / float(n_reachable)

print "Probed sites: {0}".format(n_sites)
print "Reachable sites: {0} ({1:2f})%".format(
    n_reachable, (float(n_reachable) / float(n_sites)) * 100
)
print "Average path hops: {0}".format(avg_path_hops)
