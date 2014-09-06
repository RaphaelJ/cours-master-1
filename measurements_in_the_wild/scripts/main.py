#! /usr/bin/env python2

DEFAULT_CSV_FILE    = "top-1m.csv"
DEFAULT_N_SITES     = 50
DEFAULT_MAX_TTL     = 30
DEFAULT_TIMEOUT     = 2
DEFAULT_PARALLELISM = 1

import argparse
from itertools  import count, ifilter, islice, izip

import matplotlib
matplotlib.use('Agg') # Other back-ends require a X display to run.
import matplotlib.pyplot as plt

from alexa      import parse_top_sites
from proxy      import proxy_test
from traceroute import traceroute
from util       import accumulate, buffered_par_map, get_tcp_mss, ilen, \
                       is_private_ip, normalize, percentage, response_is_rfc1812

def get_cli_args_parser():
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
        "--max-ttl", metavar="max_ttl", type=int, default=DEFAULT_MAX_TTL,
        help="Largest TTL tried to reach a target."
    )
    parser.add_argument(
        "--timeout", "-t", metavar="timeout", type=int, default=DEFAULT_TIMEOUT,
        help="Timeout before a response to be discarded."
    )
    parser.add_argument(
        "--test-proxy", "-p", action='store_true',
        help="Test if a proxy is running for each site (slow)."
    )
    parser.add_argument(
        "--cdf-file", metavar="cdf_file", type=str, default=None,
        help="""
             Where to save the CDF graphs of the path length (hops), the
             RFC1812-compliant router positions and the MSS change positions.
             If not specified, will be displayed in a GUI.
             """
    )
    parser.add_argument(
        "--parallelism", metavar="parallelism", type=int,
        default=DEFAULT_PARALLELISM,
        help="How many traceroutes can be executed at the same time."
    )
    parser.add_argument(
        "--verbose", "-v", action='store_true', help="Enable Scapy traces."
    )
    return parser

if __name__ == "__main__":
    cli_args      = get_cli_args_parser().parse_args()
    csv_file      = cli_args.csv_file
    n_sites       = cli_args.n_sites
    max_ttl       = cli_args.max_ttl
    timeout       = cli_args.timeout
    test_proxy    = cli_args.test_proxy
    cdf_file      = cli_args.cdf_file
    parallelism   = cli_args.parallelism
    verbose       = cli_args.verbose

    sites = islice(parse_top_sites(csv_file), n_sites)

    # Width of the hops histograms.
    hist_size = max_ttl + 1

    # Number of reachable sites and the list of sites which were not reached.
    n_reachable = 0
    unreachable = []

    # Number of reachable sites for a given number of hops.
    # path_hops_hist[i] is the number of sites with an hop count equals to i.
    path_hops_hist = [0] * hist_size

    # Number of silent regarding the way they handled expired TTLs.
    n_silent = 0

    # Routers which responded with time-expired IMCP packets.
    unsilent = []

    # Number RFC1812-compliant routers at a given number of hops.
    rfc1812_hist = [0] * hist_size
    # Set of unique RFC1812-compliant routers.
    rfc1812_unique = set()

    # Number of sites on witch the MSS value changed.
    n_mss_changed_path = 0
    # Number routers which changed the MSS at a given number of hops.
    mss_changed_hist = [0] * hist_size
    # Set of unique routers which change the MSS.
    mss_changed_unique = set()

    if test_proxy:
        # Set of sites which have been detected as running behind a proxy.
        # Contains tuples of type ("domain.tld", has a proxy probability).
        proxied = []

    ### Probing

    #INIT_MSS = None
    #INIT_MSS = 1
    INIT_MSS = 536

    def trace_site(site):
        trace = traceroute(
            site, max_ttl, timeout, verbose=verbose,
            tcp_options=[('MSS', INIT_MSS)]
        )
        return site, trace

    for site, path in buffered_par_map(trace_site, sites, parallelism):
        if path == None:
            unreachable.append(site)
            print "Doesn't succeed to reach {0}.".format(site)
        else:
            n_reachable += 1
            print "Succeed to reach {0}.".format(site)

            path        = list(path)
            mss         = INIT_MSS
            mss_changed = False
            n_hops      = len(path)
            for qry, resp in path:
                if resp == None:
                    n_silent += 1
                else:
                    if response_is_rfc1812(resp):
                        rfc1812_hist[n_hops] += 1
                        rfc1812_unique.add(resp.src)

                        tcp_resp = resp.payload.payload.payload
                        new_mss  = get_tcp_mss(tcp_resp)
                        if new_mss != mss:
                            mss_changed_hist[n_hops] += 1
                            mss = new_mss
                            mss_changed_unique.add(resp.src)
                            mss_changed = True

                    unsilent.append(resp.src)

            if mss_changed:
                n_mss_changed_path += 1

            path_hops_hist[n_hops] += 1

            if test_proxy:
                print "Testing {0} for a proxy ...".format(site)
                result = proxy_test(site, max_ttl, timeout)
                if result != None:
                    has_proxy, prob = result
                    if has_proxy:
                        proxied.append((site, prob))

    print "### Site stats ###"

    n_sites = n_reachable + len(unreachable)

    sum_path_hops = sum(
        n_hops * n_sites for n_hops, n_sites in izip(count(0), path_hops_hist)
    )
    avg_path_hops = float(sum_path_hops) / float(n_reachable)

    print "Probed sites: {0}.".format(n_sites)
    print "Reachable sites: {0} ({1} %).".format(
        n_reachable, percentage(n_reachable, n_sites)
    )

    print "Unreachable sites ({0}): {1}.".format(
        len(unreachable), ", ".join(unreachable)
    )

    print "Average path hops: {0}.".format(avg_path_hops)

    print "Path on which MSS changed: {0} ({1} %).".format(
        n_mss_changed_path, percentage(n_mss_changed_path, n_sites)
    )

    if test_proxy:
        print "Site running behind a proxy ({0}): {1}.".format(
            len(proxied),
            ", ".join("{0} (prob. : {1}%)".format(site, prob*100)
                for site, prob in proxied
            )
        )

    print "### Routers stats ###"

    n_routers                 = n_silent + len(unsilent)
    n_unsilent                = len(unsilent)
    n_unique_unsilent         = len(set(unsilent))
    private_unsilent          = filter(is_private_ip, unsilent)
    n_private_unsilent        = len(private_unsilent)
    n_unique_private_unsilent = len(set(private_unsilent))

    n_rfc1812                 = sum(rfc1812_hist)
    n_rfc1812_unique          = len(rfc1812_unique)

    n_mss_changed             = sum(mss_changed_hist)
    n_mss_changed_unique      = len(mss_changed_unique)

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

    print "RFC1812-compliant routers: {0} ({1}%, {2}% of un-silent)".format(
        n_rfc1812, percentage(n_rfc1812, n_routers),
        percentage(n_rfc1812, n_unsilent)
    )
    print "Unique RFC1812-compliant routers: {0} ({1}% of unique "\
          "un-silent)".format(
        n_rfc1812_unique, percentage(n_rfc1812_unique, n_unique_unsilent)
    )

    print "Routers which changed the MSS: {0} ({1}%, {2}% of un-silent)".format(
        n_mss_changed, percentage(n_mss_changed, n_routers),
        percentage(n_mss_changed, n_unsilent)
    )
    print "Unique routers which changed the MSS: {0} ({1}% of unique "\
          "un-silent)".format(
        n_mss_changed_unique,
        percentage(n_mss_changed_unique, n_unique_unsilent)
    )

    print "### Graphs ###"

    print "Plotting the graphs ..."

    plt.xlabel("Position (hops)")
    plt.ylabel("Density")
    plt.grid(True)

    def hist_to_cdf(histogram):
        return list(accumulate(normalize(histogram)))

    plt.plot(hist_to_cdf(path_hops_hist), label="Destinations")
    plt.plot(hist_to_cdf(rfc1812_hist), label="RFC1812-compliant routers")
    plt.plot(hist_to_cdf(mss_changed_hist),label="MSS changes")

    plt.xlim(0, hist_size - 1)
    plt.ylim(0, 1)
    plt.legend(loc=4)

    if cdf_file != None:
        plt.savefig(cdf_file)
    else:
        plt.show()
