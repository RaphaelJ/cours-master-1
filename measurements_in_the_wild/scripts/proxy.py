#! /usr/bin/env python2

DEFAULT_MAX_TTL      = 30
DEFAULT_TIMEOUT      = 2
DEFAULT_HTTPS_TRACES = 10
DEFAULT_HTTP_TRACES  = 10

import argparse, socket
from itertools  import groupby

from traceroute import traceroute
from util       import ilen

def proxy_test(target, max_ttl, timeout, verbose=None, \
               n_https_traces=DEFAULT_HTTPS_TRACES,    \
               n_http_traces=DEFAULT_HTTP_TRACES):
    """
    Tests if the target is running behind a proxy.
    Does this by comparing the traceroute traces of a set of HTTPS and HTTP SYN
    packets.
    Returns None if the target was unreachable or a tuple if it was reachable.
    The tuple (has_proxy, probability) gives if a proxy is running and an
    estimation of the probability that the proxy really exist (false positive
    are possible because of path load-balancing).
    """

    def path_IPs(path):
        """
        Returns the tuple of routers IPs from a traceroute trace.
        Returns a tuple instead of a list as lists are not hashable and can not
        be used in sets.
        """
        return tuple(None if resp == None else resp.src for _, resp in path)

    target_ip = socket.gethostbyname(target)

    # Probes the target with HTTPS SYNs to gather some (probably) non-proxied
    # paths.

    paths = []
    for _ in xrange(n_https_traces):
        path = traceroute(
            target_ip, max_ttl, timeout, dport=443, verbose=verbose
        )

        # Ignores unreachable traces. Only remember the response source IPs.
        if path != None:
            paths.append(path_IPs(path))

    n_https_paths = len(paths)
    if n_https_paths == 0:
        return None

    # Probes the target with HTTP SYNs. If a traced path equals one previously
    # probed path, considers that there is no proxy.

    unique_paths = set(paths)

    n_http_paths = 0
    for _ in xrange(n_http_traces):
        path = traceroute(
            target_ip, max_ttl, timeout, dport=80, verbose=verbose
        )

        # Ignores unreachable traces. Only remember the response source IPs.
        if path != None:
            if path_IPs(path) in unique_paths:
                return (False, 0)
            else:
                n_http_paths += 1

    # No HTTP path is the same as any HTTPS one. HTTP is probably running behind
    # a proxy. Computes the probability that no corresponding path have been
    # found because of something else that a proxy (load-balancing ...).

    # For each probed HTTPS path, computes how many paths differs. Sum these
    # values.
    # Complexity : O(n log n).

    def n_differ_group(group):
        n = ilen(group)
        return n * (n_https_paths - n)

    paths.sort()
    n_differ = sum(n_differ_group(group) for _, group in groupby(paths))

    # Deduces an estimation of the probability of two given paths to differ.
    prob_differ = float(n_differ) / float(n_https_paths * (n_https_paths - 1))

    # print "P: {0} - HTTPS: {1} - HTTP: {2}".format(
    #    prob_differ, n_https_paths, n_http_paths
    # )

    # Uses this estimation to compute the probability that our result is a
    # product of a proxy and not of routing dynamics.
    prob_result = 1 - prob_differ**(n_https_paths * n_http_paths)

    return (True, prob_result)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Execute HTTPS and HTTP traceroutes to detect TCP proxies.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    parser.add_argument("site", metavar="site", type=str, help="Site to test.")
    parser.add_argument(
        "--max-ttl", metavar="max_ttl", type=int, default=DEFAULT_MAX_TTL,
        help="Largest TTL tried to reach a target."
    )
    parser.add_argument(
        "--timeout", "-t", metavar="timeout", type=int, default=DEFAULT_TIMEOUT,
        help="Timeout before a response to be discarded."
    )
    parser.add_argument(
        "--https-traces", metavar="https_traces", type=int,
        default=DEFAULT_HTTPS_TRACES,
        help="Number of traceroute over the HTTPS protocol."
    )
    parser.add_argument(
        "--http-traces", metavar="http_traces", type=int,
        default=DEFAULT_HTTP_TRACES,
        help="Number of traceroute over the HTTP protocol."
    )
    parser.add_argument(
        "--verbose", "-v", action='store_true', help="Enable Scapy traces."
    )

    cli_args = parser.parse_args()

    result = proxy_test(
        cli_args.site, cli_args.max_ttl, cli_args.timeout, cli_args.verbose,
        cli_args.https_traces, cli_args.http_traces
    )

    if result == None:
        print "Destination unreachable."
    else:
        has_proxy, prob = result
        if has_proxy:
            print "HTTP traffic doesn't pass through the same path as the " \
                  "HTTPS traffic "\
                  "(probability of a proxy: {0}%).".format(prob * 100)
        else:
            print "HTTP traffic doesn't seem to pass through a proxy."
