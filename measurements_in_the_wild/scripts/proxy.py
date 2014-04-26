#! /usr/bin/env python2

from itertools  import groupby
import socket

from traceroute import traceroute
from util       import ilen

def proxy_test(target, max_ttl, timeout, n_https_syns=10, n_http_syns=10):
    """
    
    """

    def path_IPs(path):
        return tuple(None if resp == None else resp.src for _, resp in path)

    target_ip = socket.gethostbyname(target)

    # Probes the target with HTTPS SYNs to computes the probability that a path
    # change (load-balancing, ...).
    # This estimation will be used to compute the probability that, when probing
    # the target with HTTP SYNs, any path difference is caused by proxies and
    # not by load balancing.

    paths = []
    for _ in xrange(n_https_syns):
        path = traceroute(target_ip, max_ttl, timeout, dport=443)

        # Ignores unreachable traces. Only remember the response source IPs.
        if path != None:
            paths.append(path_IPs(path))

    n_https_paths = len(paths)
    if n_https_paths == 0:
        return None

    def n_differ_group(group):
        """
        For a given group of identical paths, returns the number of paths which
        doesn't match the group, times the number of path in the group.
        """
        n = ilen(group)
        return n * (n_https_paths - n)

    # For each probed path, computes how many paths differs. Sum these values.
    # Complexity : O(n log n).
    paths.sort()
    n_differ = sum(n_differ_group(group) for _, group in groupby(paths))

    # Deduces an estimation of the probability of two given paths to differ.
    prob_differ = float(n_differ) / float(n_https_paths**2)

    # Probes the target with HTTP SYNs. If a traced path equals one previously
    # probed path, considers that there is no proxy.

    unique_paths = set(paths)

    n_http_paths = 0
    for _ in xrange(n_http_syns):
        path = traceroute(target_ip, max_ttl, timeout, dport=80)

        # Ignores unreachable traces. Only remember the response source IPs.
        if path != None:
            if path_IPs(path) in unique_paths:
                return (False, 1)
            else:
                n_http_paths += 1

    # No HTTP path is the same as any HTTPS. HTTP is probably running behind a
    # proxy. Computes the probability that no corresponding path have been found
    # because of something else that a proxy.
    print "P: {0} - HTTPS: {1} - HTTP: {2}".format(
        prob_differ, n_https_paths, n_http_paths
    )

    return (True, 1 - prob_differ**(n_https_paths * n_http_paths))
