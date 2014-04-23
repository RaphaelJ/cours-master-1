#! /usr/bin/env python2

DEFAULT_CSV_FILE    = "top-1m.csv"
DEFAULT_N_SITES     = 50
DEFAULT_MAX_TTL     = 35
DEFAULT_RETRY       = 2
DEFAULT_TIMEOUT     = 10
DEFAULT_PARALLELISM = 1

import argparse, threading
from itertools import chain, count, ifilter, islice, izip, takewhile

from scapy.all import *

def parse_top_sites(csv_file):
    """Parses the specified Alexa CSV file and returns the urls."""

    with open(csv_file) as f:
        for l in f:
            # Removes the tailing \n and takes the second field.
            yield l[:-1].split(',')[1]

def lazy_property(f):
    """
    Decorator which transforms a function into a lazy evaluated property.
    The function will only be called the first time the value will be requested.
    """

    def inner(self):
        attr = "_" + f.__name__ + "_lazy"

        if not hasattr(self, attr):
            setattr(self, attr, f(self))

        return getattr(self, attr)

    return property(inner)

def buffered_par_map(f, iterable, buffer_size):
    """
    Executes f on each item of iterable with evaluating the buffer_size next
    items in parallel.
    """

    class Task(threading.Thread):

        def __init__(self, item):
            threading.Thread.__init__(self)
            self.item = item

        def run(self):
            self.result = f(self.item)

    def take(n, gen):
        l = []
        try:
            for _ in xrange(n):
                l += next(gen)
        finally:
            return l

    for i in iterable:
        task = Task(i)
        task.start()
        task.join()
        yield task.result

class Path:

    def __init__(self, path):
        self.path = list(path)

    @lazy_property
    def n_silent(self):
        return sum(1 for _ in ifilter(lambda node: node == None, self))

    @property
    def n_unsilent(self):
        return len(self) - self.n_silent

    def __iter__(self):
        return self.path.__iter__()

    def __len__(self):
        return len(self.path)

def traceroute(target, max_ttl, retry, timeout):
    """
    Sends an TCP SYN request on port 80 to the target with a varying TTL.
    Returns an instance of Path if the target has been reached or None if the
    target was unreachable.
    """

    def by_ttl(ans1, ans2):
        """Compares two answers by the TTL of their respective query."""
        qry1, _ = ans1
        qry2, _ = ans2
        return cmp(qry1.ttl, qry2.ttl)

    def from_target(target_ip, ans):
        """Returns True if the response comes from the given target."""
        _, resp = ans
        return resp.src == target_ip

    # Sends a bunch of SYN packets to reach the destination. Varies the TTL.
    # Sends packets with TTL from 1 to max_ttl at the same time as it is faster.
    try:
        target_ip = socket.gethostbyname(target)
    except socket.gaierror:
        return None
    packet       = IP(dst=target_ip, ttl=(0, max_ttl))/TCP(dport=80, flags="S")
    anss, unanss = sr(packet, retry=retry, timeout=timeout)

    if any(from_target(target_ip, ans) for ans in anss):
        # Target reached.
        # Combines the list of answered queries with the list of non-answered
        # queries.
        # Sorts them by their TTL. Unanswered queries will have a None answer.
        packets = list(chain(anss, ((unans, None) for (unans) in unanss)))
        packets.sort(by_ttl)

        # Selects the path up to the destination
        return Path(
            takewhile(
                lambda p: p[1] == None or not from_target(target_ip, p),
                packets
            )
        )
    else:
        # Failed to reach the target.
        return None

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
