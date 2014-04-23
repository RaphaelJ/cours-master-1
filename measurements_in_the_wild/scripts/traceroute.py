#! /usr/bin/env python2

import socket
from itertools import chain, ifilter, takewhile

from scapy.all  import *

from util      import lazy_property

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