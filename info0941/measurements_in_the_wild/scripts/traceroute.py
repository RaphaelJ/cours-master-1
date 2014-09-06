#! /usr/bin/env python2

import socket
from itertools import chain, ifilter, takewhile

import scapy.all as scapy

def traceroute(target, max_ttl, timeout, dport=80, verbose=None, \
               tcp_options=[]):
    """
    Sends an TCP SYN request on dport to the target with a varying TTL.
    Return None if the target was unreachable or returns an iterable of pairs
    representing the hops. The first item of the tuple contains the packet sent
    whereas the second one contains the packet received (None if the router did
    not respond).
    """

    def by_ttl(ans1, ans2):
        """Compares two answers by the TTL of their respective query."""
        qry1, _ = ans1
        qry2, _ = ans2
        return cmp(qry1.ttl, qry2.ttl)

    def from_target(target_ip, resp):
        """Returns True if the response comes from the given target."""
        return resp.src == target_ip

    try:
        target_ip = socket.gethostbyname(target)
    except socket.gaierror:
        return None

    # Sends a set of TCP SYN packets with a varying TTL. The following code is
    # from the Scapy's traceroute source code, slightly adjusted.
    anss, unanss = scapy.sr(
        scapy.IP(
            dst=target_ip, id=scapy.RandShort(), ttl=(1, max_ttl)
        )/scapy.TCP(
            seq=scapy.RandInt(), sport=scapy.RandShort(), dport=dport,
            options=tcp_options
        ), timeout=timeout, verbose=verbose,
        # We only consider ICMP error packets and TCP packets with at least the
        # ACK flag set *and* either the SYN or the RST flag set.
        filter="(icmp and (icmp[0]=3 or icmp[0]=4 or icmp[0]=5 or icmp[0]=11 \
                or icmp[0]=12)) or (tcp and (tcp[13] & 0x16 > 0x10))"
    )

    if any(from_target(target_ip, resp) for _, resp in anss):
        # Target reached.
        # Combines the list of answered queries with the list of non-answered
        # queries.
        # Sorts them by their TTL. Unanswered queries will have a None answer.
        # Selects the path up to the destination
        return takewhile(
            lambda (_, resp): resp == None or not from_target(target_ip, resp),
            sorted(chain(anss, ((unans, None) for (unans) in unanss)), by_ttl)
        )
    else:
        # Failed to reach the target.
        return None
