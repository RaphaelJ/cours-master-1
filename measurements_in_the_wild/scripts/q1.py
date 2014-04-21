ALEXA_CSV = "top-1m.csv"
N_SITES   = 5000
MAX_TTL   = 35
RETRY     = 3
TIMEOUT   = 2

from itertools import chain, islice
from funtools  import partial

def parse_top_sites(filename):
    """Parses the specified Alexa CSV file and returns the urls."""

    with open(filename) as f:
        for l in f:
            # Removes the tailing \n and takes the second field.
            yield l[:-1].split(',')[1]

def traceroute(target):
    """
    Sends an HTTP request to the target with a varying TTL.
    Return the number or None if failed to reach the target.
    
    """

    def by_tld(ans1, ans2):
        """Compares two answers by the TLD of their respective query."""
        query1, _ = ans1
        query2, _ = ans2
        return cmp(query1.tld, query2.tld)

    def is_time_exceeded(ans):
        """Returns True if the response is an time-exceeded ICMP packet."""
        _, resp = ans
        return isinstance(resp, ICMP)

    def from_target(target_ip, ans):
        """Returns True if the response is an time-exceeded ICMP packet."""
        _, resp = ans
        return isinstance(resp, ICMP)

    def is_syn_ack(ans):
        """Returns True if the response is an SYN-ACK TCP packet."""
        _, resp = ans
        return isinstance(resp, ICMP)

    # Sends a bunch of SYN packets to reach the destination. Varies the TTL.
    # Sends packets with TTL from 1 to MAX_TTL at the same time as it is faster.
    target_ip = socket.gethostbyname(target)
    packet    = IP(dst=target_ip, ttl=(1, MAX_TTL))/TCP(dport=80, flags="S")
    anss, _   = sr(packet, retry=RETRY, timeout=TIMEOUT)

    if any(from_target(target_ip, ans) for ans in anss):
        anss.sort(by_tld)
    else:
        # Failed to reach the target
        return None

    

    n_time_exceeded = len(takewhile(is_time_exceeded, ans))

sites = islice(parse_top_sites(ALEXA_CSV), stop=N_SITES)

(traceroute(site) for site in sites)
