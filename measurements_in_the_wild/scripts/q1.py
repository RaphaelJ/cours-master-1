ALEXA_CSV = "top-1m.csv"
N_SITES   = 5000
MAX_TTL   = 35
RETRY     = 3

from itertools import islice

def parse_top_sites(filename):
    """Parses the specified Alexa CSV file and returns the urls."""

    with open(filename) as f:
        for l in f:
            # Removes the tailing \n and takes the second field.
            yield l[:-1].split(',')[1]

def traceroute(target):
    """Sends a """

    target_ip = socket.gethostbyname(target)
    packet = IP(dst=target_ip, ttl=(0, MAX_TTL))/TCP(dport=80, flags="S")

    sr(packet, retry=RETRY)

sites = islice(parse_top_sites(ALEXA_CSV), stop=N_SITES)
