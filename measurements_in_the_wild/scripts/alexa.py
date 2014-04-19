
def parse_top_sites(filename):
    """Parses the specified Alexa CSV file and returns the urls."""

    with open(filename) as f:
        for l in f:
            # Removes the tailing \n and takes the second field.
            yield l[:-1].split(',')[1]
