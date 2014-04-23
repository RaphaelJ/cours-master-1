#! /usr/bin/env python2

import threading
from collections import deque
from itertools   import count, izip

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

    gen = iterable.__iter__()

    fifo = deque()

    try:
        # Starts the first buffer_size item concurrently.
        for _ in xrange(buffer_size):
            task = Task(next(gen))
            task.start()
            fifo.append(task)

        # Consumes the queue and dispatches a new task when a task finishes.
        while len(fifo) > 0:
            # Takes the first element from the generator before popping the
            # queue's head as next() might raise an exception. This exception
            # would discard the popped element if executed before the yield
            # instruction.
            new_task = Task(next(gen))

            # Waits for the first task to finish.
            task = fifo.popleft()
            task.join()

            # Starts the new task.
            new_task.start()
            fifo.append(new_task)

            yield task.result

    except StopIteration: # Raised when gen is empty.
        # No more item to dispatch in new threads.
        # Waits for all tasks to finish.
        while len(fifo) > 0:
            task = fifo.popleft()
            task.join()
            yield task.result

def ilen(iterable):
    """Generalization of len() which runs on any iterable. Complexity: O(n)."""
    return sum(1 for _ in iterable)

def ip_to_int(str_ip):
    """Converts an IP string to its 32 bits value."""
    byts = map(int, str_ip.split('.'))
    return byts[3] + (byts[2] << 8) + (byts[1] << 16) + (byts[0] << 24)

def is_private_ip(str_ip):
    """
    Returns True if the IP is a private IP (network is 10.0.0.0/8, 172.16.0.0/12
    or 192.168.0.0/16).
    """

    def in_net(int_ip, net, net_mask):
        int_net      = ip_to_int(net)
        int_net_mask = ip_to_int(net_mask)

        return (int_ip & int_net_mask) ^ int_net == 0

    int_ip = ip_to_int(str_ip)

    return in_net(int_ip, "10.0.0.0",    "255.0.0.0")   \
        or in_net(int_ip, "172.16.0.0",  "255.240.0.0") \
        or in_net(int_ip, "192.168.0.0", "255.255.0.0")

def percentage(value, total):
    """Returns the given value ratio as a percentage."""
    return (float(value) / float(total)) * 100

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
