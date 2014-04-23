#! /usr/bin/env python2

import threading
from collections import deque

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
