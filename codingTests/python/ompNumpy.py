import os
import time
import math
import numpy as np
from numpy.linalg import svd as svd
import multiprocessing


# If numpy is compiled for OpenMP, then make sure to control
# the number of OpenMP threads via the OMP_NUM_THREADS environment
# variable before running this benchmark.


MATRIX_SIZE = 1000
MATRIX_COUNT = 16


def rnd_matrix():
    offset = np.random.randint(1,10)
    stretch = 2*np.random.rand()+0.1
    return offset + stretch * np.random.rand(MATRIX_SIZE, MATRIX_SIZE)


print "Creating input matrices in parent process."
# Create input in memory. Children access this input.
INPUT = [rnd_matrix() for _ in xrange(MATRIX_COUNT)]


def worker_function(result_queue, worker_index, chunk_boundary):
    """Work on a certain chunk of the globally defined `INPUT` list.
    """
    result_chunk = []
    for m in INPUT[chunk_boundary[0]:chunk_boundary[1]]:
        # Perform single value decomposition (CPU intense).
        u, s, v = svd(m)
        # Build single numeric value as output.
        output =  int(np.sum(s))
        result_chunk.append(output)
    result_queue.put((worker_index, result_chunk))


def work(n_workers=1):
    def calc_chunksize(l, n):
        """Rudimentary function to calculate the size of chunks for equal
        distribution of a list `l` among `n` workers.
        """
        return int(math.ceil(len(l)/float(n)))

    # Build boundaries (indices for slicing) for chunks of `INPUT` list.
    chunk_size = calc_chunksize(INPUT, n_workers)
    chunk_boundaries = [
        (i, i+chunk_size) for i in xrange(0, len(INPUT), chunk_size)]

    # When n_workers and input list size are of same order of magnitude,
    # the above method might have created less chunks than workers available.
    if n_workers != len(chunk_boundaries):
        return None

    result_queue = multiprocessing.Queue()
    # Prepare child processes.
    children = []
    for worker_index in xrange(n_workers):
        children.append(
            multiprocessing.Process(
                target=worker_function,
                args=(
                    result_queue,
                    worker_index,
                    chunk_boundaries[worker_index],
                    )
                )
            )

    # Run child processes.
    for c in children:
        c.start()

    # Create result list of length of `INPUT`. Assign results upon arrival.
    results = [None] * len(INPUT)

    # Wait for all results to arrive.
    for _ in xrange(n_workers):
        worker_index, result_chunk = result_queue.get(block=True)
        chunk_boundary = chunk_boundaries[worker_index]
        # Store the chunk of results just received to the overall result list.
        results[chunk_boundary[0]:chunk_boundary[1]] = result_chunk

    # Join child processes (clean up zombies).
    for c in children:
        c.join()
    return results


def main():
    durations = []
    n_children = [1, 2, 4]
    for n in n_children:
        print "Crunching input with %s child(ren)." % n
        t0 = time.time()
        result = work(n)
        if result is None:
            continue
        duration = time.time() - t0
        print "Result computed by %s child process(es): %s" % (n, result)
        print "Duration: %.2f s" % duration
        durations.append(duration)
    normalized_durations = [durations[0]/d for d in durations]
    for n, normdur in zip(n_children, normalized_durations):
        print "%s-children speedup: %.2f" % (n, normdur)


if __name__ == '__main__':
    main()
