# flat-combiner
This library implements a FC data structure that can be applied over generic data structures. The library also contains an implementation of an FC queue that I use to benchmark against other types of concurrent queues to analyze the speedup of using a FC to minimze the bottle neck due to contention. The library also performs test cases on all the queues to ensure sequential consistency is preserved"

## Motivation
The goal of this exercise is to understand how different scheduling policies affect the efficiency of the flat-combiner (the little sibling concept of the batch parallel data-structure). In proving that there is some reasonable speedup here, we can then move to experimenting with different scheduling for p-batch data structures

## Further Explanation
It is important to clarify our intuition behind why we could expect some speedup from different scheduling policies. To accomplish that, we need to explain why scheduling is used and where it excels in providing performance gains.

### Scheduling
In contrast to typical programs which execute synchronously (in order specified by the program), scheduling provides us a facility to execute our program asynchronously (the order of two executions do not need to be predertemined because they do not reliant on each other). 

In general, scheduling is used often in the context of overcoming the bottleneck of waiting for slow IO tasks (disk reads/writes, network latency). The key realisation here is that instead of waiting for the result of some IO, the processor go ahead and execute other tasks in the meantime whilst the data is being retrieved. (At the lowest level, this is possible due to the use of buffering and signals. E.g. A disk read happens independently of the processor where the reading of data into memory is performed by the hard-disk, when the read is completed, the hard disk sends a signal to the processor and interrupts it's computation to signal that the data is ready). Obviously, the tasks that can procede whilst waiting for IO must not rely on the result of the read. This is what is known as concurrent code. The structure of a program that permits interleaving of computations because they do not rely on each other's result.

However, designing a correct concurrent programs are extremely difficult. On top of that, it would be painful to also implement the scheduler wrapping your program. Hence, these things are often worked on separately. The scheduler is often implemented at the OS level and the language runtime. They provide a memory model and threading primatives to the algorithm developer who can then work on the program with these abstractions.

It is not always the case that a program can be split nicely into independent tasks. Different tasks are likely to access the same resource which bears the natural consequence that synchronisation is neccessary to prevent unexpected behaviour. As a result, techniques such as using locks, have been designed to address this problem. 

### Types of scheduling
Scheduling algorithms are typically organized into two categories. Preemptive and Cooperative scheduling. The former means that the scheduler can swap threads to run arbritrarily and whenever it's time slice runs out. The latter queues the program thread tasks to run to completion before running the next task.

- Preemptive scheduling
    - Completely Fair Scheduler (Linux scheduler)
    - Round Robin Scheduler
- Cooperative scheduling
    - Lwt
    - Async
    - Domainslib

### Multithreaded Single Core
It does not seem obvious at first why in a multi-threaded single core system, that synchronization is required. The way to think about this is to imagine a scheduler that is premptive (Arbritrarily decides when to swap out threads). This design leads to the awareness that one thread may be busy doing some work on shared memory and then swapped out before it is done. Another thread is prevented from accessing the same resource because the previous thread holds the lock.

### Multithreaded Multicore
The true advantage of multi-threading and parallelism come in here where we can take advantage of scheduling tasks to run on independent cores. This increases our complexity because shared resources are accessed in parallel which makes the threat of data races even more frequent. To circumvent the issue of interleaving accesses to shared memory along with program reordering, explicit memory fences give us some control. Concurrent data structures that allow parallel accesses to the same underlying structure must utilize this to ensure it's sequential consistency.

What happens then to shared memory accesses between parallel domains? How does a read occur to a similar memory location between two processors?

## Implementation
In our implementation, the following idea is as such
- Write the flat-combiner with Domainslib
- Write tests that require more tasks than domains
- Functorize the flat-combiner over the domainslib signature
- Implement different schedulers with the masters thesis
- Plot the different results varying the number of threads and the schedulers.
## Setup
To set up and install the dependencies for this library in a local switch
```
make switch
```
## Commands
(For flat_combiner directory)
- `make test` (Testing sequential consistency of queues)
- `make testq` (Run sequential consistency tests for smaller input for quick results
- `make speedtest` (Benchmarking enques between queues.)


# Notice
- Much of the code in the `sched_experiments` directory is not written by me, it is taken from https://github.com/bartoszmodelski/ebsl great work
- `flat_combiner` directory holds most of the FC implementations that uses the 5.0.0~alpha1 switch which is incompatible with the scheduler library that we rely on.
- `sched_experiments` uses the 4.12.0+domains switch and runs tests on the FC to see if we have performance gains from using different schedulers.