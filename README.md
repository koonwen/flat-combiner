# flat-combiner
This library implements a FC data structure that can be applied over generic data structures. The library also contains an implementation of an FC queue that I use to benchmark against other types of concurrent queues to analyze the speedup of using a FC to minimze the bottle neck due to contention. The library also performs test cases on all the queues to ensure sequential consistency is preserved"

## Motivation
The goal of this exercise is to understand how different scheduling policies affect the efficiency of the flat-combiner (which is the little sibling concept of the batch parallel data-structure). In proving that there is some reasonable speedup here, we can then moev to experimenting with different scheduling for p-batch data structures

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
- `make test` (Testing sequential consistency of queues)
- `make testq` (Run sequential consistency tests for smaller input for quick results
- `make speedtest` (Benchmarking enques between queues.)


