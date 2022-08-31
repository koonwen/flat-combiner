# flat-combiner
This library implements a FC data structure that can be applied over generic data structures. The library also contains an implementation of an FC queue that I use to benchmark against other types of concurrent queues to analyze the speedup of using a FC to minimze the bottle neck due to contention. The library also performs test cases on all the queues to ensure sequential consistency is preserved"

## Setup
To set up and install the dependencies for this library in a local switch
```
make switch
```

## Commands
- `make test` (Testing sequential consistency of queues)
- `make testq` (Run sequential consistency tests for smaller input for quick results)
- `make speedtest` (Benchmarking enques between queues.)