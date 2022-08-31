(lang dune 3.3)

(name flat_combiner)

(generate_opam_files true)

(package
 (allow_empty)
 (name flat_combiner)
 (synopsis "A Flat combiner data structure library")
 (description
  "This library implements a FC data structure that can be applied over generic data structures. The library also contains an implementation of an FC queue that I use to benchmark against other types of concurrent queues to analyze the speedup of using a FC to minimze the bottle neck due to contention. The library also performs test cases on all the queues to ensure sequential consistency is preserved")
 (depends
  (ocaml-base-compiler
   (= 5.0.0~alpha1))
  dune
  alcotest
  benchmark
  lockfree))
