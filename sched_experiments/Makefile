.PHONY:switch
switch:
	opam switch create . 5.0.0~alpha1 --deps-only

.PHONY=test_sc
test_sc: 
	dune runtest ./tests --no-buffer -j 1 --force

.PHONY=test_speed
test_speed :
	hyperfine --warmup=2 ../_build/default/sched_experiments/baseline.exe 
	hyperfine --warmup=2 ../_build/default/sched_experiments/hybrid.exe
	hyperfine --warmup=2 ../_build/default/sched_experiments/lifo.exe
	hyperfine --warmup=2 ../_build/default/sched_experiments/fifo.exe

.PHONY=testp
testp :
	dune build
	hyperfine --warmup=2 ../_build/default/sched_experiments/tests/fifo.exe
