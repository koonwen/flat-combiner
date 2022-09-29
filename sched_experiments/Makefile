.PHONY:switch
switch:
	opam switch create . 5.0.0~alpha1 --deps-only

.PHONY=test_sc
test_sc: 
	dune runtest ./tests --no-buffer -j 1 --force

.PHONY=test_speed
test_speed :
	hyperfine --warmup=2 ../_build/default/lib/baseline.exe 
	hyperfine --warmup=2 ../_build/default/lib/hybrid.exe
	hyperfine --warmup=2 ../_build/default/lib/lifo.exe
	hyperfine --warmup=2 ../_build/default/lib/fifo.exe