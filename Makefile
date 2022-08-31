.PHONY:build
build:
	dune build -w --terminal-persistence=clear-on-rebuild

.PHONY:switch
switch:
	opam switch create . 5.0.0~alpha1 --deps-only

.PHONY:test
test:
	dune exec -- test/main.exe test --bail "Implementation" "0-1"
.PHONY:testq
testq:
	dune exec -- test/main.exe -q

.PHONY:speedtest
speedtest:
	dune exec -- test/speedtest.exe

clean:
	dune clean