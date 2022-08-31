.PHONY:build
build:
	dune build -w --terminal-persistence=clear-on-rebuild

.PHONY:create
create:
	opam switch create . --deps-only

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