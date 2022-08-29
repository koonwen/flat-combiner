.PHONY = build
build:
	dune build -w --terminal-persistence=clear-on-rebuild

main:
	dune exec -- ./src/main.exe

main_info:
	dune exec -- ./src/main.exe -v

main_debug:
	dune exec -- ./src/main.exe -vv

.PHONY:test
test:
	dune exec -- test/main.exe
.PHONY:testq
testq:
	dune exec -- test/main.exe -q

.PHONY:speedtest
speedtest:
	dune exec -- test/speedtest.exe

clean:
	dune clean