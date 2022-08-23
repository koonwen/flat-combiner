.PHONY = build
build:
	dune build -w --terminal-persistence=clear-on-rebuild

exec:
	dune exec -- ./src/main.exe

info:
	dune exec -- ./src/main.exe -v

debug:
	dune exec -- ./src/main.exe -vv