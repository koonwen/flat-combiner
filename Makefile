.PHONY = build
build:
	dune build -w --terminal-persistence=clear-on-rebuild

main:
	dune exec -- ./src/main.exe

main_info:
	dune exec -- ./src/main.exe -v

main_debug:
	dune exec -- ./src/main.exe -vv

# fcqt:
# 	dune exec -- ./src/fcq_threads.exe

# fcqd:
# 	dune exec -- ./src/fcq_domains.exe

test:
	dune test