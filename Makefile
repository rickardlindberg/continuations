all: pythagoras hello-world test-runtime compiler

pythagoras: pythagoras.c
	gcc -o pythagoras pythagoras.c

hello-world: hello-world.c
	gcc -o hello-world hello-world.c

compiler: pythagoras.con Compiler.hs runtime.h runtime.c
	cat pythagoras.con | runhaskell Compiler.hs > compiler.c && gcc -o compiler compiler.c runtime.c

test-runtime: test-runtime.c runtime.c
	gcc -o test-runtime test-runtime.c runtime.c
