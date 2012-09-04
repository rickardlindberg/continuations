all: pythagoras hello-world compiled test-runtime

pythagoras: pythagoras.c
	gcc -o pythagoras pythagoras.c

hello-world: hello-world.c
	gcc -o hello-world hello-world.c

compiled: pythagoras.con Compiler.hs runtime.h runtime.c
	cat pythagoras.con | runhaskell Compiler.hs > compiled.c && gcc -o compiled compiled.c runtime.c

test-runtime: test-runtime.c runtime.c
	gcc -o test-runtime test-runtime.c runtime.c
