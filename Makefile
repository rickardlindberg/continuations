test: pythagoras hello-world compiled test-runtime
	#./pythagoras
	#./hello-world
	./compiled
	./test-runtime

pythagoras: pythagoras.c
	gcc -o pythagoras pythagoras.c

hello-world: hello-world.c
	gcc -o hello-world hello-world.c

compiled: compiled.c runtime.h runtime.c
	gcc -lm -g -o compiled compiled.c runtime.c

compiled.c: pythagoras.con Compiler.hs
	cat pythagoras.con | runhaskell Compiler.hs > compiled.c

test-runtime: test-runtime.c runtime.c
	gcc -lm -o test-runtime test-runtime.c runtime.c

.PHONY: clean
clean:
	rm -f pythagoras
	rm -f hello-world
	rm -f compiled
	rm -f compiled.c
	rm -f test-runtime
