test: examples/pythagoras tests/test-runtime
	./tests/test-runtime
	./examples/pythagoras

tests/test-runtime: tests/test-runtime.c runtime/runtime.c runtime/runtime.h
	gcc -g -Iruntime -lm -o tests/test-runtime tests/test-runtime.c runtime/runtime.c

examples/pythagoras: examples/pythagoras.c runtime/runtime.c runtime/runtime.h
	gcc -g -Iruntime -lm -o examples/pythagoras examples/pythagoras.c runtime/runtime.c

examples/pythagoras.c: examples/pythagoras.con compiler/Compiler.hs
	cat examples/pythagoras.con | runhaskell compiler/Compiler.hs > examples/pythagoras.c

.PHONY: clean
clean:
	rm -f tests/test-runtime
	rm -f examples/pythagoras
	rm -f examples/pythagoras.c
