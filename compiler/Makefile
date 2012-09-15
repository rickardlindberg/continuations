test: examples/pythagoras tests/test-runtime
	./tests/test-runtime
	./examples/pythagoras

tests/test-runtime: tests/test-runtime.c runtime/runtime.c runtime/runtime.h
	gcc -g -Iruntime -lm -o tests/test-runtime tests/test-runtime.c runtime/runtime.c

examples/pythagoras: examples/pythagoras.c runtime/runtime.c runtime/runtime.h
	gcc -g -Iruntime -lm -o examples/pythagoras examples/pythagoras.c runtime/runtime.c

examples/pythagoras.c: examples/pythagoras.con compiler/Compiler
	cat examples/pythagoras.con | compiler/Compiler > examples/pythagoras.c

compiler/Compiler: compiler/*.hs
	cd compiler && ghc --make Compiler.hs

.PHONY: clean
clean:
	rm -f tests/test-runtime
	rm -f examples/pythagoras
	rm -f examples/pythagoras.c
	rm -f compiler/*.o
	rm -f compiler/*.hi
	rm -f compiler/Compiler
