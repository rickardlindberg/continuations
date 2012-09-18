EXAMPLES  = examples/pythagoras \
	    examples/odd-even \
	    examples/music

RUNTIME_C = runtime/*.c
RUNTIME_H = runtime/*.h
RUNTIME   = $(RUNTIME_C) $(RUNTIME_H)

COMPILER  = compiler/Compiler

test: tests/test-runtime $(EXAMPLES)
	./tests/test-runtime
	./examples/pythagoras
	./examples/odd-even
	./examples/music

tests/test-runtime: tests/test-runtime.c $(RUNTIME)
	gcc -Wall -g -Iruntime -lm -o $@ $< $(RUNTIME_C)

examples/%: examples/%.c $(RUNTIME)
	gcc -Wall -g -Iruntime -lm -o $@ $< $(RUNTIME_C)

examples/%.c: examples/%.con $(COMPILER)
	cat $< | $(COMPILER) > $@

compiler/Compiler: compiler/*.hs compiler/Types/*.hs
	cd compiler && ghc -Werror -fwarn-unused-imports --make Compiler.hs

.PRECIOUS: examples/%.c

.PHONY: clean
clean:
	rm -f tests/test-runtime
	rm -f $(EXAMPLES)
	rm -f examples/*.c
	rm -f compiler/*.o
	rm -f compiler/*.hi
	rm -f compiler/Compiler
