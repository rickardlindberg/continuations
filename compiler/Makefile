EXAMPLES  = examples/pythagoras \
	    examples/odd-even \
	    examples/music \
	    examples/lists

RUNTIME_C = runtime/*.c
RUNTIME_H = runtime/*.h
RUNTIME   = $(RUNTIME_C) $(RUNTIME_H)

HS_FILES := $(shell find compiler -name '*.hs')

COMPILER  = compiler/Compiler

all: compiler/Debugger tests/test-runtime $(EXAMPLES)
	./tests/test-runtime
	./examples/pythagoras
	./examples/odd-even
	./examples/music
	./examples/lists

tests/test-runtime: tests/test-runtime.c $(RUNTIME)
	gcc -Wall -g -Iruntime -lm -o $@ $< $(RUNTIME_C)

examples/%: examples/%.c $(RUNTIME)
	gcc -Wall -g -Iruntime -lm -o $@ $< $(RUNTIME_C)

examples/%.c: examples/%.con $(COMPILER)
	cat $< | $(COMPILER) > $@

compiler/Compiler: $(HS_FILES)
	cd compiler && ghc -Werror -fwarn-unused-imports --make Compiler.hs

compiler/Debugger: $(HS_FILES)
	cd compiler && ghc -Werror -fwarn-unused-imports --make Debugger.hs

.PRECIOUS: examples/%.c

.PHONY: clean
clean:
	rm -f tests/test-runtime
	rm -f $(EXAMPLES)
	rm -f examples/*.c
	find compiler -name '*.o' | xargs rm -f
	find compiler -name '*.hi' | xargs rm -f
	rm -f compiler/Compiler
	rm -f compiler/Debugger
