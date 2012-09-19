all: compiler/conc-debugger \
     tests/test-runtime \
     examples/pythagoras-conc-build/pythagoras \
     examples/odd-even-conc-build/odd-even \
     examples/music-conc-build/music \
     examples/lists-conc-build/lists
	./tests/test-runtime
	./examples/pythagoras-conc-build/pythagoras
	./examples/odd-even-conc-build/odd-even
	./examples/music-conc-build/music
	./examples/lists-conc-build/lists

# EXAMPLES

COMPILER_DEPS = ./compiler/conc ./runtime/runtime.c ./runtime/runtime.h

examples/pythagoras-conc-build/pythagoras: examples/pythagoras.con $(COMPILER_DEPS)
	./compiler/conc $<

examples/odd-even-conc-build/odd-even: examples/odd-even.con $(COMPILER_DEPS)
	./compiler/conc $<

examples/music-conc-build/music: examples/music.con $(COMPILER_DEPS)
	./compiler/conc $<

examples/lists-conc-build/lists: examples/lists.con $(COMPILER_DEPS)
	./compiler/conc $<

# TEST FOR RUNTIME

tests/test-runtime: tests/test-runtime.c runtime/*.h runtime/*.c
	gcc -Wall -g -Iruntime -lm -o $@ $< runtime/*.c

# THE COMPILER ITSELF

HS_FILES := $(shell find compiler -name '*.hs')

compiler/conc: $(HS_FILES)
	cd compiler && ghc -o conc -Werror -fwarn-unused-imports --make Compiler.hs

compiler/conc-debugger: $(HS_FILES)
	cd compiler && ghc -o conc-debugger -Werror -fwarn-unused-imports --make Debugger.hs

# CLEAN ALL

.PHONY: clean
clean:
	rm -f compiler/conc
	rm -f compiler/conc-debugger
	rm -f tests/test-runtime
	rm -rf examples/*-conc-build
	find compiler -name '*.o' | xargs rm -f
	find compiler -name '*.hi' | xargs rm -f
