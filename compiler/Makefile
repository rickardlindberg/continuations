all: compiler/conc-debugger \
     tests/test-runtime \
     examples/pythagoras-conc-build-cpc/pythagoras \
     examples/odd-even-conc-build-cpc/odd-even \
     examples/music-conc-build-cpc/music \
     examples/music-conc-build-carduino/music \
     examples/lists-conc-build-cpc/lists
	./tests/test-runtime
	./examples/pythagoras-conc-build-cpc/pythagoras
	./examples/odd-even-conc-build-cpc/odd-even
	./examples/music-conc-build-cpc/music
	./examples/lists-conc-build-cpc/lists

# EXAMPLES

COMPILER_DEPS = ./compiler/conc ./runtime/runtime.c ./runtime/runtime.h

examples/pythagoras-conc-build-cpc/pythagoras: examples/pythagoras.con $(COMPILER_DEPS)
	./compiler/conc $< cpc

examples/odd-even-conc-build-cpc/odd-even: examples/odd-even.con $(COMPILER_DEPS)
	./compiler/conc $< cpc

examples/music-conc-build-cpc/music: examples/music.con $(COMPILER_DEPS)
	./compiler/conc $< cpc

examples/music-conc-build-carduino/music: examples/music.con $(COMPILER_DEPS)
	./compiler/conc $< carduino

examples/lists-conc-build-cpc/lists: examples/lists.con $(COMPILER_DEPS)
	./compiler/conc $< cpc

# TEST FOR RUNTIME

tests/test-runtime: tests/test-runtime.c runtime/*.h runtime/*.c
	gcc -Wall -g -Iruntime -lm -o $@ $< runtime/*.c

# THE COMPILER ITSELF

HS_FILES := $(shell find compiler -name '*.hs')

compiler/conc: $(HS_FILES)
	cd compiler && ghc -O1 -o conc -Werror -fwarn-unused-imports --make Compiler.hs

compiler/conc-debugger: $(HS_FILES)
	cd compiler && ghc -O1 -o conc-debugger -Werror -fwarn-unused-imports --make Debugger.hs

# CLEAN ALL

.PHONY: clean
clean:
	rm -f compiler/conc
	rm -f compiler/conc-debugger
	rm -f tests/test-runtime
	rm -rf examples/*-conc-build-*
	find compiler -name '*.o' | xargs rm -f
	find compiler -name '*.hi' | xargs rm -f
