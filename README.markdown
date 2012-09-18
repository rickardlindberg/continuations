The purpose of this repository is to learn about continuation passing style,
and specifically how one can implement such a language in C.

# Step 1

The first thing I did was to get a feel for how continuation passing style
works. I did this by experimenting with it in Haskell. (`Learning.hs`)

# Step 2

Next I imagined there was a compiler from a continuation passing style language
to C. I tried to imagine what C code the compiler would generate for a simple
hello world program. (`hello-world.c`)

# Step 3

The hello world program was quite simple and there was no need for an
environment. I tried the same exercise as in the previous step, but with a more
complex program: a program to calculate the length of one side of a triangle.
(`pythagoras.c`)

# Step 4

I didn't manage to to figure out what the compiled version of the pythagoras
program would look like, so I decided to start writing the compiler instead. Of
course I didn't know exactly what code the compiler should generate, but it
gave me a different way to think about the problem.

I managed to get the compiler working, but I did not take care of garbage
collection. If you run a compiled program long enough, it will run out of
memory. Later, I fixed that by implementing reference counting.

(`Compiler.hs`, `runtime.h`, `runtime.c`, `pythagoras.con`, `test-runtime.c`)

# Step 5

I wanted to continue experimenting with the compiler, so I copied it over to
`/compiler`.

Additional ideas implemented in `/compiler`:

* Generate implementation of built in functions
* Syntactic sugar for let expressions
* TODO: Perform type checking
