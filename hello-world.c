// This is a manually compiled version of the hello world program:
//
// let main = \() ->
//     print "Hello World" exit

#include <stdio.h>

// FRAME

struct frame {
    void (*fn)(struct frame *frame);
    void* args[10];
};

void* arg(struct frame *frame, int n) {
    return frame->args[n];
}

// BUILTIN

void builtin_exit(struct frame *frame) {
    frame->fn = 0;
}

void builtin_print(struct frame *frame) {
    printf((char*)arg(frame, 0));
    frame->fn = arg(frame, 1);
}

// COMPILED

void compiled_main(struct frame *frame) {
    char* s = "hello world\n";
    frame->args[0] = s;
    frame->args[1] = &builtin_exit;
    frame->fn = &builtin_print;
}

// LOOP

int main() {
    struct frame frame;
    frame.fn = &compiled_main;
    while (frame.fn != 0) {
        (*frame.fn)(&frame);
    }
    return 1;
}
