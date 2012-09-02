// Manually compiled version of pythagoras program:
//
// let main = \() ->
//     pythagoras 2 4 \(res) ->
//         printNumber res exit
//
// let pythagoras = \(x, y, k) ->
//     * x x \(x2) ->
//         * y y \(y2) ->
//             + x2 y2 \(sum) ->
//                 sqrt sum k

#include <stdio.h>
#include <stdlib.h>

// ENV

struct env_pair {
    char * key;
    void * value;
};

struct env {
    int size;
    struct env_pair * entries;
    struct env * parent;
};

struct env * env_create(int size, struct env * parent) {
    struct env * env = (struct env *)malloc(sizeof(struct env));
    env->size = size;
    env->entries = (struct env_pair *)malloc(sizeof(struct env_pair) * size);
    env->parent = parent;
    return env;
}

void env_destroy(struct env * env) {
    free(env->entries);
    free(env);
}

void env_insert(struct env * env, int i, char * key, void * value) {
    struct env_pair pair;
    pair.key = key;
    pair.value = value;
    env->entries[i] = pair;
}

void * env_lookup(struct env * env, char * name) {
    int i;
    if (env == NULL) {
        return NULL;
    }
    for (i = 0; i < env->size; i++) {
        if (strcmp(env->entries[i].key, name) == 0) {
            return env->entries[i].value;
        }
    }
    return env_lookup(env->parent, name);
}

// FRAME

//struct fn {
//    void (*fn)(struct env *env);
//    struct env *env;
//};
//
//struct frame {
//    struct fn *fn;
//    void* args[10];
//};
//
//void* arg(struct frame *frame, int n) {
//    return frame->args[n];
//}
//
//// BUILTIN
//
//void builtin_exit(struct frame *frame) {
//    frame->fn = 0;
//}
//
//void builtin_print_number(struct frame *frame) {
//    printf("%d", (char*)arg(frame, 0));
//    frame->fn = arg(frame, 1);
//}
//
//void builtin_plus(struct frame *frame) {
//    int res = *(int*)arg(frame, 0) + *(int*)arg(frame, 1);
//    frame->fn = arg(frame, 2);
//    frame->args[0] = &res;
//}
//
//// COMPILED
//
//struct fn * compiled_main(struct env * env) {
//    struct fn * pythagoras = env_lookup(env, "pythagoras");
//    int n1 = 2;
//    int n2 = 4;
//    (struct fn *)(strucnt env *) anon = &compiled_anonymous_res;
//}
//
//struct fn * compiled_anonymous_res(struct env * env) {
//}
//
//struct fn * compiled_pythagoras(struct env * env) {
//}
//
//struct fn * compiled_anonymous_times_x(struct env * env) {
//}
//
//struct fn * compiled_anonymous_times_y(struct env * env) {
//}
//
//struct fn * compiled_anonymous_sum(struct env * env) {
//}

// LOOP

void test_env() {
    char * v1 = "value1";
    char * v2 = "value2";
    struct env * global_env = env_create(2, NULL);
    env_insert(global_env, 0, "key1", v1);
    env_insert(global_env, 1, "key2", v2);
    printf("RES = %s\n", (char *)env_lookup(global_env, "key1"));
    printf("RES = %s\n", (char *)env_lookup(global_env, "key2"));
    printf("RES = %s\n", (char *)env_lookup(global_env, "key3"));
}

int main() {
    test_env();

//    struct env env;
//
//    struct frame frame;
//    frame.fn = &compiled_main;
//    while (frame.fn != 0) {
//        (*frame.fn)(&frame);
//    }
    return 1;
}
