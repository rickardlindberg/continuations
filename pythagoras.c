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

typedef struct env_pair * EnvPair;

struct env {
    int size;
    EnvPair entries;
    struct env * parent;
};

typedef struct env * Env;

Env env_create(int size, Env parent) {
    Env env = (Env)malloc(sizeof(struct env));
    env->size = size;
    env->entries = (EnvPair)malloc(sizeof(struct env_pair) * size);
    env->parent = parent;
    return env;
}

void env_destroy(Env env) {
    free(env->entries);
    free(env);
}

void env_insert(Env env, int i, char * key, void * value) {
    struct env_pair pair;
    pair.key = key;
    pair.value = value;
    env->entries[i] = pair;
}

void * env_lookup(Env env, char * name) {
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

// ARGUMENTS

struct args {
    int size;
    void ** args;
};

typedef struct args * Args;

Args args_create(int size) {
    Args args = (Args)malloc(sizeof(struct args));
    args->size = size;
    args->args = (void **)malloc(sizeof(void *) * size);
    return args;
}

void args_destroy(Args args) {
    free(args->args);
    free(args);
}

void args_insert(Args args, int i, void * value) {
    args->args[i] = value;
}

void * args_get(Args args, int i) {
    return args->args[i];
}

// FUNCTION STUFF

typedef struct call * (*FnPtr)(Env env, Args args);

struct fn {
    FnPtr fn_spec;
    Env env;
};

typedef struct fn * Fn;

Fn fn_create(FnPtr fn_spec, Env env) {
    Fn fn = (struct fn *)malloc(sizeof(struct fn));
    fn->fn_spec = fn_spec;
    fn->env = env;
    return fn;
};

void fn_destroy(Fn fn) {
    free(fn);
}

struct call {
    Fn fn;
    Args args;
};

typedef struct call * Call;

Call call_create(Fn fn, Args args) {
    Call call = (struct call *)malloc(sizeof(struct call));
    call->fn = fn;
    call->args = args;
    return call;
}

void call_destroy(Call call) {
    free(call);
}

// BUILTIN

Call builtin_exit(Env env, Args args) {
    return NULL;
}

Call builtin_print(Env env, Args args) {
    printf((char*)args_get(args, 0));
    return call_create(fn_create(args_get(args, 1), env), args_create(0));
}

// COMPILED

Call compiled_main(Env env, Args in_args) {
    char* s = "hello world\n";
    Args args = args_create(2);
    // prepare args
    args_insert(args, 0, s);
    args_insert(args, 1, env_lookup(env, "exit"));
    // prepare call
    return call_create(fn_create(env_lookup(env, "print"), env), args);
}

// TESTS

void test_env() {
    char * v1 = "value1";
    char * v2 = "value2";
    Env env = env_create(2, NULL);
    printf("Testing env\n");
    env_insert(env, 0, "key1", v1);
    env_insert(env, 1, "key2", v2);
    printf("RES = %s\n", (char *)env_lookup(env, "key1"));
    printf("RES = %s\n", (char *)env_lookup(env, "key2"));
    printf("RES = %s\n", (char *)env_lookup(env, "key3"));
    env_destroy(env);
}

void test_args() {
    char * a1 = "arg 1";
    char * a2 = "arg 2";
    Args args = args_create(2);
    printf("Testing args\n");
    args_insert(args, 0, a1);
    args_insert(args, 1, a2);
    printf("RES = %s\n", (char *)args_get(args, 0));
    printf("RES = %s\n", (char *)args_get(args, 1));
    printf("RES = %s\n", (char *)args_get(args, 2));
    args_destroy(args);
}

// LOOP

int main() {
    Env global_env = env_create(2, NULL);
    env_insert(global_env, 0, "exit", &builtin_exit);
    env_insert(global_env, 1, "print", &builtin_print);

    Fn fn = fn_create(&compiled_main, global_env);
    Args args = args_create(0);
    Call call = call_create(fn, args);

    test_env();
    test_args();

    while (1) {
        if (call == NULL) {
            return 1;
        }
        call = (*call->fn->fn_spec)(call->fn->env, call->args);
    }
}
