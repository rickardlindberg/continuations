#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "runtime.h"

// Env

Env create_env(Env parent) {
    Env env = (Env)malloc(sizeof(struct env));
    env->size = 0;
    env->pairs = (struct pair *)malloc(env->size * sizeof(struct pair));
    env->parent = parent;
}

void env_insert(Env env, char * key, void * value) {
    struct pair pair;
    pair.key = key;
    pair.value = value;
    env->size++;
    env->pairs = (struct pair *)realloc(env->pairs, env->size * sizeof(struct pair));
    env->pairs[env->size - 1] = pair;
}

void * env_lookup(Env env, char * key) {
    int i;
    if (env == NULL) {
        printf("Did not find key '%s'\n", key);
        return NULL;
    }
    for (i = 0; i < env->size; i++) {
        if (strcmp(env->pairs[i].key, key) == 0) {
            return env->pairs[i].value;
        }
    }
    return env_lookup(env->parent, key);
}

// Args

Args create_args(int size) {
    Args args = (Args)malloc(sizeof(struct args));
    args->size = size;
    args->args = (void **)malloc(args->size * sizeof(void *));
    return args;
}

void * args_get(Args args, int i) {
    return args->args[i];
}

void args_set(Args args, int i, void * value) {
    args->args[i] = value;
}

// Binding

Binding create_binding(FnSpec fn_spec, Env env) {
    Binding binding = (Binding)malloc(sizeof(struct binding));
    binding->fn_spec = fn_spec;
    binding->env = env;
    return binding;
}

// Call

Call create_call(Binding binding, Args args) {
    Call call = (Call)malloc(sizeof(struct call));
    call->binding = binding;
    call->args = args;
    return call;
}

// Constants

void * const_number(double i) {
    double * value = (double *)malloc(sizeof(double));
    *value = i;
    return (void *)value;
}

// Built-ins

Call builtin_times(Env env, Args args) {
    double * left = (double *)args_get(args, 0);
    double * right = (double *)args_get(args, 1);
    Binding k = (Binding)args_get(args, 2);
    Args next_args = create_args(1);
    args_set(next_args, 0, const_number((*left) * (*right)));
    return create_call(k, next_args);
}

Call builtin_plus(Env env, Args args) {
    double * left = (double *)args_get(args, 0);
    double * right = (double *)args_get(args, 1);
    Binding k = (Binding)args_get(args, 2);
    Args next_args = create_args(1);
    args_set(next_args, 0, const_number((*left) + (*right)));
    return create_call(k, next_args);
}

Call builtin_sqrt(Env env, Args args) {
    double * n = (double *)args_get(args, 0);
    Binding k = (Binding)args_get(args, 1);
    Args next_args = create_args(1);
    args_set(next_args, 0, const_number(sqrt(*n)));
    return create_call(k, next_args);
}

Call builtin_printNumber(Env env, Args args) {
    double * n = (double *)args_get(args, 0);
    Binding k = (Binding)args_get(args, 1);
    printf("%f\n", *n);
    return create_call(k, create_args(0));
}

Call builtin_exit(Env env, Args args) {
    return NULL;
}
