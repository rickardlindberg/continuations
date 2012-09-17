#include <math.h>
#include <stdio.h>
#include "builtins.h"

Call builtin_times(Env env, Args args) {
    Number left = (Number)args_get(args, 0);
    Number right = (Number)args_get(args, 1);
    Closure k = (Closure)args_get(args, 2);
    Args next_args = create_args(1);
    args_set(next_args, 0, const_number(left->value * right->value));
    return create_call(k, next_args);
}

Call builtin_plus(Env env, Args args) {
    Number left = (Number)args_get(args, 0);
    Number right = (Number)args_get(args, 1);
    Closure k = (Closure)args_get(args, 2);
    Args next_args = create_args(1);
    args_set(next_args, 0, const_number(left->value + right->value));
    return create_call(k, next_args);
}

Call builtin_minus(Env env, Args args) {
    Number left = (Number)args_get(args, 0);
    Number right = (Number)args_get(args, 1);
    Closure k = (Closure)args_get(args, 2);
    Args next_args = create_args(1);
    args_set(next_args, 0, const_number(left->value - right->value));
    return create_call(k, next_args);
}

Call builtin_sqrt(Env env, Args args) {
    Number n = (Number)args_get(args, 0);
    Closure k = (Closure)args_get(args, 1);
    Args next_args = create_args(1);
    args_set(next_args, 0, const_number(sqrt(n->value)));
    return create_call(k, next_args);
}

Call builtin_printNumber(Env env, Args args) {
    Number n = (Number)args_get(args, 0);
    Closure k = (Closure)args_get(args, 1);
    printf("%f\n", n->value);
    return create_call(k, create_args(0));
}

Call builtin_isZeroP(Env env, Args args) {
    Number n = (Number)args_get(args, 0);
    Closure trueFn = (Closure)args_get(args, 1);
    Closure falseFn = (Closure)args_get(args, 2);
    if (n->value == 0) {
        return create_call(trueFn, create_args(0));
    } else {
        return create_call(falseFn, create_args(0));
    }
}

Call builtin_setTempo(Env env, Args args) {
    Number n = (Number)args_get(args, 0);
    Closure k = (Closure)args_get(args, 1);
    printf("setting tempo: %f\n", n->value);
    return create_call(k, create_args(0));
}

Call builtin_setBeat1(Env env, Args args) {
    Number n = (Number)args_get(args, 0);
    Closure k = (Closure)args_get(args, 1);
    printf("setting beat 1: %f\n", n->value);
    return create_call(k, create_args(0));
}

Call builtin_exit(Env env, Args args) {
    return NULL;
}
