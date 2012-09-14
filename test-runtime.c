#include <stdlib.h>
#include <stdio.h>
#include "runtime.h"

void test_env() {
    Env env1 = create_env(NULL);
    Env env2 = create_env(env1);
    Number value1 = const_number(1);
    Number value2 = const_number(2);
    Number value3 = const_number(3);

    env_insert(env1, "foo", value1);
    env_insert(env2, "bar", value2);
    env_insert(env2, "baz", value3);

    printf("%f\n", ((Number)env_lookup(env2, "foo"))->value);
    printf("%f\n", ((Number)env_lookup(env2, "bar"))->value);
    printf("%f\n", ((Number)env_lookup(env2, "baz"))->value);
    //printf("%s\n", env_lookup(env2, "foobar"));
}

void test_args() {
    Args args = create_args(2);
    Number value1 = const_number(1);
    Number value2 = const_number(2);

    args_set(args, 0, value1);
    args_set(args, 1, value2);

    printf("%f\n", ((Number)args_get(args, 0))->value);
    printf("%f\n", ((Number)args_get(args, 1))->value);
    //printf("%s\n", args_get(args, 2));
}

int main() {
    test_env();
    test_args();
    return 0;
}
