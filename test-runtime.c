#include <stdlib.h>
#include <stdio.h>
#include "runtime.h"

void test_env() {
    Env env1 = create_env(NULL);
    Env env2 = create_env(env1);
    char * value1 = "v1";
    char * value2 = "v2";
    char * value3 = "v3";

    env_insert(env1, "foo", value1);
    env_insert(env2, "bar", value2);
    env_insert(env2, "baz", value3);

    printf("%s\n", env_lookup(env2, "foo"));
    printf("%s\n", env_lookup(env2, "bar"));
    printf("%s\n", env_lookup(env2, "baz"));
    //printf("%s\n", env_lookup(env2, "foobar"));
}

void test_args() {
    Args args = create_args(2);
    char * value1 = "arg1";
    char * value2 = "arg2";

    args_set(args, 0, value1);
    args_set(args, 1, value2);

    printf("%s\n", args_get(args, 0));
    printf("%s\n", args_get(args, 1));
    //printf("%s\n", args_get(args, 2));
}

int main() {
    test_env();
    test_args();
    return 0;
}
