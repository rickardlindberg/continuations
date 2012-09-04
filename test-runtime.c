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

int main() {
    test_env();
    return 0;
}
