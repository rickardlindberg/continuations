#include <stdlib.h>
#include "runtime.h"

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
        return NULL;
    }
    for (i = 0; i < env->size; i++) {
        if (strcmp(env->pairs[i].key, key) == 0) {
            return env->pairs[i].value;
        }
    }
    return env_lookup(env->parent, key);
}
