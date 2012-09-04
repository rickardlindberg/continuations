Env create_env(Env parent) {
    Env env = (Env)malloc(sizeof(struct env));
    env->size = 0;
    env->pairs = (struct pair *)malloc(env->size * sizeof struct pair);
}

void env_insert(Env env, char * key, void * value) {
    struct pair;
    pair.key = key;
    pair.value = value;
    env->size++;
    env->pairs = (struct pair *)realloc(env->size * sizeof struct pair);
    env->pairs[env->size - 1] = pair;
}
