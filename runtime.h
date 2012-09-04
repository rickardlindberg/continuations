struct pair {
    char * key;
    void * value;
};

struct env {
    int size;
    struct pair * pairs;
    struct env * parent;
};

typedef struct env * Env;

Env create_env(Env parent);
void * env_lookup(Env env, char * key);
