// Env

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

// Args

struct args {
    int size;
    void ** args;
};

typedef struct args * Args;

Args create_args(int size);
void * args_get(Args args, int i);
void args_set(Args args, int i, void * value);
