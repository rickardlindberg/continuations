struct pair {
    char * key;
    void * value;
};

struct env {
    int size;
    struct pair * pairs;
};

typedef struct env * Env;

Env create_env(Env parent);
