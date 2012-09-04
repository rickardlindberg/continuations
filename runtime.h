struct pair {
    char * key;
    void * value;
};

struct env {
    int size;
    struct pair * pairs;
    struct env * parent;
};

struct args {
    int size;
    void ** args;
};

struct binding {
};

struct call {
};

typedef struct args * Args;
typedef struct binding * Binding;
typedef struct call * Call;
typedef struct env * Env;
typedef Call (*FnSpec)(Env env, Args args);

// Env

Env create_env(Env parent);
void * env_lookup(Env env, char * key);

// Args

Args create_args(int size);
void * args_get(Args args, int i);
void args_set(Args args, int i, void * value);

// Binding

Binding create_binding(FnSpec fn_spec, Env env);

// Call

Call create_call(Binding binding, Args args);

// Constants

void * const_int(int i);
