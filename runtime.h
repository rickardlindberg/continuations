typedef struct env * Env;
typedef struct args * Args;
typedef struct binding * Binding;
typedef struct call * Call;
typedef Call (*FnSpec)(Env env, Args args);

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

Env create_env(Env parent);
void * env_lookup(Env env, char * key);

// Args

struct args {
    int size;
    void ** args;
};

Args create_args(int size);
void * args_get(Args args, int i);
void args_set(Args args, int i, void * value);

// Binding

struct binding {
    FnSpec fn_spec;
    Env env;
};

Binding create_binding(FnSpec fn_spec, Env env);

// Call

struct call {
    Binding binding;
    Args args;
};

Call create_call(Binding binding, Args args);

// Constants

void * const_number(double i);

// Built-ins

Call builtin_times(Env env, Args args);
Call builtin_plus(Env env, Args args);
Call builtin_sqrt(Env env, Args args);
Call builtin_printNumber(Env env, Args args);
Call builtin_exit(Env env, Args args);
