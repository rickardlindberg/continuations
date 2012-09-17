#ifndef BUILTINS_H
#define BUILTINS_H

#include "runtime.h"

Call builtin_times(Env env, Args args);
Call builtin_plus(Env env, Args args);
Call builtin_minus(Env env, Args args);
Call builtin_sqrt(Env env, Args args);
Call builtin_printNumber(Env env, Args args);
Call builtin_exit(Env env, Args args);
Call builtin_isZeroP(Env env, Args args);
Call builtin_setTempo(Env env, Args args);
Call builtin_setBeat1(Env env, Args args);

#endif
