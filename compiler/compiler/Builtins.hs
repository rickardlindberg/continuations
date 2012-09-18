module Builtins where

import qualified Types.Types as T
import Types.Semantic

builtins =
    [ Builtin
        { name = "times"
        , code = "k = arg2; next_args = create_args(1); args_set(next_args, 0, const_number(arg0->value * arg1->value)); return create_call(k, next_args);"
        , fnType = T.Function [T.Number, T.Number, T.Function [T.Number]]
        , includes = []
        }
    , Builtin
        { name = "plus"
        , code = "k = arg2; next_args = create_args(1); args_set(next_args, 0, const_number(arg0->value + arg1->value)); return create_call(k, next_args);"
        , fnType = T.Function [T.Number, T.Number, T.Function [T.Number]]
        , includes = []
        }
    , Builtin
        { name = "minus"
        , code = "k = arg2; next_args = create_args(1); args_set(next_args, 0, const_number(arg0->value - arg1->value)); return create_call(k, next_args);"
        , fnType = T.Function [T.Number, T.Number, T.Function [T.Number]]
        , includes = []
        }
    , Builtin
        { name = "sqrt"
        , code = "k = arg1; next_args = create_args(1); args_set(next_args, 0, const_number(sqrt(arg0->value))); return create_call(k, next_args);"
        , fnType = T.Function [T.Number, T.Function [T.Number]]
        , includes = ["<math.h>"]
        }
    , Builtin
        { name = "printNumber"
        , code = "k = arg1; next_args = create_args(0); printf(\"%f\\n\", arg0->value); return create_call(k, next_args);"
        , fnType = T.Function [T.Number, T.Function []]
        , includes = ["<stdio.h>"]
        }
    , Builtin
        { name = "exit"
        , code = "return NULL;"
        , fnType = T.Function []
        , includes = []
        }
    , Builtin
        { name = "isZero?"
        , code = "next_args = create_args(0); if (arg0->value == 0) { k = arg1; } else { k = arg2; }; return create_call(k, next_args);"
        , fnType = T.Function [T.Number, T.Function [T.Number], T.Function [T.Number]]
        , includes = []
        }
    , Builtin
        { name = "setTempo"
        , code = "k = arg1; next_args = create_args(0); printf(\"set tempo: %f\\n\", arg0->value); return create_call(k, next_args);"
        , fnType = T.Function [T.Number, T.Function []]
        , includes = ["<stdio.h>"]
        }
    , Builtin
        { name = "setBeat1"
        , code = "k = arg1; next_args = create_args(0); printf(\"set beat 1: %f\\n\", arg0->value); return create_call(k, next_args);"
        , fnType = T.Function [T.Number, T.Function []]
        , includes = ["<stdio.h>"]
        }
    ]

addBuiltins :: Program -> Program
addBuiltins (Program lets) = Program (newLets ++ lets)
    where
        newLets = map (\(b) -> Let (name b) (Function (Fn (fnType b) b))) builtins
