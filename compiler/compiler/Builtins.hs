module Builtins where

import CodeGenHelper
import Control.Monad
import Control.Monad.Trans.State.Lazy as ST

data Type = TNumber | Fn [Type]

builtins =
    [ ("times"       , "k = arg2; next_args = create_args(1); args_set(next_args, 0, const_number(arg0->value * arg1->value)); return create_call(k, next_args);"
                     , Fn [TNumber, TNumber,      Fn [TNumber]])
    , ("plus"        , "k = arg2; next_args = create_args(1); args_set(next_args, 0, const_number(arg0->value + arg1->value)); return create_call(k, next_args);"
                     , Fn [TNumber, TNumber,      Fn [TNumber]])
    , ("minus"       , "k = arg2; next_args = create_args(1); args_set(next_args, 0, const_number(arg0->value - arg1->value)); return create_call(k, next_args);"
                     , Fn [TNumber, TNumber,      Fn [TNumber]])
    , ("sqrt"        , "k = arg1; next_args = create_args(1); args_set(next_args, 0, const_number(sqrt(arg0->value))); return create_call(k, next_args);"
                     , Fn [TNumber, Fn [TNumber]])
    , ("printNumber" , "k = arg1; next_args = create_args(0); printf(\"%f\\n\", arg0->value); return create_call(k, next_args);"
                     , Fn [TNumber, Fn []])
    , ("exit"        , "return NULL;"
                     , Fn [])
    , ("isZero?"     , "next_args = create_args(0); if (arg0->value == 0) { k = arg1; } else { k = arg2; }; return create_call(k, next_args);"
                     , Fn [TNumber, Fn [TNumber], Fn [TNumber]])
    , ("setTempo"    , "k = arg1; next_args = create_args(0); printf(\"set tempo: %f\\n\", arg0->value); return create_call(k, next_args);"
                     , Fn [TNumber, Fn []])
    , ("setBeat1"    , "k = arg1; next_args = create_args(0); printf(\"set beat 1: %f\\n\", arg0->value); return create_call(k, next_args);"
                     , Fn [TNumber, Fn []])
    ]

cType TNumber = "Number"
cType (Fn _) = "Closure"

outBuiltin :: String -> String -> Type -> ST.State AccumulatedCode String
outBuiltin name code (Fn argTypes) = do
    n <- nextCounter
    addGlobalName name ("create_closure(&fn_" ++ show n ++ ", env)")
    writeLine $ "Call fn_" ++ show n ++ "(Env parent_env, Args args) {"
    forM (zip [0..] argTypes) $ \(i, argType) -> do
        writeLine $ "    " ++ cType argType ++ " arg" ++ show i ++ " = (" ++ cType argType ++ ")args_get(args, " ++ show i ++ ");"
    writeLine $ "    Closure k;"
    writeLine $ "    Args next_args;"
    writeLine $ "    " ++ code
    writeLine $ "}"
    writeLine ""
    return $ "create_closure(&fn_" ++ show n ++ ", env)"
