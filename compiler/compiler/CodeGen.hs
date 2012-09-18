module CodeGen where

import Builtins
import CodeGenHelper
import Control.Monad
import Control.Monad.Trans.State.Lazy as ST
import Data

generateCode :: Program -> String
generateCode = runGenerator . outProgram

outProgram :: Program -> ST.State AccumulatedCode ()
outProgram (Program lets) = do
    addInclude "\"runtime.h\""
    writeLine ""
    mapM_ outLet lets
    outMain

outBuiltin :: Builtin -> ST.State AccumulatedCode String
outBuiltin (Builtin name code (Fn argTypes) includes) = do
    mapM_ addInclude includes
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
    where
        cType TNumber = "Number"
        cType (Fn _) = "Closure"

outLet :: Let -> ST.State AccumulatedCode ()
outLet (Let name term) = outTerm term >>= addGlobalName name

outTerm :: Term -> ST.State AccumulatedCode String
outTerm (Identifier s)          = return $ "env_lookup(env, \"" ++ s ++ "\")"
outTerm (Number     n)          = return $ "const_number(" ++ show n ++ ")"
outTerm (Lambda     args terms) = do
    n <- nextCounter
    terms <- mapM outTerm terms
    writeLine $ "Call fn_" ++ show n ++ "(Env parent_env, Args args) {"
    writeLine $ "    Args next_args;"
    writeLine $ "    Closure closure;"
    writeLine $ "    Env env;"
    writeLine $ ""
    writeLine $ "    env = create_env(parent_env);"
    forM (zip [0..] args) $ \(i, arg) -> do
        writeLine $ "    env_insert(env, \"" ++ arg ++ "\", args_get(args, " ++ show i ++ "));"
    writeLine $ ""
    writeLine $ "    closure = (Closure)" ++ head terms ++ ";"
    writeLine $ ""
    writeLine $ "    next_args = create_args(" ++ show (length (tail terms)) ++ ");"
    forM (zip [0..] (tail terms)) $ \(i, term) -> do
        writeLine $ "    args_set(next_args, " ++ show i ++ ", " ++ term ++ ");"
    writeLine $ ""
    writeLine $ "    free_ref_countable(env);"
    writeLine $ ""
    writeLine $ "    return create_call(closure, next_args);"
    writeLine $ "}"
    writeLine ""
    return $ "create_closure(&fn_" ++ show n ++ ", env)"
outTerm (BuiltinFn builtin) = outBuiltin builtin

outMain :: ST.State AccumulatedCode ()
outMain = do
    state <- get
    writeLine "int main() {"
    writeLine "    Env env;"
    writeLine "    Call call, next_call;"
    writeLine ""
    writeLine "    env = create_env(NULL);"
    forM (globalNames state) $ \(name, code) -> do
        writeLine $ "    env_insert(env, \"" ++ name ++ "\", " ++ code ++ ");"
    writeLine ""
    writeLine "    call = create_call((Closure)env_lookup(env, \"main\"), create_args(0));"
    writeLine ""
    writeLine "    while (call != NULL) {"
    writeLine "        next_call = call->closure->fn_spec(call->closure->env, call->args);"
    writeLine "        free_ref_countable(call);"
    writeLine "        call = next_call;"
    writeLine "    }"
    writeLine ""
    writeLine "    free_ref_countable(env);"
    writeLine ""
    writeLine "    return 0;"
    writeLine "}"
