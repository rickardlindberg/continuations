module Stages.Backends.CCommon where

import CodeGenHelper
import Control.Monad
import Control.Monad.Trans.State.Lazy as ST
import qualified Types.Types as T
import Types.Semantic

generateCode :: Bool -> Program -> String
generateCode asLibrary = runGenerator . (outProgram asLibrary)

outProgram :: Bool -> Program -> ST.State AccumulatedCode ()
outProgram asLibrary (Program lets) = do
    addInclude "\"runtime.h\""
    writeLine ""
    mapM_ outLet lets
    outMain asLibrary

outLet :: Let -> ST.State AccumulatedCode ()
outLet (Let name term) = outTerm term >>= addGlobalName name

outTerm :: Term -> ST.State AccumulatedCode String
outTerm (Identifier s)   = return $ "env_lookup(env, \"" ++ s ++ "\")"
outTerm (Number     n)   = return $ "const_number(" ++ show n ++ ")"
outTerm (Function   t b) = outFunction t b

outFunction :: T.Type -> FnBody -> ST.State AccumulatedCode String
outFunction _ (Lambda args terms) = do
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
outFunction (T.Function argTypes) (Builtin includes code) = do
    mapM_ addInclude includes
    n <- nextCounter
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
        cType T.Number = "Number"
        cType (T.Function _) = "Closure"

outMain :: Bool -> ST.State AccumulatedCode ()
outMain asLibrary = do
    state <- get
    if asLibrary
        then writeLine "void run() {"
        else writeLine "int main() {"
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
    when (not asLibrary) $ do
        writeLine ""
        writeLine "    return 0;"
    writeLine "}"
