module CodeGen where

import CodeGenHelper
import Control.Monad
import Control.Monad.Trans.State.Lazy as ST
import Data
import Data.String.Utils

generateCode :: Program -> String
generateCode = runGenerator . outProgram

outProgram :: Program -> ST.State AccumulatedCode ()
outProgram (Program fns) = do
    writeLine "#include <stdio.h>"
    writeLine "#include <stdlib.h>"
    writeLine "#include \"runtime.h\""
    writeLine ""
    outList fns outLet (writeLine "")
    writeLine ""
    outMain

outLet :: Let -> ST.State AccumulatedCode ()
outLet (Let name term) = do
    code <- outTerm term
    addGlobalName name code
    return ()

outTerm :: Term -> ST.State AccumulatedCode String
outTerm (Identifier s) = return $ "env_lookup(env, \"" ++ s ++ "\")"
outTerm (Number     n) = return $ "const_number(" ++ show n ++ ")"
outTerm (Lambda     args terms) = do
    n <- outLambda args terms
    writeLine ""
    return $ "create_closure(&fn_" ++ show n ++ ", env)"

outLambda :: [String] -> [Term] -> ST.State AccumulatedCode Int
outLambda args terms = do
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
    return n

outMain :: ST.State AccumulatedCode ()
outMain = do
    writeLine "int main() {"
    writeLine "    Env env;"
    writeLine "    Call call, next_call;"
    writeLine ""
    writeLine "    env = create_env(NULL);"
    s <- get
    let gn = globalNames s
    let bn = ["times", "plus", "minus", "sqrt", "printNumber", "exit", "isZero?"]
    mapM_ (\(name, n) -> writeLine $ "    env_insert(env, \"" ++ name ++ "\", " ++ n ++ ");") gn
    mapM_ (\name -> writeLine      $ "    env_insert(env, \"" ++ name ++ "\", create_closure(&builtin_" ++ (replace "?" "P" name) ++ ", env));") bn
    writeLine ""
    writeLine "    call = create_call(env_lookup(env, \"main\"), create_args(0));"
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
