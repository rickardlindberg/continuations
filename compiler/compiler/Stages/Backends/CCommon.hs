module Stages.Backends.CCommon where

import CodeGenHelper
import Control.Monad
import Control.Monad.Trans.State.Lazy as ST
import Data.Maybe
import qualified Data.Map as M
import qualified Types.Builtin as B
import qualified Types.Types as T
import Types.Semantic

data CCommonBuiltin = CCommonBuiltin
    { type_ :: T.Type
    , gen   :: ST.State AccumulatedCode ()
    }

commonCBuiltins :: M.Map String CCommonBuiltin
commonCBuiltins = M.fromList $
    [ ("times", CCommonBuiltin (T.Function [T.Number, T.Number, T.Function [T.Number]]) $ do
        writeLine "k = arg2;"
        writeLine "next_args = create_args(1);"
        writeLine "args_set(next_args, 0, const_number(arg0->value * arg1->value));"
        writeLine "return create_call(k, next_args);"
    )
    , ("plus", CCommonBuiltin (T.Function [T.Number, T.Number, T.Function [T.Number]]) $ do
        writeLine "k = arg2;"
        writeLine "next_args = create_args(1);"
        writeLine "args_set(next_args, 0, const_number(arg0->value + arg1->value));"
        writeLine "return create_call(k, next_args);"
    )
    , ("minus", CCommonBuiltin (T.Function [T.Number, T.Number, T.Function [T.Number]]) $ do
        writeLine "k = arg2;"
        writeLine "next_args = create_args(1);"
        writeLine "args_set(next_args, 0, const_number(arg0->value - arg1->value));"
        writeLine "return create_call(k, next_args);"
    )
    , ("sqrt", CCommonBuiltin (T.Function [T.Number, T.Function [T.Number]]) $ do
        addInclude "<math.h>"
        writeLine "k = arg1;"
        writeLine "next_args = create_args(1);"
        writeLine "args_set(next_args, 0, const_number(sqrt(arg0->value)));"
        writeLine "return create_call(k, next_args);"
    )
    , ("printNumber", CCommonBuiltin (T.Function [T.Number, T.Function []]) $ do
        addInclude "<stdio.h>"
        writeLine "k = arg1;"
        writeLine "next_args = create_args(0);"
        writeLine "printf(\"%f\\n\", arg0->value);"
        writeLine "return create_call(k, next_args);"
    )
    , ("exit", CCommonBuiltin (T.Function []) $ do
        writeLine "return NULL;"
    )
    , ("isZero?", CCommonBuiltin (T.Function [T.Number, T.Function [T.Number], T.Function [T.Number]]) $ do
        writeLine "next_args = create_args(0);"
        writeLine "if (arg0->value == 0) {"
        writeLine "    k = arg1;"
        writeLine "} else {"
        writeLine "    k = arg2;"
        writeLine "}"
        writeLine "return create_call(k, next_args);"
    )
    , ("setTempo", CCommonBuiltin (T.Function [T.Number, T.Function []]) $ do
        addInclude "<stdio.h>"
        writeLine "k = arg1;"
        writeLine "next_args = create_args(0);"
        writeLine "printf(\"set tempo: %f\\n\", arg0->value);"
        writeLine "return create_call(k, next_args);"
    )
    , ("setBeat1", CCommonBuiltin (T.Function [T.Number, T.Function []]) $ do
        addInclude "<stdio.h>"
        writeLine "k = arg1;"
        writeLine "next_args = create_args(0);"
        writeLine "printf(\"set beat 1: %f\\n\", arg0->value);"
        writeLine "return create_call(k, next_args);"
    )
    , ("setBeat2", CCommonBuiltin (T.Function [T.Number, T.Number, T.Function []]) $ do
        addInclude "<stdio.h>"
        writeLine "k = arg2;"
        writeLine "next_args = create_args(0);"
        writeLine "printf(\"set beat 2: %f, %f\\n\", arg0->value, arg1->value);"
        writeLine "return create_call(k, next_args);"
    )
    , ("setBeat3", CCommonBuiltin (T.Function [T.Number, T.Number, T.Number, T.Function []]) $ do
        addInclude "<stdio.h>"
        writeLine "k = arg3;"
        writeLine "next_args = create_args(0);"
        writeLine "printf(\"set beat 3: %f, %f, %f\\n\", arg0->value, arg1->value, arg2->value);"
        writeLine "return create_call(k, next_args);"
    )
    ]

extendBuiltins :: M.Map String CCommonBuiltin -> M.Map String CCommonBuiltin -> M.Map String CCommonBuiltin
extendBuiltins original extension = M.union extension original

exportBuiltins :: M.Map String CCommonBuiltin -> [B.Builtin]
exportBuiltins = M.foldrWithKey toGeneralBuiltin []
    where
        toGeneralBuiltin name (CCommonBuiltin type_ _) rest = B.Builtin name type_ : rest

data Options = Options
    { isLibrary :: Bool
    , builtinsX :: M.Map String CCommonBuiltin
    }

generateCode :: Options -> Program -> String
generateCode options = runGenerator . (outProgram options)

outProgram :: Options -> Program -> ST.State AccumulatedCode ()
outProgram options (Program lets) = do
    addInclude "\"runtime.h\""
    writeLine ""
    mapM_ (outLet options) lets
    outMain options

outLet :: Options -> Let -> ST.State AccumulatedCode ()
outLet options (Let name term) = outTerm options term >>= addGlobalName name

outTerm :: Options -> Term -> ST.State AccumulatedCode String
outTerm options (Identifier s)   = return $ "env_lookup(env, \"" ++ s ++ "\")"
outTerm options (Number     n)   = return $ "const_number(" ++ show n ++ ")"
outTerm options (Function   t b) = outFunction options t b

outFunction :: Options -> T.Type -> FnBody -> ST.State AccumulatedCode String
outFunction options _ (Lambda args terms) = do
    n <- nextCounter
    terms <- mapM (outTerm options) terms
    writeLine $ "Call fn_" ++ show n ++ "(Env parent_env, Args args) {"
    indented $ do
        writeLine $ "Args next_args;"
        writeLine $ "Closure closure;"
        writeLine $ "Env env;"
        writeLine $ ""
        writeLine $ "env = create_env(parent_env);"
        forM (zip [0..] args) $ \(i, arg) -> do
            writeLine $ "env_insert(env, \"" ++ arg ++ "\", args_get(args, " ++ show i ++ "));"
        writeLine $ ""
        writeLine $ "closure = (Closure)" ++ head terms ++ ";"
        writeLine $ ""
        writeLine $ "next_args = create_args(" ++ show (length (tail terms)) ++ ");"
        forM (zip [0..] (tail terms)) $ \(i, term) -> do
            writeLine $ "args_set(next_args, " ++ show i ++ ", " ++ term ++ ");"
        writeLine $ ""
        writeLine $ "free_ref_countable(env);"
        writeLine $ ""
        writeLine $ "return create_call(closure, next_args);"
    writeLine $ "}"
    writeLine ""
    return $ "create_closure(&fn_" ++ show n ++ ", env)"
outFunction options (T.Function argTypes) (Builtin name) = do
    n <- nextCounter
    writeLine $ "Call fn_" ++ show n ++ "(Env parent_env, Args args) {"
    indented $ do
        forM (zip [0..] argTypes) $ \(i, argType) -> do
            writeLine $ cType argType ++ " arg" ++ show i ++ " = (" ++ cType argType ++ ")args_get(args, " ++ show i ++ ");"
        writeLine $ "Closure k;"
        writeLine $ "Args next_args;"
        writeBuiltinCode name (builtinsX options)
    writeLine $ "}"
    writeLine ""
    return $ "create_closure(&fn_" ++ show n ++ ", env)"

outMain :: Options -> ST.State AccumulatedCode ()
outMain options = do
    state <- get
    if isLibrary options
        then writeLine "void run() {"
        else writeLine "int main() {"
    indented $ do
        writeLine "Env env;"
        writeLine "Call call, next_call;"
        writeLine ""
        writeLine "env = create_env(NULL);"
        forM (globalNames state) $ \(name, code) -> do
            writeLine $ "env_insert(env, \"" ++ name ++ "\", " ++ code ++ ");"
        writeLine ""
        writeLine "call = create_call((Closure)env_lookup(env, \"main\"), create_args(0));"
        writeLine ""
        writeLine "while (call != NULL) {"
        indented $ do
            writeLine "next_call = call->closure->fn_spec(call->closure->env, call->args);"
            writeLine "free_ref_countable(call);"
            writeLine "call = next_call;"
        writeLine "}"
        writeLine ""
        writeLine "free_ref_countable(env);"
        when (not (isLibrary options)) $ do
            writeLine ""
            writeLine "return 0;"
    writeLine "}"

cType T.Number       = "Number"
cType (T.Function _) = "Closure"

writeBuiltinCode builtinName builtins = gen $ fromJust $ M.lookup builtinName builtins
