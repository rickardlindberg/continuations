import Control.Monad
import Control.Monad.Trans.State.Lazy as ST
import Data
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec.Token (TokenParser, makeTokenParser)

main :: IO ()
main = do
    input <- getContents
    case parse translate "" input of
        Left  error   -> print error
        Right program -> putStr (generateCode program)

-- Parser

translate :: Parser Program
translate  =  do whiteSpace
                 p <- program
                 eof
                 return p
program    =  fmap Program (many function)
function   =  do reserved "let"
                 name <- identifier
                 symbol "="
                 l <- lambda
                 return (Function name l)
lambda     =  do symbol "\\("
                 args <- sepBy identifier (symbol ",")
                 symbol ")"
                 symbol "->"
                 terms <- many1 term
                 return (Lambda args terms)
term       =  fmap Identifier identifier
          <|> fmap Number     natural
          <|> fmap TermLambda lambda

whiteSpace = P.whiteSpace lexer
symbol     = P.symbol     lexer
natural    = P.natural    lexer
identifier = P.identifier lexer
reserved   = P.reserved   lexer
lexer      = makeTokenParser $ haskellDef { P.reservedNames = ["let"] }

-- Code generator

generateCode :: Program -> String
generateCode program = finalCode $ ST.execState (outProgram program) (AccumulatedCode 0 [] "")

outProgram :: Program -> ST.State AccumulatedCode ()
outProgram (Program fns) = do
    writeLine "#include <stdio.h>"
    writeLine "#include <stdlib.h>"
    writeLine "#include \"runtime.h\""
    writeLine ""
    forM fns $ \fn -> do
        outFunction fn
        writeLine ""
    writeLine "int main() {"
    writeLine "    Env env;"
    writeLine "    Call call, next_call;"
    writeLine ""
    writeLine "    env = create_env(NULL);"
    s <- get
    let gn = globalNames s
    let bn = ["times", "plus", "sqrt", "printNumber", "exit"]
    mapM_ (\(name, n) -> writeLine $ "    env_insert(env, \"" ++ name ++ "\", create_closure(&fn_" ++ show n ++ ", env));") gn
    mapM_ (\name -> writeLine      $ "    env_insert(env, \"" ++ name ++ "\", create_closure(&builtin_" ++ name ++ ", env));") bn
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

outFunction :: Function -> ST.State AccumulatedCode ()
outFunction (Function name lambda) = do
    n <- outLambda lambda
    addGlobalName name n
    return ()

outLambda :: Lambda -> ST.State AccumulatedCode Int
outLambda (Lambda args terms) = do
    n <- nextCounter
    terms <- mapM outTerm terms
    writeLine $ "Call fn_" ++ show n ++ "(Env env, Args args) {"
    writeLine $ "    Args next_args;"
    writeLine $ "    Closure closure;"
    writeLine $ "    Env new_env;"
    writeLine $ ""
    writeLine $ "    new_env = create_env(env);"
    forM (zip [0..] args) $ \(i, arg) -> do
        writeLine $ "    env_insert(new_env, \"" ++ arg ++ "\", args_get(args, " ++ show i ++ "));"
    writeLine $ ""
    writeLine $ "    closure = (Closure)" ++ head terms ++ ";"
    writeLine $ ""
    writeLine $ "    next_args = create_args(" ++ show (length (tail terms)) ++ ");"
    forM (zip [0..] (tail terms)) $ \(i, term) -> do
        writeLine $ "    args_set(next_args, " ++ show i ++ ", " ++ term ++ ");"
    writeLine $ ""
    writeLine $ "    free_ref_countable(new_env);"
    writeLine $ ""
    writeLine $ "    return create_call(closure, next_args);"
    writeLine $ "}"
    return n

outTerm :: Term -> ST.State AccumulatedCode String
outTerm (Identifier s) = return $ "env_lookup(new_env, \"" ++ s ++ "\")"
outTerm (Number     n) = return $ "const_number(" ++ show n ++ ")"
outTerm (TermLambda l) = do
    n <- outLambda l
    writeLine ""
    return $ "create_closure(&fn_" ++ show n ++ ", new_env)"

data AccumulatedCode = AccumulatedCode
    { counter     :: Int
    , globalNames :: [(String, Int)]
    , finalCode   :: String
    }

writeLine :: String -> ST.State AccumulatedCode ()
writeLine line = ST.modify (\s -> s { finalCode = finalCode s ++ line ++ "\n" })

addGlobalName :: String -> Int -> ST.State AccumulatedCode ()
addGlobalName name n = ST.modify (\s -> s { globalNames = (name, n):globalNames s })

nextCounter :: ST.State AccumulatedCode Int
nextCounter = do
    state <- get
    ST.modify (\s -> s { counter = counter s + 1 })
    return $ counter state
