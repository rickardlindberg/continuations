import Control.Monad
import Control.Monad.Trans.State.Lazy as ST
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec.Token (TokenParser, makeTokenParser)

main :: IO ()
main = compile

compile :: IO ()
compile = do
    input <- getContents
    case parse translate "" input of
        Left  error   -> print error
        Right program -> putStr (generateCode program)

-- Data for program

data Program  = Program [Function]

data Function = Function String Lambda

data Lambda   = Lambda [String] [Term]

data Term     = Identifier String
              | Number     Integer
              | TermLambda Lambda

-- Parser

lexer :: TokenParser ()
lexer  = makeTokenParser $ haskellDef
                         { P.reservedOpNames = ["*", "/", "+", "-"]
                         , P.reservedNames   = ["let"]
                         }
whiteSpace = P.whiteSpace lexer
symbol     = P.symbol lexer
natural    = P.natural lexer
parens     = P.parens lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer

translate :: Parser Program
translate = do
    whiteSpace
    p <- program
    eof
    return p

program  = fmap Program (many function)
function = do reserved "let"
              name <- identifier
              symbol "="
              l <- lambda
              return (Function name l)
lambda   = do symbol "\\("
              args <- sepBy identifier (symbol ",")
              symbol ")"
              symbol "->"
              terms <- many1 term
              return (Lambda args terms)
term     =  fmap Identifier identifier
        <|> fmap Number     natural
        <|> fmap TermLambda lambda

-- Code generator

data GenState = GenState
    { counter     :: Int
    , globalNames :: [(String, Int)]
    , finalCode   :: String
    }

writeLine line = ST.modify (doit line)
    where doit line gs = gs { finalCode = finalCode gs ++ line ++ "\n" }

addGlobalName name n = ST.modify (\s -> s { globalNames = (name, n):globalNames s })

nextCounter = do
    state <- get
    ST.modify (\s -> s { counter = counter s + 1 })
    return $ counter state

generateCode :: Program -> String
generateCode program = finalCode $ ST.execState (outProgram program) (GenState 0 [] "")

outProgram (Program fns) = do
    writeLine "#import \"runtime.h\""
    writeLine ""
    mapM_ outFunction fns
    writeLine "int main() {"
    writeLine "    Env env;"

    writeLine ""

    s <- get
    let gn = globalNames s
    writeLine $ "    env = create_env(NULL);"
    mapM_ (\(name, n) -> writeLine $ "    env_insert(env, \"" ++ name ++ "\", create_binding(&fn_" ++ show n ++ ", env));") gn

    writeLine ""

    writeLine "    return 0;"
    writeLine "}"

outFunction (Function name lambda) = do
    n <- outLambda lambda
    addGlobalName name n
    return ()

outLambda (Lambda args terms) = do
    n <- nextCounter
    terms <- mapM outTerm terms
    writeLine $ "void fn_" ++ show n ++ "(...) {"

    writeLine $ "    Args args;"
    writeLine $ "    Binding binding;"
    writeLine $ "    Env new_env;"

    writeLine $ ""

    writeLine $ "    new_env = create_env(env);"
    forM (zip [0..] args) $ \(i, arg) -> do
        writeLine $ "    env_insert(new_env, arg_get(args, " ++ show i ++ "));"

    writeLine $ ""

    writeLine $ "    binding = " ++ head terms ++ ";"

    writeLine $ ""

    writeLine $ "    args = create_args(" ++ show (length (tail terms)) ++ ");"
    forM (zip [0..] (tail terms)) $ \(i, term) -> do
        writeLine $ "    args_set(" ++ show i ++ ", " ++ term ++ ");"

    writeLine $ ""

    writeLine $ "    return unit_new(binding, args);"

    writeLine $ "}"
    writeLine $ ""
    return n

outTerm (Identifier s) = return $ "lookup(env, \"" ++ s ++ "\")"
outTerm (Number     n) = return $ "const_int(" ++ show n ++ ")"
outTerm (TermLambda l) = outLambda l >>= \n ->
                         return $ "create_binding(&fn_" ++ show n ++ ", new_env)"
