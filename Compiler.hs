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
        <|> fmap TermLambda lambda

-- Code generator

data GenState = GenState
    { counter     :: Int
    , globalNames :: [(String, Int)]
    , finalCode   :: String
    }

writeLine :: String -> GenState -> GenState
writeLine line gs = gs { finalCode = finalCode gs ++ line ++ "\n" }

generateCode :: Program -> String
generateCode program = finalCode $ ST.execState (outProgram program) (GenState 0 [] "")

outProgram (Program fns) = do
    ST.modify (writeLine "#import <stdio.h>")
    ST.modify (writeLine "")
    mapM_ outFunction fns
outFunction (Function name lambda) = do
    ST.modify (writeLine name)
    outLambda lambda
outLambda (Lambda args terms) = do
    ST.modify (writeLine "l")
