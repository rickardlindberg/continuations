import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Token (TokenParser, makeTokenParser)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (haskellDef)

main :: IO ()
main = compile

compile :: IO ()
compile = do
    input <- getContents
    case parse translate "" input of
        Left  error   -> print error
        Right program -> putStr (generateCode program)

-- Data for program

data Program = Program [Function]

data Function = Function String Lambda

data Lambda = Lambda [String] [Term]

data Term = Identifier String
          | Number Integer
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

generateCode :: Program -> String
generateCode p = "c code here..."
