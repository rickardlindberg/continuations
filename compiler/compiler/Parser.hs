module Parser where

import Data
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec.Token (TokenParser, makeTokenParser)

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
          <|> parens term

whiteSpace = P.whiteSpace lexer
symbol     = P.symbol     lexer
natural    = P.natural    lexer
identifier = P.identifier lexer
reserved   = P.reserved   lexer
parens     = P.parens     lexer
lexer      = makeTokenParser $ haskellDef
             { P.reservedNames = ["let"]
             , P.identLetter   = P.identLetter haskellDef <|> char '?'
             }
