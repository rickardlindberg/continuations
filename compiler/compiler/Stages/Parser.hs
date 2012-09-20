module Stages.Parser where

import Control.Monad
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Text.ParserCombinators.Parsec.Token (makeTokenParser)
import Types.Syntax

textToSyntax :: FilePath -> String -> Either String Program
textToSyntax srcPath input =
    case parse translate srcPath input of
        Left  error   -> Left $ show error
        Right program -> Right program

translate :: Parser Program
translate  =  do whiteSpace
                 p <- program
                 eof
                 return p
program    =  fmap Program (many letp)
letp       =  do reserved "let"
                 name <- identifier
                 symbol "="
                 t <- term
                 return (Let name t)
term       =  fmap Identifier identifier
          <|> fmap Number     natural
          <|> do symbol "\\("
                 args <- sepBy identifier (symbol ",")
                 symbol ")"
                 symbol "->"
                 lets <- many letp
                 when (not (null lets)) (reserved "in")
                 terms <- many1 term
                 return (Lambda lets args terms)
          <|> parens term

whiteSpace = P.whiteSpace lexer
symbol     = P.symbol     lexer
natural    = P.natural    lexer
identifier = P.identifier lexer
reserved   = P.reserved   lexer
parens     = P.parens     lexer
lexer      = makeTokenParser $ haskellDef
             { P.reservedNames = ["let", "in"]
             , P.identLetter   = P.identLetter haskellDef <|> char '?' <|> char '-'
             }
