import Stages.Analyze (syntaxToSemantic)
import Stages.CodeGen (generateCode)
import Stages.Parser (translate)
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = do
    input <- getContents
    case parse translate "" input of
        Left  error   -> print error
        Right program -> putStr (generateCode (syntaxToSemantic program))
