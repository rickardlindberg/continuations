import CodeGen (generateCode)
import Parser (translate)
import qualified Builtins as B
import qualified Types.Semantic as Sem
import qualified Types.Syntax as Syn
import qualified Types.Types as T
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = do
    input <- getContents
    case parse translate "" input of
        Left  error   -> print error
        Right program -> putStr (generateCode (syntaxToSemantic program))

syntaxToSemantic :: Syn.Program -> Sem.Program
syntaxToSemantic (Syn.Program lets) = addBuiltins $ Sem.Program (map convertLet lets)
    where
        convertLet  (Syn.Let s t)      = Sem.Let s (convertTerm t)
        convertTerm (Syn.Identifier s) = Sem.Identifier s
        convertTerm (Syn.Number n)     = Sem.Number n
        convertTerm (Syn.Lambda ss ts) = Sem.Function T.Unknown (Sem.Lambda ss (map convertTerm ts))

addBuiltins :: Sem.Program -> Sem.Program
addBuiltins (Sem.Program lets) = Sem.Program (newLets ++ lets)
    where
        newLets = map (\(b) -> Sem.Let (B.name b) (Sem.Function (B.fnType b) (b2b b))) B.builtins
        b2b (B.Builtin n c t i) = (Sem.Builtin i c)
