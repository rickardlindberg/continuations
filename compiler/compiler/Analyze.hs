module Analyze where

import Optimize (optimize)
import qualified Builtins as B
import qualified Types.Semantic as Sem
import qualified Types.Syntax as Syn
import qualified Types.Types as T

syntaxToSemantic :: Syn.Program -> Sem.Program
syntaxToSemantic (Syn.Program lets) = optimize $ addBuiltins $ Sem.Program (map convertLet lets)
    where
        convertLet  (Syn.Let s t)      = Sem.Let s (convertTerm t)
        convertTerm (Syn.Identifier s) = Sem.Identifier s
        convertTerm (Syn.Number n)     = Sem.Number n
        convertTerm (Syn.Lambda ss ts) = Sem.Function T.Unknown (Sem.Lambda ss (map convertTerm ts))

addBuiltins :: Sem.Program -> Sem.Program
addBuiltins (Sem.Program lets) = Sem.Program (newLets ++ lets)
    where
        newLets = map (\(b) -> Sem.Let (B.name b) (Sem.Function (B.fnType b) (b2b b)))
                      (filter (\(b) -> B.name b `notElem` letNames) B.builtins)
        letNames = map (\(Sem.Let name _) -> name) lets
        b2b (B.Builtin n c t i) = (Sem.Builtin i c)
