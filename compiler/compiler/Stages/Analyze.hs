module Stages.Analyze where

import qualified Backends.Builtins as B
import qualified Types.Semantic as Sem
import qualified Types.Syntax as Syn
import qualified Types.Types as T
import Stages.Optimize (optimize)

syntaxToSemantic :: Syn.Program -> Sem.Program
syntaxToSemantic = optimize . addBuiltins . convertProgram

convertProgram :: Syn.Program -> Sem.Program
convertProgram (Syn.Program lets)           = Sem.Program (map convertLet lets)
convertLet     (Syn.Let name term)          = Sem.Let name (convertTerm term)
convertTerm    (Syn.Identifier name)        = Sem.Identifier name
convertTerm    (Syn.Number num)             = Sem.Number num
convertTerm    (Syn.Lambda lets args terms) =
    Sem.Function T.Unknown (Sem.Lambda args (convertLets lets terms))
convertLets    lets innerMostTerms          =
    foldr wrapWithLet (map convertTerm innerMostTerms) lets

wrapWithLet :: Syn.Let -> [Sem.Term] -> [Sem.Term]
wrapWithLet (Syn.Let name term) innerTerms =
    [ Sem.Function T.Unknown (Sem.Lambda [name] innerTerms)
    , convertTerm term
    ]

addBuiltins :: Sem.Program -> Sem.Program
addBuiltins (Sem.Program lets) = Sem.Program (builtinLets ++ lets)
    where
        builtinLets = map builtinToLet nonOverridenBuiltins
        nonOverridenBuiltins = filter (\(b) -> B.name b `notElem` letNames) B.builtins
        letNames = map (\(Sem.Let name _) -> name) lets

builtinToLet :: B.Builtin -> Sem.Let
builtinToLet builtin = Sem.Let (B.name builtin) (Sem.Function (B.fnType builtin) (b2b builtin))
    where
        b2b (B.Builtin n c t i) = (Sem.Builtin i c)
