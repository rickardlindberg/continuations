module Analyze where

import qualified Builtins as B
import qualified Types.Semantic as Sem
import qualified Types.Syntax as Syn
import qualified Types.Types as T

syntaxToSemantic :: Syn.Program -> Sem.Program
syntaxToSemantic (Syn.Program lets) = addBuiltins $ Sem.Program (map convertLet lets)
    where
        convertLet  (Syn.Let s t)      = Sem.Let s (convertTerm t)
        convertTerm (Syn.Identifier s) = Sem.Identifier s
        convertTerm (Syn.Number n)     = Sem.Number n
        convertTerm (Syn.Lambda ss ts) = Sem.Function T.Unknown (Sem.Lambda ss (map convertTerm ts))

extractBuiltinNames :: Sem.Program -> [String]
extractBuiltinNames (Sem.Program lets) = extractFromLets lets globalNames
    where
        globalNames = map (\(Sem.Let name _) -> name) lets
        extractFromLets lets existing = concatMap (\l -> extractFromLet l existing) lets
        extractFromLet (Sem.Let _ t) existing = extractFromTerm t existing
        extractFromTerms terms existing = concatMap (\t -> extractFromTerm t existing) terms
        extractFromTerm (Sem.Identifier s)    existing = if s `elem` existing then [] else [s]
        extractFromTerm (Sem.Function _ body) existing = extractFromFn body existing
        extractFromTerm _                     _ = []
        extractFromFn (Sem.Lambda args terms) existing = extractFromTerms terms (existing ++ args)
        extractFromFn _                       _        = []

addBuiltins :: Sem.Program -> Sem.Program
addBuiltins (Sem.Program lets) = Sem.Program (newLets ++ lets)
    where
        newLets = map (\(b) -> Sem.Let (B.name b) (Sem.Function (B.fnType b) (b2b b)))
                      (filter (\(b) -> B.name b `elem` sfd) B.builtins)
        sfd = extractBuiltinNames (Sem.Program lets)
        b2b (B.Builtin n c t i) = (Sem.Builtin i c)
