module Optimize where

import Types.Semantic

optimize = removeUnusedFunctions

removeUnusedFunctions :: Program -> Program
removeUnusedFunctions program@(Program lets) = Program newLets
    where
        usedGlobalNames = "main" : extractGlobalNames program
        newLets = filter (\(Let name _) -> name `elem` usedGlobalNames) lets

extractGlobalNames :: Program -> [String]
extractGlobalNames (Program lets) = extractFromLets lets []
    where
        extractFromLets lets existing = concatMap (\l -> extractFromLet l existing) lets
        extractFromLet (Let _ t) existing = extractFromTerm t existing
        extractFromTerms terms existing = concatMap (\t -> extractFromTerm t existing) terms
        extractFromTerm (Identifier s)    existing = if s `elem` existing then [] else [s]
        extractFromTerm (Function _ body) existing = extractFromFn body existing
        extractFromTerm _                     _ = []
        extractFromFn (Lambda args terms) existing = extractFromTerms terms (existing ++ args)
        extractFromFn _                       _        = []
