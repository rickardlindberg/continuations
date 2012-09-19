module Stages.Optimize where

import Types.Semantic

optimize :: Program -> Program
optimize = removeUnusedFunctions

removeUnusedFunctions :: Program -> Program
removeUnusedFunctions program@(Program lets) = Program $ filter isUsed lets
    where
        isUsed (Let name _) = name `elem` usedGlobalNames
        usedGlobalNames = "main" : extractUsedGlobalNames program

extractUsedGlobalNames :: Program -> [String]
extractUsedGlobalNames (Program lets) = extractFromLets lets []
    where
        extractFromLets lets              localNames = concatMap (\l -> extractFromLet l localNames) lets
        extractFromLet (Let _ t)          localNames = extractFromTerm t localNames
        extractFromTerms terms            localNames = concatMap (\t -> extractFromTerm t localNames) terms
        extractFromTerm (Identifier s)    localNames = if s `elem` localNames then [] else [s]
        extractFromTerm (Function _ body) localNames = extractFromFn body localNames
        extractFromTerm _                 _          = []
        extractFromFn (Lambda args terms) localNames = extractFromTerms terms (localNames ++ args)
        extractFromFn _                   _          = []
