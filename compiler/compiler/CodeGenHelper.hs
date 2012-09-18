module CodeGenHelper where

import Control.Monad.Trans.State.Lazy as ST

runGenerator :: ST.State AccumulatedCode a -> String
runGenerator m = includesStr ++ bodyCode st
    where
        st = ST.execState m (AccumulatedCode 0 [] [] "")
        includesStr = concatMap (\x -> "#include " ++ x ++ "\n") (includes st)

data AccumulatedCode = AccumulatedCode
    { counter     :: Int
    , globalNames :: [(String, String)]
    , includes    :: [String]
    , bodyCode   :: String
    }

addInclude :: String -> ST.State AccumulatedCode ()
addInclude include = ST.modify (\s -> s { includes = update (includes s) })
    where
        update x | include `elem` x = x
                 | otherwise        = x ++ [include]

writeLine :: String -> ST.State AccumulatedCode ()
writeLine line = ST.modify (\s -> s { bodyCode = bodyCode s ++ line ++ "\n" })

addGlobalName :: String -> String -> ST.State AccumulatedCode ()
addGlobalName name code = ST.modify (\s -> s { globalNames = (name, code):globalNames s })

nextCounter :: ST.State AccumulatedCode Int
nextCounter = do
    state <- get
    ST.modify (\s -> s { counter = counter s + 1 })
    return $ counter state
