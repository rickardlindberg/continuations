module CodeGenHelper where

import Control.Monad.Trans.State.Lazy as ST

runGenerator :: ST.State AccumulatedCode a -> String
runGenerator m = finalCode $ ST.execState m (AccumulatedCode 0 [] "")

outList :: [a] -> (a -> ST.State AccumulatedCode ()) -> ST.State AccumulatedCode () -> ST.State AccumulatedCode ()
outList []     item separator = return ()
outList [x]    item separator = item x
outList (x:xs) item separator = item x >> separator >> outList xs item separator

data AccumulatedCode = AccumulatedCode
    { counter     :: Int
    , globalNames :: [(String, Int)]
    , finalCode   :: String
    }

writeLine :: String -> ST.State AccumulatedCode ()
writeLine line = ST.modify (\s -> s { finalCode = finalCode s ++ line ++ "\n" })

addGlobalName :: String -> Int -> ST.State AccumulatedCode ()
addGlobalName name n = ST.modify (\s -> s { globalNames = (name, n):globalNames s })

nextCounter :: ST.State AccumulatedCode Int
nextCounter = do
    state <- get
    ST.modify (\s -> s { counter = counter s + 1 })
    return $ counter state
