import Runtime.CPC (compile)
import System (getArgs)

main :: IO ()
main = do
    [filepath] <- getArgs
    compile filepath
