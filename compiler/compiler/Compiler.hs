import qualified Backends.CArduino as CArduino
import qualified Backends.CPC as CPC
import qualified Data.Map as M
import Stages.Analyze (syntaxToSemantic)
import Stages.CodeGen (generateCode)
import Stages.Parser (translate)
import System.Directory
import System (exitFailure, getArgs)
import System.FilePath
import Text.ParserCombinators.Parsec (parse)

runtimes = M.fromList [ ("cpc"     , CPC.generateAndCompile)
                      , ("carduino", CArduino.generateAndCompile)
                      ]

main :: IO ()
main = do
    [srcPath, runtimeName] <- getArgs

    let runtimeDir = "runtime"

    let buildDir   = takeDirectory srcPath </>
                     takeBaseName srcPath ++ "-conc-build-" ++ runtimeName

    case M.lookup runtimeName runtimes of
        Nothing -> exitFailure
        _       -> return ()

    let (Just generateAndCompile) = M.lookup runtimeName runtimes

    input <- readFile srcPath
    case parse translate srcPath input of
        Left  error -> do
            print error
            exitFailure
        Right program -> do
            createDirectoryIfMissing True buildDir
            generateAndCompile
                srcPath
                runtimeDir
                (generateCode runtimeName (syntaxToSemantic program))
                buildDir
