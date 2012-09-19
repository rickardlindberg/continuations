import Runtime.CPC
import Stages.Analyze (syntaxToSemantic)
import Stages.CodeGen (generateCode)
import Stages.Parser (translate)
import System.Directory
import System (exitFailure, getArgs)
import System.FilePath
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = do
    [srcPath] <- getArgs
    let runtimeDir = "runtime"
    let runtimeName = "cpc"
    let buildDir   = takeDirectory srcPath </>
                     takeBaseName srcPath ++ "-conc-build-" ++ runtimeName
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
                (generateCode (syntaxToSemantic program))
                buildDir
