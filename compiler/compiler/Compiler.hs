import qualified Stages.Backend as Backend
import Stages.Analyze (syntaxToSemantic)
import Stages.Parser (textToSyntax)
import System.Directory
import System (exitFailure, getArgs)
import System.FilePath

main :: IO ()
main = do
    [srcPath, backendName] <- getArgs

    let Just backend = Backend.fromString backendName

    let runtimeDir   = "runtime"

    let buildDir     = takeDirectory srcPath </>
                       takeBaseName srcPath ++ "-conc-build-" ++ Backend.toString backend

    input            <- readFile srcPath

    case textToSyntax srcPath input of
        Left  error   -> putStrLn error >> exitFailure
        Right program -> do
            case syntaxToSemantic backend program of
                Left  error   -> putStrLn error >> exitFailure
                Right program -> do
                    createDirectoryIfMissing True buildDir
                    (Backend.generateAndCompile backend)
                        srcPath
                        runtimeDir
                        program
                        buildDir
