import qualified Backend as Backend
import Stages.Analyze (syntaxToSemantic)
import Stages.CodeGen (generateCode)
import Stages.Parser (translate)
import System.Directory
import System (exitFailure, getArgs)
import System.FilePath
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = do
    [srcPath, backendName] <- getArgs

    let Just backend = Backend.fromString backendName

    let runtimeDir   = "runtime"

    let buildDir     = takeDirectory srcPath </>
                       takeBaseName srcPath ++ "-conc-build-" ++ Backend.toString backend

    input <- readFile srcPath
    case parse translate srcPath input of
        Left  error -> do
            print error
            exitFailure
        Right program -> do
            createDirectoryIfMissing True buildDir
            (Backend.generateAndCompile backend)
                srcPath
                runtimeDir
                (generateCode backendName (syntaxToSemantic program))
                buildDir
