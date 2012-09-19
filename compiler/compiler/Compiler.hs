import Stages.Analyze (syntaxToSemantic)
import Stages.CodeGen (generateCode)
import Stages.Parser (translate)
import System
import System.Directory
import System.FilePath
import System.Process
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = do

    [srcFilePath] <- getArgs

    let runtimeDir = "runtime"

    let runtimeH   = runtimeDir </> "runtime.h"
    let runtimeC   = runtimeDir </> "runtime.c"

    let buildDir   = takeDirectory srcFilePath </>
                     takeBaseName srcFilePath ++ "-conc-build"

    let destFile   = buildDir </>
                     takeBaseName srcFilePath ++ ".c"

    input <- readFile srcFilePath
    case parse translate srcFilePath input of
        Left  error   -> print error
        Right program -> do

            createDirectoryIfMissing True buildDir

            writeFile destFile (generateCode (syntaxToSemantic program))

            copyFile runtimeH (buildDir </> "runtime.h")
            copyFile runtimeC (buildDir </> "runtime.c")

            let makefile = buildDir </> "Makefile"
            let oo = dropExtension (takeBaseName destFile)
            let makefileLines = [ oo ++ ": " ++ takeFileName destFile ++ " runtime.c runtime.h\n"
                                , "\t gcc -lm -o " ++ oo ++ " " ++ takeFileName destFile ++ " runtime.c\n"
                                ]

            writeFile makefile (concat makefileLines)

            setCurrentDirectory buildDir

            res <- rawSystem "make" []
            exitWith res
