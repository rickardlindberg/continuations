module Backends.CPC where

import Stages.CodeGen (generateCode)
import System
import System.Directory
import System.FilePath
import System.Process
import Types.Semantic

generateAndCompile :: FilePath -> FilePath -> Program -> FilePath -> IO ()
generateAndCompile srcPath runtimeDir program buildDir = do

    let compiledCode = generateCode False program

    let destPath     = buildDir </>
                       takeBaseName srcPath ++ ".c"

    let destName     = takeFileName destPath

    let destBinName  = takeBaseName destPath

    let makefilePath = buildDir </> "Makefile"

    let runtimeH     = runtimeDir </> "runtime.h"
    let runtimeC     = runtimeDir </> "runtime.c"

    let makefileCode = destBinName ++ ": " ++ destName ++ " runtime.c runtime.h\n\tgcc -lm -o " ++ destBinName ++ " " ++ destName ++ " runtime.c\n"

    writeFile destPath compiledCode
    writeFile makefilePath makefileCode
    copyFile runtimeH (buildDir </> "runtime.h")
    copyFile runtimeC (buildDir </> "runtime.c")
    setCurrentDirectory buildDir >> rawSystem "make" [] >>= exitWith
