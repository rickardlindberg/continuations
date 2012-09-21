module Stages.Backends.CArduino where

import CodeGenHelper
import qualified Stages.Backends.CCommon as CCommon
import qualified Types.Types as T
import System
import System.Directory
import System.FilePath
import System.Process
import Types.Semantic

builtins =
    [ CCommon.CCommonBuiltin
        "setTempo" (T.Function [T.Number, T.Function []]) $ do
            addInclude "\"extra.h\""
            addInclude "\"WProgram.h\""
            writeLine "k = arg1;"
            writeLine "next_args = create_args(0);"
            writeLine "tempo = (unsigned int)arg0->value;"
            writeLine "Serial.print(\"set tempo: \");"
            writeLine "Serial.println(tempo);"
            writeLine "return create_call(k, next_args);"
    , CCommon.CCommonBuiltin
        "setBeat1" (T.Function [T.Number, T.Function []]) $ do
            addInclude "\"extra.h\""
            addInclude "\"WProgram.h\""
            writeLine "k = arg1;"
            writeLine "next_args = create_args(0);"
            writeLine "beat1 = (unsigned int)arg0->value;"
            writeLine "tone(3, beat1);"
            writeLine "delay((60*1000)/tempo);"
            writeLine "Serial.print(\"set beat 1: \");"
            writeLine "Serial.println(beat1);"
            writeLine "return create_call(k, next_args);"
    ] ++ CCommon.commonCBuiltins

exportedBuiltins = CCommon.exportBuiltins builtins

generateAndCompile :: FilePath -> FilePath -> Program -> FilePath -> IO ()
generateAndCompile srcPath runtimeDir program buildDir = do

    let compiledCode = CCommon.generateCode (CCommon.Options True builtins) program

    let destPath     = buildDir </>
                       takeBaseName srcPath ++ ".cpp"

    let destHPath    = buildDir </>
                       takeBaseName srcPath ++ ".h"

    let destName     = takeFileName destPath

    let destBinName  = takeBaseName destPath

    let makefilePath = buildDir </> "Makefile"

    let extraH       = buildDir </> "extra.h"

    let sketchPath   = buildDir </> "sketch.pde"

    let runtimeH     = runtimeDir </> "runtime.h"
    let runtimeC     = runtimeDir </> "runtime.c"

    writeFile destPath compiledCode
    writeFile destHPath destHCode
    writeFile makefilePath makefileCode
    writeFile sketchPath sketchCode
    writeFile extraH extraCode
    copyFile runtimeH (buildDir </> "runtime.h")
    copyFile runtimeC (buildDir </> "runtime.cpp")
    setCurrentDirectory buildDir >> rawSystem "make" [] >>= exitWith

makefileCode = unlines
    [ "BOARD_TAG     = uno"
    , "ARDUINO_PORT  = /dev/ttyACM*"
    , "ARDUINO_LIBS  ="
    , ""
    , "ARDUINO_DIR   = /usr/share/arduino"
    , "ARDMK_DIR     = /usr/local"
    , "AVR_TOOLS_DIR = /usr"
    , ""
    , "AM_HOME       = $(HOME)/downloads/Arduino-Makefile"
    , "ARDMK_PATH    = $(AM_HOME)/bin"
    , ""
    , "include $(AM_HOME)/arduino-mk/Arduino.mk"
    ]

sketchCode = unlines
    [ "#include \"music.h\""
    , ""
    , "void setup() {"
    , "  pinMode(3, OUTPUT);"
    , "  pinMode(13, OUTPUT);"
    , "  pinMode(12, OUTPUT);"
    , "  Serial.begin(9600);"
    , "}"
    , ""
    , "void loop()"
    , "{"
    , "    run();"
    , "}"
    ]

extraCode = unlines
    [ "unsigned int tempo = 120;"
    , "unsigned int beat1 = 0;"
    ]

destHCode = unlines
    [ "void run();"
    ]
