module Stages.Backends.CArduino where

import CodeGenHelper
import qualified Stages.Backends.CCommon as CCommon
import qualified Types.Types as T
import System
import System.Directory
import System.FilePath
import System.Process
import Types.Semantic

my =
    [ CCommon.CCommonBuiltin
        "setTempo" (T.Function [T.Number, T.Function []]) $ do
            addInclude "\"WProgram.h\""
            writeLine "k = arg1;"
            writeLine "next_args = create_args(0);"
            writeLine "Serial.print(\"set tempo: \");"
            writeLine "Serial.println(arg0->value);"
            writeLine "return create_call(k, next_args);"
    , CCommon.CCommonBuiltin
        "setBeat1" (T.Function [T.Number, T.Function []]) $ do
            addInclude "\"WProgram.h\""
            writeLine "k = arg1;"
            writeLine "next_args = create_args(0);"
            writeLine "Serial.print(\"set beat 1: \");"
            writeLine "Serial.println(arg0->value);"
            writeLine "return create_call(k, next_args);"
    ]

myBuiltins = my ++ CCommon.commonCBuiltins

getBuiltins = CCommon.getBuiltins myBuiltins

generateAndCompile :: FilePath -> FilePath -> Program -> FilePath -> IO ()
generateAndCompile srcPath runtimeDir program buildDir = do

    let compiledCode = CCommon.generateCode (CCommon.Options True myBuiltins) program

    let destPath     = buildDir </>
                       takeBaseName srcPath ++ ".cpp"

    let destHPath    = buildDir </>
                       takeBaseName srcPath ++ ".h"

    let destName     = takeFileName destPath

    let destBinName  = takeBaseName destPath

    let makefilePath = buildDir </> "Makefile"

    let sketchPath   = buildDir </> "sketch.pde"

    let runtimeH     = runtimeDir </> "runtime.h"
    let runtimeC     = runtimeDir </> "runtime.c"

    writeFile destPath compiledCode
    writeFile destHPath destHCode
    writeFile makefilePath makefileCode
    writeFile sketchPath sketchCode
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
    , "const int ledPin = 13;"
    , ""
    , "void setup() {"
    , "  pinMode(ledPin, OUTPUT);"
    , "  Serial.begin(9600);"
    , "}"
    , ""
    , "void loop()"
    , "{"
    , "    Serial.println(\"low\");"
    , "    digitalWrite(ledPin, LOW);"
    , "    delay(1000);"
    , "    Serial.println(\"high\");"
    , "    digitalWrite(ledPin, HIGH);"
    , "    delay(1000);"
    , "    Serial.println(\"music\");"
    , "    run();"
    , "}"
    ]

destHCode = unlines
    [ "void run();"
    ]
