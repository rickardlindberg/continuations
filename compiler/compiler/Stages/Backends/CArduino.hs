module Stages.Backends.CArduino where

import CodeGenHelper
import qualified Data.Map as M
import qualified Stages.Backends.CCommon as CCommon
import qualified Types.Types as T
import System
import System.Directory
import System.FilePath
import System.Process
import Types.Semantic

builtins = CCommon.extendBuiltins CCommon.commonCBuiltins $ M.fromList
    [ ("setTempo", CCommon.CCommonBuiltin (T.Function [T.Number, T.Function []]) $ do
            addInclude "\"multitone.h\""
            writeLine "k = arg1;"
            writeLine "next_args = create_args(0);"
            writeLine "setTempo((unsigned int)arg0->value);"
            writeLine "return create_call(k, next_args);"
    )
    , ("setBeat1", CCommon.CCommonBuiltin (T.Function [T.Number, T.Function []]) $ do
            addInclude "\"multitone.h\""
            writeLine "k = arg1;"
            writeLine "next_args = create_args(0);"
            writeLine "setTone1((unsigned int)arg0->value);"
            writeLine "return create_call(k, next_args);"
    )
    , ("setBeat2", CCommon.CCommonBuiltin (T.Function [T.Number, T.Number, T.Function []]) $ do
            addInclude "\"multitone.h\""
            writeLine "k = arg2;"
            writeLine "next_args = create_args(0);"
            writeLine "setTone2((unsigned int)arg0->value, (unsigned int)arg1->value);"
            writeLine "return create_call(k, next_args);"
    )
    , ("setBeat3", CCommon.CCommonBuiltin (T.Function [T.Number, T.Number, T.Number, T.Function []]) $ do
            addInclude "\"multitone.h\""
            writeLine "k = arg3;"
            writeLine "next_args = create_args(0);"
            writeLine "setTone3((unsigned int)arg0->value, (unsigned int)arg1->value, (unsigned int)arg2->value);"
            writeLine "return create_call(k, next_args);"
    )
    ]

exportedBuiltins = CCommon.exportBuiltins builtins

generateAndCompile :: FilePath -> FilePath -> Program -> FilePath -> IO ()
generateAndCompile srcPath runtimeDir program buildDir = do

    let compiledCode = CCommon.generateCode (CCommon.Options True builtins) program

    let destPath     = buildDir </>
                       takeBaseName srcPath ++ ".cpp"

    let destHPath    = buildDir </>
                       takeBaseName srcPath ++ ".h"

    let makefilePath = buildDir </> "Makefile"

    let sketchPath   = buildDir </> "sketch.pde"

    let runtimeH     = runtimeDir </> "runtime.h"
    let runtimeC     = runtimeDir </> "runtime.c"

    let multiToneH   = runtimeDir </> "multitone.h"
    let multiToneC   = runtimeDir </> "multitone.cpp"

    writeFile destPath compiledCode
    writeFile destHPath destHCode
    writeFile makefilePath makefileCode
    writeFile sketchPath sketchCode
    copyFile runtimeH (buildDir </> "runtime.h")
    copyFile runtimeC (buildDir </> "runtime.cpp")
    copyFile multiToneH (buildDir </> "multitone.h")
    copyFile multiToneC (buildDir </> "multitone.cpp")
    setCurrentDirectory buildDir >> rawSystem "make" [] >>= exitWith

destHCode = unlines
    [ "void run();"
    ]

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
    [ "#include \"multitone.h\""
    , "#include \"music.h\""
    , ""
    , "void setup() {"
    , "    Serial.begin(9600);"
    , "    multiToneSetup();"
    , "    run();"
    , "}"
    , ""
    , "void loop() {"
    , "}"
    ]
