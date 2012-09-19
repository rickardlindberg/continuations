module Runtime.CArduino where

import System
import System.Directory
import System.FilePath
import System.Process

generateAndCompile :: FilePath -> FilePath -> String -> FilePath -> IO ()
generateAndCompile srcPath runtimeDir compiledCode buildDir = do

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
