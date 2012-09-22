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
            addInclude "\"multitone.h\""
            writeLine "k = arg1;"
            writeLine "next_args = create_args(0);"
            writeLine "setTempo((unsigned int)arg0->value);"
            writeLine "return create_call(k, next_args);"
    , CCommon.CCommonBuiltin
        "setBeat1" (T.Function [T.Number, T.Function []]) $ do
            addInclude "\"multitone.h\""
            writeLine "k = arg1;"
            writeLine "next_args = create_args(0);"
            writeLine "setTone1((unsigned int)arg0->value);"
            writeLine "return create_call(k, next_args);"
    , CCommon.CCommonBuiltin
        "setBeat2" (T.Function [T.Number, T.Number, T.Function []]) $ do
            addInclude "\"multitone.h\""
            writeLine "k = arg2;"
            writeLine "next_args = create_args(0);"
            writeLine "setTone2((unsigned int)arg0->value, (unsigned int)arg1->value);"
            writeLine "return create_call(k, next_args);"
    , CCommon.CCommonBuiltin
        "setBeat3" (T.Function [T.Number, T.Number, T.Number, T.Function []]) $ do
            addInclude "\"multitone.h\""
            writeLine "k = arg3;"
            writeLine "next_args = create_args(0);"
            writeLine "setTone3((unsigned int)arg0->value, (unsigned int)arg1->value, (unsigned int)arg2->value);"
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

    let multiToneH   = buildDir </> "multitone.h"
    let multiToneC   = buildDir </> "multitone.cpp"

    let sketchPath   = buildDir </> "sketch.pde"

    let runtimeH     = runtimeDir </> "runtime.h"
    let runtimeC     = runtimeDir </> "runtime.c"

    writeFile destPath compiledCode
    writeFile destHPath destHCode
    writeFile makefilePath makefileCode
    writeFile sketchPath sketchCode
    writeFile multiToneH multiToneHCode
    writeFile multiToneC multiToneCCode
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
    , "#include \"multitone.h\""
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

multiToneHCode = unlines
    [ "#include \"WProgram.h\""
    , "void setTempo(unsigned int t);"
    , "void setTone1(float one);"
    , "void setTone2(float one, float two);"
    , "void setTone3(float one, float two, float three);"
    , "void clearFrom(int i);"
    , "void multiToneSetup();"
    ]

multiToneCCode = unlines
    [ "// Adapted verison of http://www.jeremyblum.com/2010/09/05/driving-5-speakers-simultaneously-with-an-arduino/"
    , ""
    , "//** ReacXion Source Code **//"
    , "//** www.jeremyblum.com **//"
    , ""
    , "#include \"WProgram.h\""
    , "#include \"multitone.h\""
    , ""
    , "/* Timer reload value, globally available */"
    , "unsigned int tcnt2;"
    , ""
    , "#define SIZE 3"
    , ""
    , "unsigned int pins[SIZE]       = {2, 3, 4};"
    , "unsigned int pinsActive[SIZE] = {0, 0, 0};"
    , "unsigned int toggles[SIZE]    = {0, 0, 0};"
    , "unsigned int counts[SIZE]     = {0, 0, 0};"
    , "unsigned int targets[SIZE]    = {0, 0, 0};"
    , "unsigned int tempo            = 250; // 120bpm"
    , ""
    , "void setTempo(unsigned int t) {"
    , "    tempo = (unsigned int)(30000/t);"
    , "}"
    , ""
    , "void setTone(int i, float tone) {"
    , "    if (tone == 0) {"
    , "        pinsActive[i] = 0;"
    , "    } else if (tone == 1) {"
    , "        pinsActive[i] = 1;"
    , "    } else {"
    , "        pinsActive[i] = 1;"
    , "        targets[i]    = (unsigned int)((1/tone)/(2*64*0.000001));"
    , "        toggles[i]    = 0;"
    , "        counts[i]     = 0;"
    , "    }"
    , "}"
    , ""
    , "void setTone1(float one) {"
    , "    clearFrom(1);"
    , "    setTone(0, one);"
    , "    delay(tempo);"
    , "}"
    , ""
    , "void setTone2(float one, float two) {"
    , "    clearFrom(2);"
    , "    setTone(0, one);"
    , "    setTone(1, two);"
    , "    delay(tempo);"
    , "}"
    , ""
    , "void setTone3(float one, float two, float three) {"
    , "    clearFrom(3);"
    , "    setTone(0, one);"
    , "    setTone(1, two);"
    , "    setTone(2, three);"
    , "    delay(tempo);"
    , "}"
    , ""
    , "void clearFrom(int index) {"
    , "    int i;"
    , "    for (i = index; i < SIZE; i++) {"
    , "        pinsActive[i] = 0;"
    , "    }"
    , "}"
    , ""
    , "void multiToneSetup() {"
    , "    int i;"
    , ""
    , "    /* First disable the timer overflow interrupt*/"
    , "    TIMSK2 &= ~(1<<TOIE2);"
    , ""
    , "    /* Configure timer2 in normal mode (no PWM) */"
    , "    TCCR2A &= ~((1<<WGM21) | (1<<WGM20));"
    , "    TCCR2B &= ~(1<<WGM22);"
    , ""
    , "    /* Select clock source: internal I/O clock */"
    , "    ASSR &= ~(1<<AS2);"
    , ""
    , "    /* Disable Compare Match A interrupt (only overflow) */"
    , "    TIMSK2 &= ~(1<<OCIE2A);"
    , ""
    , "    /* Configure the prescaler to CPU clock divided by 128 */"
    , "    TCCR2B |= (1<<CS22)  | (1<<CS20); // Set bits"
    , "    TCCR2B &= ~(1<<CS21);             // Clear bit"
    , ""
    , "    /* We need to calculate a proper value to load the counter."
    , "    * The following loads the value 248 into the Timer 2 counter"
    , "    * The math behind this is:"
    , "    * (Desired period) = 64us."
    , "    * (CPU frequency) / (prescaler value) = 125000 Hz -> 8us."
    , "    * (desired period) / 8us = 8."
    , "    * MAX(uint8) - 8 = 248;"
    , "    */"
    , "    /* Save value globally for later reload in ISR */"
    , "    tcnt2 = 248;"
    , ""
    , "    /* Finally load end enable the timer */"
    , "    TCNT2 = tcnt2;"
    , "    TIMSK2 |= (1<<TOIE2);"
    , ""
    , "    for (i = 0; i < SIZE; i++) {"
    , "        pinMode(pins[i], OUTPUT);"
    , "    }"
    , "}"
    , ""
    , "/* Install the Interrupt Service Routine (ISR) for Timer2.  */"
    , "ISR(TIMER2_OVF_vect) {"
    , "    int i;"
    , ""
    , "    /* Reload the timer */"
    , "    TCNT2 = tcnt2;"
    , ""
    , "    for (i = 0; i < SIZE; i++) {"
    , "        if (pinsActive[i]) {"
    , "            counts[i]++;"
    , "        } else {"
    , "            digitalWrite(pins[i], 0);"
    , "        }"
    , "    }"
    , ""
    , "    for (i = 0; i < SIZE; i++) {"
    , "        if (pinsActive[i] && counts[i] == targets[i]) {"
    , "            digitalWrite(pins[i], toggles[i] == 0 ? HIGH : LOW);"
    , "            toggles[i] = ~toggles[i];"
    , "            counts[i] = 0;"
    , "        }"
    , "    }"
    , "}"
    ]

destHCode = unlines
    [ "void run();"
    ]
