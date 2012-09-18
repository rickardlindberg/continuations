#!/bin/sh

(cd ../.. && make)

cp ../../runtime/runtime.h runtime.h
cp ../../runtime/runtime.c runtime.cpp

cat ../music.c \
    | sed 's/int main/int music/' \
    | sed 's/#include "runtime.h"/#include "runtime.h"\n#include "WProgram.h"/' \
    | sed 's/printf("set tempo[^)]*)/Serial.print("tempo: "); Serial.println(arg0->value)/' \
    | sed 's/printf("set beat[^)]*)/Serial.print("beat 1: "); Serial.println(arg0->value)/' \
    > music.cpp

rm -f music.h
echo "#ifndef MUSIC_H" >> music.h
echo "#define MUSIC_H" >> music.h
echo ""                >> music.h
echo "int music();"    >> music.h
echo ""                >> music.h
echo "#endif"          >> music.h

rm -f sketch.pde
echo "#include \"music.h\""            >> sketch.pde
echo ""                                >> sketch.pde
echo "const int ledPin = 13;"          >> sketch.pde
echo ""                                >> sketch.pde
echo "void setup() {"                  >> sketch.pde
echo "  pinMode(ledPin, OUTPUT);"      >> sketch.pde
echo "  Serial.begin(9600);"           >> sketch.pde
echo "}"                               >> sketch.pde
echo ""                                >> sketch.pde
echo "void loop()"                     >> sketch.pde
echo "{"                               >> sketch.pde
echo "    Serial.println(\"low\");"    >> sketch.pde
echo "    digitalWrite(ledPin, LOW);"  >> sketch.pde
echo "    delay(1000);"                >> sketch.pde
echo "    Serial.println(\"high\");"   >> sketch.pde
echo "    digitalWrite(ledPin, HIGH);" >> sketch.pde
echo "    delay(1000);"                >> sketch.pde
echo "    Serial.println(\"music\");"  >> sketch.pde
echo "    music();"                    >> sketch.pde
echo "}"                               >> sketch.pde