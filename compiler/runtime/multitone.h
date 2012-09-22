#ifndef MULTITONE_H
#define MULTITONE_H

#include "WProgram.h"

void multitoneSetup();
void multitoneSetTempo(unsigned int t);
void multitoneSetOne(float one);
void multitoneSetTwo(float one, float two);
void multitoneSetThree(float one, float two, float three);

#endif
