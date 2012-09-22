/*
 * Adapted version of "Driving 5 Speakers Simultaneously with an Arduino".
 * http://www.jeremyblum.com/2010/09/05/driving-5-speakers-simultaneously-with-an-arduino/
 * ReacXion Source Code
 * www.jeremyblum.com
 *
 */

#include "WProgram.h"
#include "multitone.h"

/* Timer reload value, globally available */
unsigned int tcnt2;

#define SIZE 3

unsigned int pins[SIZE]       = {2, 3, 4};
unsigned int pinsActive[SIZE] = {0, 0, 0};
unsigned int toggles[SIZE]    = {0, 0, 0};
unsigned int counts[SIZE]     = {0, 0, 0};
unsigned int targets[SIZE]    = {0, 0, 0};
unsigned int tempo            = 250; // 120bpm

void setTone(int i, float tone) {
    if (tone == 0) {
        pinsActive[i] = 0;
    } else if (tone == 1) {
        pinsActive[i] = 1;
    } else {
        pinsActive[i] = 1;
        targets[i]    = (unsigned int)((1/tone)/(2*64*0.000001));
        toggles[i]    = 0;
        counts[i]     = 0;
    }
}

void deactivateFrom(int index) {
    int i;
    for (i = index; i < SIZE; i++) {
        pinsActive[i] = 0;
    }
}

void multitoneSetup() {
    int i;

    /* First disable the timer overflow interrupt*/
    TIMSK2 &= ~(1<<TOIE2);

    /* Configure timer2 in normal mode (no PWM) */
    TCCR2A &= ~((1<<WGM21) | (1<<WGM20));
    TCCR2B &= ~(1<<WGM22);

    /* Select clock source: internal I/O clock */
    ASSR &= ~(1<<AS2);

    /* Disable Compare Match A interrupt (only overflow) */
    TIMSK2 &= ~(1<<OCIE2A);

    /* Configure the prescaler to CPU clock divided by 128 */
    TCCR2B |= (1<<CS22)  | (1<<CS20); // Set bits
    TCCR2B &= ~(1<<CS21);             // Clear bit

    /* We need to calculate a proper value to load the counter.
    * The following loads the value 248 into the Timer 2 counter
    * The math behind this is:
    * (Desired period) = 64us.
    * (CPU frequency) / (prescaler value) = 125000 Hz -> 8us.
    * (desired period) / 8us = 8.
    * MAX(uint8) - 8 = 248;
    */
    /* Save value globally for later reload in ISR */
    tcnt2 = 248;

    /* Finally load end enable the timer */
    TCNT2 = tcnt2;
    TIMSK2 |= (1<<TOIE2);

    for (i = 0; i < SIZE; i++) {
        pinMode(pins[i], OUTPUT);
    }
}

void multitoneSetTempo(unsigned int t) {
    tempo = (unsigned int)(30000/t);
}

void multitoneSetOne(float one) {
    deactivateFrom(1);
    setTone(0, one);
    delay(tempo);
}

void multitoneSetTwo(float one, float two) {
    deactivateFrom(2);
    setTone(0, one);
    setTone(1, two);
    delay(tempo);
}

void multitoneSetThree(float one, float two, float three) {
    deactivateFrom(3);
    setTone(0, one);
    setTone(1, two);
    setTone(2, three);
    delay(tempo);
}

/* Install the Interrupt Service Routine (ISR) for Timer2.  */
ISR(TIMER2_OVF_vect) {
    int i;

    /* Reload the timer */
    TCNT2 = tcnt2;

    for (i = 0; i < SIZE; i++) {
        if (pinsActive[i]) {
            counts[i]++;
        } else {
            digitalWrite(pins[i], 0);
        }
    }

    for (i = 0; i < SIZE; i++) {
        if (pinsActive[i] && counts[i] == targets[i]) {
            digitalWrite(pins[i], toggles[i] == 0 ? HIGH : LOW);
            toggles[i] = ~toggles[i];
            counts[i] = 0;
        }
    }
}
