---
layout: post
title: Computer Controlled Oscillator
tags: blog
score: 12
teaser: Notes for the blinkbit DCO design
img: /images/bbsamp.jpg
imgalt: caption
---

What is a DCO?

Overview of a Juno-style DCO

Why a DCO?

How to design:

The DCO consists of the following parts: a digitally controlled pulse wave, a voltage controlled switch, an integrator, and a digitally controlled voltage input.  For the blinkbit DCO, the digitally controlled pulse wave will be created by the xmega's timers and a simple RC differentiator.  A simple bipolar transistor will suffice for the switch.  The voltage input will come from our DAC on the xmega, and for an integrator we can use a simple RC integrator.  So, given that framework, we just have to choose component values and how much voltage to send out the DAC for each frequency.

The two concerns here are distortion and amplitude.  The lower the frequency, the closer we get to our RC time constant in period, so the further we get from an ideal saw wave.  But, the lower the frequency, the lower the amplitude.  The rightt approach is to first fix the RC time constant so that the lowest frequncy you care about will have minimal distortion.  

We've chosen an RC time constant of .02 seconds, implemented by a .1uF capacitor and a 200k resistor.  Roughly, this means we'll have minimal distortion above 50hz, while we'll have plenty of distortion below that.  This is fine, because frequencies lower than 50hz are most useful for percussive effects and in that context a little distortion won't be much of a problem.

So, now that we've ensured minimal distortion, we must ensure approximately constant gain for the remaining frequencies.  The amplitude as a function of frequency is (1 - e^(-1/RCf)) * Vin, where Vin is from the DAC.  We can generate stable frequencies up to 5kHz or so, so I'll choose that as the top range for constant amplitude.  Then, the amplitude at full voltage from the dac, at 5kHz is approximately .01v.  So, we just invert the amplitude curve, starting from 1% dac output.

<img src="/images/DCO_DCO20.png">

TODO: AC coupling - aim for 1.5 Hz cut off - this is 1uf, 100k resistor