---
layout: post
title: Eurorack Headphone Amp
tags: blog
score: 1
teaser: 
img: /images/headphonebanner.png
imgalt: headphone amp circuit board
---

![PCB](/images/headphone_side.jpg)

## background

Continuing my slow but steady progress on my [DIY modular synthesizer](/posts/2013-01-27-hexbuffer.html), I've just finished my second module: an inexpensive headphone and line-level amplifier.

The humble purpose of this module is to bring signals from eurorack levels to both headphones and other audio equipment.

## Electronic Design

Like my previous multiple module, the design doesn't contain anything novel or at all difficult.  It's a totally standard amplifier, with levels set to the eurorack standard.  One thing I was careful to do was use a socketed op-amp with a standard dual pinout, so users with [discerning tastes](http://tangentsoft.net/audio/opamps.html) can replace the "jellybean" LF353 with a more "smooth" or "laid back" chip.

For the board layout I again used [EAGLE](http://www.cadsoftusa.com/), which is free for non-commercial use.  This time, most of the design was SMT, with the exception of the previously mentioned op-amps.

Again, all of the design files for the project, including a list of all parts required, are [on GitHub](https://github.com/russellmcc/eurorack_headphones) under a permissive license.

## Board Fabrication

![PCB](/images/headphone_board.jpg)

Like last time, The board was fabricated by [OSH Park](http://oshpark.com/).

I'll write a future post on the SMT soldering technique I used, which involves lasers, toaster ovens, and a home-made stencil rig that I designed with my friend [Brendan](https://twitter.com/brendan0powers).

## Front Panel

![completed buffer](/images/headphone_front.jpg)

Again, the panel was lasercut out of acrylic from [Inventables](https://www.inventables.com/) at a [local lasercutting shop](http://dangerawesome.co/).  The design files were done in InkScape.
