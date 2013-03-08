---
layout: post
title: web-based soundfx generator
tags: blog
score: 1
teaser: 
img: /images/soundfxbanner.png
imgalt: HTML oscilloscope UI.
---

[Here](http://russellmcc.com/soundfxweb/) is [another emulation](http://www.ghostfact.com/soundfx-machine) of the [Remco soundfx machine](http://www.youtube.com/watch?&v=kokFu2xZt44), this time for HTML5 audio.

![screen shot](/images/soundfx_ss.png)

The soundfx machine is a pretty weird toy.  It's basically a [noise generating chip](http://en.wikipedia.org/wiki/Texas_Instruments_SN76477) in a box, with each pin pulled out to a knob on the front panel.  It's the same chip that was used to create the [space invaders](http://www.youtube.com/watch?v=KgJmRZ_oNmI) sounds, among others classic video games.  I still don't know who thought it was a good idea to give this to children!

I had a lot of fun playing with my previous emulation, so I thought I'd build a web-based one as a way to learn about the current state of Web Audio.  The main audio coding only took a few hours, which is a testament to how well thought out and efficiently designed the Web Audio API is.  I ran into a few nasty bugs (the most embarassing was [this](https://code.google.com/p/chromium/issues/detail?id=82795), which has existed since May 2011 on both Chrome and Safari), but basically it was smooth sailing.

Check out the [coffeescript](http://www.coffeescript.org) code [on github](https://github.com/russellmcc/soundfxweb)!

