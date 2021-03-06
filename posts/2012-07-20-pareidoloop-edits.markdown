---
layout: post
title: Pareidoloop In Color
tags: blog
score: 4
teaser: I made some modifications to a face-generation toy called <a href="http://russellmcc.github.com/pareidoloop">pareidoloop</a>
img: /images/plfacebanner.png
imgalt: Creepy face generated in pareidoloop
---

## The ghost in the machine has a face

![creepy face 1](/images/plface0.png)

Internet citizen Philip McCarthy created [pareidoloop](http://iobound.com/2012/08/pareidoloop/), a "face" generator that was simply a random-image generator hooked up in a loop to a face recognizer program.  Things that look more like faces to the computer are kept, and things that look less like faces are discarded.  After a long time, something vaguely representing a distorted face appears.

![creepy face 1](/images/plface1.png)

The images are visually very striking, and there's always something a little cosmically creepy about computer-generated art.

I made a few modifications to the original formula - the images are now in color, you can work at larger canvas sizes (although this would be slower), and there's a "simulated annealing" process instead of straight randomness to speed the process up while avoiding local maxima.

![creepy face 3](/images/plface2.png)

</div><div class="post">

## The code

Avaiable [on GitHub](https://github.com/russellmcc/pareidoloop).