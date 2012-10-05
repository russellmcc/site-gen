---
layout: post
title: Pareidoloop
tags: blog
score: 12
teaser: I made some modifications to a face-generation toy called <a href="http://russellmcc.github.com/pareidoloop">pareidoloop</a>
img: /images/plface0.png
imgalt: Creepy face generated in pareidoloop
---

## The ghost in the machine has a face

<img alt="creepy face 1" src="/images/plface0.png">


Internet citizen Philip McCarthy created [pareidoloop](http://iobound.com/2012/08/pareidoloop/), a "face" generator that was simply a random-image generator hooked up in a loop to a face recognizer program.  Things that look more like faces to the computer are kept, and things that look less like faces are discarded.  After a long time, something vaguely representing a distorted face appears.

<img alt="creepy face 2" src="/images/plface1.png">


The images are visually very striking, and there's always something a little cosmically creepy about computer-generated art.

I made a few modifications to the original formula - the images are now in color, you can work at larger canvas sizes (although this would be slower), and there's a "simulated annealing" process instead of straight randomness to speed the process up while avoiding local maxima.



<img alt="creepy face 3" src="/images/plface2.png">


</div><div class="post">

## The code

Avaiable [on GitHub](https://github.com/russellmcc/pareidoloop).