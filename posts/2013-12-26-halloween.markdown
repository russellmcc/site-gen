---
layout: post
title: Collaborative Remixing at a Halloween Party
tags: blog
score: 3
teaser: Collaborative Remixing at a Halloween Party
img: /images/halloween_banner.png
imgalt: rtti
---

For the last few years, my roommates and I have hosted costumed dance parties every Halloween weekend.  This year, we decided to add a twist by allowing guests with smart phones to take part in remixing the music while they dance!

To accomplish this, I built a web server in [node](www.nodejs.org) to serve a web page with some simple controls with the most common remixing tasks - a knob to filter high and low frequencies, an X-Y pad to create stutter effects, and most critically, a button to sound the all important [air-horn](http://blog.dubspot.com/contraption/).  These controls were adapted from a jQuery plug-in called [jQuery-kontrol](https://github.com/russellmcc/jQuery-Kontrol) by GitHub user [`aterrien`](https://github.com/aterrien).

</div><div class="post">

![example](/images/halloween_example.png)

</div><div class="post">

Each browser control (i.e., knob, pad, or button) communicated over WebSockets (via [socket.io](http://socket.io/)) to the server, which then translated the message into an [OSC](http://opensoundcontrol.org/) message, which was in turn sent to the effects hosted in the DJ software.  Changes to the positions of the controls were broadcast to all other clients via WebSockets so that guests could watch as others moved the controls.  Also, UDP messages were sent from the server to an Arduino that my roommate [Tamas](http://tamas-szalay.squarespace.com/) programmed with amazing lighting effects.

We printed QR code posters linking to the public URL of the site, and included a simple password authorization lest mischeivous internet users stumble upon the site.  I was impressed with the responsiveness of websockets over the internet: latency was low enough for the remixing tools to be a lot of fun.

Code for the project is available [on GitHub](https://github.com/russellmcc/halloweenparty2013) under the CC0 license, and while it probably won't be directly applicable for your project, I hope it can serve as starting-off point for similar parties.