---
layout: post
title: Project Log: Eurorack Buffered Multiple
tags: blog
score: 1
teaser: I finally finished my first-ever DIY eurorack synthesizer module - the ever-humble buffered multiple.  
img: /images/buffer_insitu.JPG
imgalt: Eurorack Buffered Multiple.
---

## background

[Modular synthesizers](http://www.youtube.com/watch?v=GCyiDaM3boc) are musical instruments made out of removable, replacable building blocks.  Each module in a synth is able to interact with all the others to create unique sonic landscapes.  While precise, [engineered](http://www.soundonsound.com/sos/allsynthsecrets.htm) sounds are possible, the tactile physical nature of the system tends to encourage organic, creative exploration of the rich parameter space.

I've been interested in designing modules for this ecosystem for a long time, and now I've finally found a comfortable design and fabrication stack for it.  As a proof of concept, I've created a very simple type of module called a buffered multiple.

A multiple is just a signal splitter, allowing the user to connect one output into multiple inputs of other modules.

## Electronic Design

The electronics of a multiple are quite simple, really just consisting of a single op amp stage per output.  There are two inputs, each providing three buffered clones of the input signal.  If there is no jack plugged into the second input, all the outputs follow the first input signal.

For the board layout I used [EAGLE](http://www.cadsoftusa.com/), which is free for non-commercial use and does the job.  The learning curve was quite difficult, and I probably wouldn't have been able to use it without the support of some experienced friends.  While there are other electronic CAD choices out there, none are nearly as ubiquitous for hobbiests.

All of the design files for the project, including a list of all parts required, are [on GitHub](https://github.com/russellmcc/hexbuffer) under a permissive license.

## Board Fabrication

![PCB](/images/buffer_board.JPG)\

The board was fabricated by [OSH Park](http://oshpark.com/), which was quite affordable at $6 per board.  A nice feature of OSH Park is that they support EAGLE files, so no export to the more-standard gerber files is needed.

I hand assembled the boards with parts from [mouser](http://www.mouser.com/ProjectManager/ProjectDetail.aspx?AccessID=8b942c2668).

## Front Panel

![completed buffer](/images/buffer_insitu.JPG)\

The panel was lasercut out of acrylic from [Inventables](https://www.inventables.com/) at a [local lasercutting shop](http://dangerawesome.co/).  The design files were done in InkScape.  
