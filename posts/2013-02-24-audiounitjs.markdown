---
layout: post
title: project log: audiounitjs
tags: blog
score: 1
teaser: One of the most frustrating, thankless, and time-consuming parts of audio plug-in development is creating the UI.  I've made a framework to allow audio programmers to use HTML and Javascript to quickly write plug-in UIs.  As a bonus, you can write your audio processing and UI code once, and then compile it as a Mac plug-in, a Mac standalone app, and an iOS app!
img: /images/fivescope.png
imgalt: HTML oscilloscope UI.
---

# Background

![HTML Oscilloscope](/images/fivescope.png)

[audiounitjs](https://www.github.com/russellmcc/audiounitjs) was borne out of frustration with two issues: audio plug-in UIs are annoying to write, and iOS audio programming is difficult even to those with much audio programming experience, due to poorly documented APIs.

In audiounitjs, you write a CoreAudio Audio Unit, and an HTML UI, and you get a Mac App, an iOS App, and a Audio Unit plug-in all using that UI, for free.

</div><div class="post">
# In Use

audiounitjs is simply a scaffolding script.  You create a simple configuration `.json` file, and then the script will produce an Xcode project filled with boilerplate.  Then, you can edit the `audio.cpp` file to create audio processing source, and also the `ui` folder to create the HTML UI.  Full documentation is available [on GitHub](https://www.github.com/russellmcc/audiounitjs)

</div><div class="post">
# Getting it

Install via npm with `npm install -g audiounitjs`.  I made a [quick screencast](http://youtu.be/tqxOLf8EmdU) of the install process.

</div><div class="post">
# Future Features

Currently, this is an extremely niche project, because it requires the user to be able to create Audio Unit C++ code.  In the future I'd like to relax this requirement by adding some sort of JavaScript audio API.  Perhaps the easiest way to do this would be to implement the WebAudio API for CoreAudio.  If I did that, the JavaScript code would control all of the audio processing, and I could add the browser as a fourth target.

Another feature that might be fun would be to automatically support OSC messages mapped to Audio Unit parameters.  This would be not difficult to implement and would allow the stand alone and iphone versions to be automated by something like [duration.cc](http://www.duration.cc/).