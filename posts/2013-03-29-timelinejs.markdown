---
layout: post
title: timeline.js
tags: blog
score: 1
teaser: 
img: /images/timelinejs.png
imgalt: timeline lines
---

<script src="http://code.jquery.com/jquery-1.9.1.min.js"></script>
<script src="http://code.jquery.com/jquery-migrate-1.1.1.min.js"></script>
<script src="https://www.russellmcc.com/timelinejs/master/timeline.min.js"></script>
<div id="timeline" style="height:200px;"></div>
<script>
  var p = [];
  var n = 35;
  for(var i = 0; i < n; ++i) {
      var h = 0.5 + 0.3 * Math.sin(i/n * Math.PI) * Math.cos(i * Math.PI);
      p.push([i/n, h]);
  }
  $('#timeline').timeline({
     points: p
  });
</script>

</div><div class="post">

[Timeline.js](http://russellmcc.com/timelinejs/) is a jquery plug-in/html5 widget that allows you to add mult-touch and mouse enabled line editing to your web app.  Retina screens welcome.

I plan to use it in future sequencer/synth projects for envelope editing.

Inspired by [duration.cc](http://www.duration.cc/)