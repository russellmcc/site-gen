---
layout: post
title: This Website
tags: portfolio
score: 12
teaser: I auto-generate this site from <a href="http://daringfireball.net/projects/markdown/">markdown</a> code using Haskell.  The generation code includes a custom CSS processor to output modern, reactive CSS.
img: /images/code_hs.png
imgalt: Haskell code in Emacs.
---

## Code

Code is available [on GitHub](http://www.github.com/russellmcc/site-gen)

The code is based on a Haskell blog framework called [Hakyll](http://jaspervdj.be/hakyll/), although there is a CSS processor entirely of my own design.

If you're interested in how it works, most of the guts are in four short files: 

<dl>

------------

<dt>[site.hs](https://github.com/russellmcc/site-gen/blob/master/site.hs)</dt>
<dd> contains the basic structure of the site.  Hakyll makes heavy use of [Arrows](http://www.haskell.org/arrows/) in its API, which are not my favorite abstraction for computation.</dd>

------------

<dt>[cssTokenizer.hs](https://github.com/russellmcc/site-gen/blob/master/cssTokenizer.hs) and [cssProcess.hs](https://github.com/russellmcc/site-gen/blob/master/cssProcess.hs)</dt>
<dd> contain the custom CSS processor used to style the site.  The style of these two files more closely matches the way I like to write Haskell.  I prefer applicatives to arrows or monads whenever possible. </dd>

------------

<dt>[frameless.scss](https://github.com/russellmcc/site-gen/blob/master/stylesheets/frameless.scss)</dt>
<dd> includes the CSS used to style the site, heavily based on the ["frameless grid"](http://framelessgrid.com/) design concept by Joni Korpi</dd>

</dl>

</div><div class="post">

## Design

Most of the initial design concept was done by my friend [Kate Lindsay](http://www.kate-lindsay.com), although due to time constraints I had to finalize the design and do the mobile layout.  I'm still very much in the early stages of learning about what works and what doesn't, so I'd appreciate any feedback or hints.