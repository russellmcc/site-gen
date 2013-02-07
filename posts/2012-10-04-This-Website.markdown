---
layout: post
title: Project Log: This Website
tags: blog
score: 1
teaser: I auto-generate this site from <a href="http://daringfireball.net/projects/markdown/">markdown</a> code using Haskell.  The generation code includes a custom CSS processor to output modern, reactive CSS.
img: /images/code_hs.png
imgalt: Haskell code in Emacs.
---

## Code

Code is available [on GitHub](http://www.github.com/russellmcc/site-gen)

The code is based on a Haskell blog framework called [Hakyll](http://jaspervdj.be/hakyll/), although there is a CSS processor entirely of my own design.

If you're interested in how it works, most of the guts are in four short files: 

------------

[site.hs](https://github.com/russellmcc/site-gen/blob/master/site.hs)
~   contains the basic structure of the site.  Hakyll 4 has finally removed all usage of the cumbersome [arrows](http://www.haskell.org/arrows/) in its API, which are not my favorite abstraction for computation.

------------

[cssTokenizer.hs](https://github.com/russellmcc/site-gen/blob/master/cssTokenizer.hs) and [cssProcess.hs](https://github.com/russellmcc/site-gen/blob/master/cssProcess.hs)
~   contain the custom CSS processor used to style the site.  Since these files don't make any reference to the Hakyll framework, I was more free to write in a style I prefer. For cssProcess.hs I used [SHE](https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/) for the applicative idiom brackets.  More syntactic sugar, please!

------------

[frameless.scss](https://github.com/russellmcc/site-gen/blob/master/stylesheets/frameless.scss)
~   includes the CSS used to style the site, heavily based on the ["frameless grid"](http://framelessgrid.com/) design concept by Joni Korpi

</div><div class="post">

## Design

Most of the initial design concept was done by my friend [Kate Lindsay](http://www.kate-lindsay.com), although due to time constraints I had to finalize the design and do the mobile layout.  Clearly, I'm not much of a designer :-).