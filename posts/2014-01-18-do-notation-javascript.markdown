---
layout: post
title: Haskell-style Do Notation in Javascript
tags: blog
score: 3
teaser: 
img: /images/fantasydo_banner.png
imgalt: rtti
---

A serious problem with callback-based asynchronous environments like browser-based JavaScript or [node.js](nodejs.org) is the verbosity required to chain together multiple asynchronous operations (colloquially known as ["callback hell"](http://elm-lang.org/learn/Escape-from-Callback-Hell.elm)).  Recently, I was delighted to come accross a proposed solution that looked syntactically a lot like Haskell's [do notation](http://en.wikibooks.org/wiki/Haskell/do_Notation) for promises.  It turns out the same trick can be used to implement something similar to do notation for a wide class of monads.  First, though, a bit of background on promises:

</div><div class="post">

## What is a promise?

One of the more interesting recent developments in the world of modern JavaScript is the rise in popularity of the [Promises/A+ specification](http://promises-aplus.github.io/promises-spec/), an attempt to address the common problem of callback hell in web apps by reifying asynchronous actions into objects called "promises".

The specification suggests, rather than directly taking callbacks into an asynchronous computation, the function that starts the computation should return a "Promise" object that can have callbacks attached to it.  This turns out to be much more easily composable than calback-based functions.  The strategy of reification of control flow is one more closely associated with "academic" language like Scala than "workaday" languages like JavaScript, so it's a bit surprising that promises have gained as much traction as they have.  The [next version of the browser DOM](http://dom.spec.whatwg.org/) will even include wide-spread Promise support.

As noted by [Brian McKenna](http://brianmckenna.org/blog/category_theory_promisesaplus), promises as defined by the spec look similar[[1]](#foot1) to [Monads](http://www.haskell.org/tutorial/monads.html).  This observation indirectly led to a train-wreck flame war on a [GitHub Issue](https://github.com/promises-aplus/promises-spec/issues/94).  At one point, a functional-programming skeptic claimed that to use the principle of generalization was to "ignore reality in favor of typed-language fantasy land".

</div><div class="post">

## What about Do Notation?

The one positive thing to come out of the flame-war was a specification for various simple algebraic constructs in JavaScript, aptly called ["Fantasy Land"](https://github.com/fantasyland/fantasy-land).  I thought I'd join in the fun by providing a sort of "do notation" for Fantasy Land monads using [harmony generators](http://wiki.ecmascript.org/doku.php?id=harmony:generators) and ideas from Mozilla's [task.js](http://taskjs.org/).  Code and examples are [available on GitHub](http://github.com/russellmcc/fantasydo) under the CC0 1.0 license.

This was my first time working with harmony features of JavaScript.  Andy Wingo wrote an [excellent article on the design of generators](http://wingolog.org/archives/2013/02/25/on-generators), which did a great job of explaining why various choices were made.

One feature I would have really enjoyed while writing this library is a way of cloning a generator's iterator.  This would allow easy support for this "Do notation" trick for branching monads (like the List monad).  Wingo writes that the main reason for not allowing resumable generators are the "allocation cost".  I don't quite understand this when we already have external iterators, because there's no fundamental reason you'd have to do any extra allocations until a user actually starts cloning the iterator.  If anyone could explain this in the comments, I'd be really grateful.

</div><div class="post">


## Footnotes

------

<a id="foot1"></a>

## 1

Promises/A+ defines one operation for promises, called `then`.  Brian McKenna points out that this is similar to a "return-type" overload of `fmap` and `bind`.  In fact, ignoring the error path, the Promises/A+ `then` is equivalent to `bind` in the sense that given one, you can write the other.

Promises/A+ are actually a little more expressive than a monad, in that they have rich operations for error catching in addition to the operations required by the monad structure.  One of the authors of the specification [claims](https://github.com/promises-aplus/promises-spec/issues/94#issuecomment-16176966) that to ignore or abstract away this extra structure is to "miss the point of promises".  Regardless, the specification seems to share much with [Scala Futures](http://www.scala-lang.org/api/current/index.html#scala.concurrent.Future), and whether the authors like it or not, promises as specified are monads.
