#!/bin/bash

cat resume | npx -p coffeescript coffee ./texify.coffee | stack exec -- pandoc -o resume-body.tex
cat resume-pre.tex resume-body.tex resume-post.tex > resume-full.tex
xelatex resume-full.tex
