#!/bin/bash

cat resume | ./texify.coffee | pandoc -o resume-body.tex
cat resume-pre.tex resume-body.tex resume-post.tex > resume-full.tex
xelatex resume-full.tex
