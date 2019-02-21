#!/usr/bin/env bash
stack run build
gh-pages -d _site -r git@github.com:russellmcc/russellmcc.github.com.git -b master