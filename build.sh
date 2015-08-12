#!/bin/sh

stack clean
stack install
d12frosted-github-io clean
git clone -b master git@github.com:d12frosted/d12frosted.github.io.git _site
d12frosted-github-io build
cd _site
git add --all
git commit -m "snapshot $(date '+%d/%m/%y %H:%M:%S')"
git push -u origin master
