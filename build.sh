#!/bin/sh

stack clean
stack install
d12frosted-github-io clean
d12frosted-github-io build
cd _site
git init
git remote add origin git@github.com:d12frosted/d12frosted.github.io.git
git fetch origin
git checkout site
git add .
git commit -m "snapshot $(date '+%m/%d/%y %H:%M')"
git push origin site
