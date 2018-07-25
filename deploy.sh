#!/usr/bin/env bash

stack build && stack exec site build
(cd _site/ && git status)
(cd _site/ && git add --all)
(cd _site/ && git commit -m "Update (`date '+%F %T %Z'`) [ci skip]")
(cd _site/ && git push origin master)
git status
git add _site/
git commit -m "Update _site (`date '+%F %T %Z'`) [ci skip]"
git push origin hakyll
