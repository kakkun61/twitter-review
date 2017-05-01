#! /usr/bin/env bash

docker run\
  --link twitter-review-db\
  -it\
  --name twitter-review\
  -v "$(pwd):/app"\
  -v "$HOME/.ssh:/ssh"\
  -p 3000:3000\
  fpco/stack-build:lts-8.12
