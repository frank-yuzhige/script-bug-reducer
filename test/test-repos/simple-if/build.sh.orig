#!/bin/bash

set -e;

./run.sh target1
./run.sh target2
./run.sh target3
./run.sh target4
if [[ $1 == "foo" ]]; then
  echo "compiling foo stuff"
  ./run.sh targetFoo1
  ./run.sh targetFoo2
elif [[ $1 == "bar" ]]; then 
  echo "compiling bar stuff"
  ./run.sh targetBar
else
  echo "compiling buggy stuff"
  ./run.sh target5
  ./run.sh target6
  ./run.sh bug
  ./run.sh target7
fi;

./run.sh target8

