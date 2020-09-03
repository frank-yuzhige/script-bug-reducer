#!/bin/bash


echo "" > out
for f in $@; do
  cat $f.o >> out;
done;
