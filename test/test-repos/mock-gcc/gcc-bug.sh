#!/bin/bash

if [[ $1 == "bug" ]]; then
  echo "bug" > $1.o
else
  echo $1 > $1.o
fi; 