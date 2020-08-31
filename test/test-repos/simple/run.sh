#!/bin/bash

if [[ $1 == "bug" ]]; then
  >&2 echo "You have triggered a BUG!" 
  exit 1
else 
  exit 0
fi