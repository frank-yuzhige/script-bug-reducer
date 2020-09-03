#!/bin/bash

if cat out | grep bug >/dev/null; then
  echo "Miscompilation detected";
  exit 1;
else 
  echo "Good!"
  exit 0;
fi;