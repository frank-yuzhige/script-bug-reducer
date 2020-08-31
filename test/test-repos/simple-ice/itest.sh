#!/bin/bash

sh ./build.sh 2> err.txt
cat err.txt | grep "internal compiler error"
