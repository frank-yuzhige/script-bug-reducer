git clone https://gitlab.doc.ic.ac.uk/lab1920_autumn/pintos_39.git
dd=$PWD
cd pintos_39/src/utils
make
export PATH=$PATH:$PWD
cd ../devices
cp -f $dd/timer.c timer.c
cp -f $dd/timer.h timer.h
mkdir build
cd build
cp -f $dd/build1.sh build.sh
cp -f $dd/test.sh test.sh

stack run -- -path=./build.sh -ipath=./test.sh -cvar=gcc -xcvar=gcc-8 -recover 