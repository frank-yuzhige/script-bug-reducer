set -e;
gcc -fgnu-tm -w -O1 -c bug.c;
gcc -c good.c;
gcc good.o bug.o -o main;