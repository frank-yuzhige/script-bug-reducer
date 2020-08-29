CC=gcc
DEPS=bug.h good.h
all: main

main: good.o bug.o
	$(CC) $? -o $@

good.o:
	$(CC) -c good.c 

bug.o:
	$(CC) -fgnu-tm -w -O1 -c bug.c

# %.o: %.c $(DEPS)
# 	$(CC) -fgnu-tm -w -O1 -c -o $@ $<

clean:
	rm -f *.o main

.PHONY: all clean
