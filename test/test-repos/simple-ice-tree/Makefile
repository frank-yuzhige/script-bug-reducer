CC=gcc
DEPS=1.h 2.h 3.h 4.h 5.h 6.h 7.h 8.h 9.h 10.h
all: main

main: 1.o 2.o 3.o 4.o 5.o 6.o 7.o 8.o 9.o 10.o
	$(CC) $? -o $@

# %.o: %.c 
# 	$(CC) -c -o $@ $<

1.o: 1.c 
	$(CC) -c -o $@ $<

2.o: 2.c 
	$(CC) -c -o $@ $<

3.o: 3.c 
	$(CC) -c -o $@ $<

4.o: 4.c 
	$(CC) -c -o $@ $<

5.o: 5.c 
	$(CC) -c -o $@ $<

6.o: 6.c 
	$(CC) -c -o $@ $<

7.o: 7.c 
	$(CC) -c -o $@ $<

8.o: 8.c 
	$(CC) -c -o $@ $<

9.o: 9.c 
	$(CC) -c -o $@ $<

10.o: 10.c 
	$(CC) -c -o $@ $<

clean:
	rm -f *.o main

.PHONY: all clean
