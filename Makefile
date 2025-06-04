CC ?= gcc
CFLAGS ?= -O2 -Wall

nagare: nagare.c
	$(CC) $(CFLAGS) nagare.c -o nagare

tester: tester.c
	$(CC) $(CFLAGS) tester.c -o tester -lpthread -lm

ring: ring.c
	$(CC) $(CFLAGS) ring.c -o ring -lpthread

.PHONY: clean
clean:
	rm -f nagare tester ring
