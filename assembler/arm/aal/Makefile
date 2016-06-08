CFLAGS = -g -Wall -pedantic
EXE = first printv sort find printtab queue jump-trig

.PHONY: all

all: $(EXE)

trig: jump-trig.o sine.o cosine.o tangent.o
	gcc $(CFLAGS) -o $@ $+ -lm

tan: tangent.o
	gcc $(CFLAGS) -o $@ $+

cos: cosine.o
	gcc $(CFLAGS) -o $@ $+

sin: sine.o
	gcc $(CFLAGS) -o $@ $+

queue: queue.o printv.o
	gcc $(CFLAGS) -o $@ $+

find: sort-and-find.o bubblesort.o binsearch.o printtab.o
	gcc $(CFLAGS) -o $@ $+

sort: tst-bubblesort.o bubblesort.o printv.o
	gcc $(CFLAGS) -o $@ $+

printtab: tst-printtab.o printtab.o
	gcc $(CFLAGS) -o $@ $+

printv: tst-printv.o printv.o
	gcc $(CFLAGS) -o $@ $+

first: first.o
	gcc $(CFLAGS) -o $@ $+

%.o: %.s
	gcc -g -c $+

clean:
	rm -vf $(EXE) *.o a.out .\#*

# Rsync targets

EROS='eros:/home/tobin/build/github/self_learning/assembler/arm/aal/'

get:
	rsync -avuzb --exclude '*~' --exclude '#*' --exclude '*.o' $(EROS) .

put:
	rsync -avuzb --exclude '*~' --exclude '#*' --exclude '*.o' . $(EROS)

sync: get put

