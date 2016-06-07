all: first

first: first.o
	gcc -o $@ $+

first.o : first.s
	as -o $@ $<

clean:
	rm -vf first *.o a.out

# Rsync targets

EROS='eros:/home/tobin/build/github/self_learning/assembler/arm/aal/'

get:
	rsync -avuzb --exclude '*~' --exclude '#*' --exclude '*.o' $(EROS) .

put:
	rsync -avuzb --exclude '*~' --exclude '#*' --exclude '*.o' . $(EROS)

sync: get put

