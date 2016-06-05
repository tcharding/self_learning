all: first

first: first.o
	gcc -o $@ $+

first.o : first.s
	as -o $@ $<

clean:
	rm -vf first *.o

# Rsync targets

EROS='eros:/home/tobin/build/github/self_learning/assembler/arm/aal/'

get:
	rsync -avuzb --exclude '*~' --exclude '#*' $(EROS) .

put:
	rsync -avuzb --exclude '*~' --exclude '#*' . $(EROS)

sync: get put

