all: ftoc ctof

ftoc: ftoc.o
	gcc -o $@ $+

ftoc.o : ftoc.s
	as -o $@ $<

ctof: ctof.o
	gcc -o $@ $+

ctof.o : ctof.s
	as -o $@ $<

clean:
	rm -vf ftoc ctof *.o

# Rsync targets

EROS='eros:/home/tobin/build/github/self_learning/assembler/arm/aal/cels-fahr/'

get:
	rsync -avuzb --exclude '*~' --exclude '#*' $(EROS) .

put:
	rsync -avuzb --exclude '*~' --exclude '#*' . $(EROS)

sync: get put

