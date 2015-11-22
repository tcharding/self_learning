from sys import argv

script, infile = argv

def print_all(f):
    print (f.read())

def rewind(f):
    f.seek(0)

def print_line(n, f):
    print (n, f.readline(), end='')

f = open(infile)
print ("%s is:" % infile)
print (f.read())

rewind(f)
print ("Print 2 lines")

line = 1
print_line(line, f)
line += 1
print_line(line, f)

