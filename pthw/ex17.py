from sys import argv
from os.path import exists

script, from_file, to_file = argv

# can be done on one line
infh = open(from_file)
indata = infh.read()

print ("The input file is %d bytes long" % len(indata))

print ("Does the output file exist? %r" % exists(to_file))
print ("Ready, hit ENTER to continue.")
input()

outfh = open(to_file, 'w')
outfh.write(indata)

print ("Alright, all done.")
infh.close()
outfh.close()
