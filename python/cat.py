from sys import argv
from os.path import exists

script, inf, outf = argv

data = open(inf).read()
fh = open(outf, 'w')
fh.truncate()
fh.write(data)
fh.close()
