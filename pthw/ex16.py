# file io
from sys import argv

script, filename = argv

print ("We're going to erase %r." % filename)
print ("If you do not want that, hit CTRL-C (^C).")
print ("If you do want that, hit ENTER.")

input("?")

print ("Opening the file ...")
target = open(filename, 'w')

print ("Truncating the file. Goodbye!")
target.truncate()

print ("Now I am going to ask you for three lines.")

line1 = input("line 1: ")
line2 = input("line 2: ")
line3 = input("line 3: ")

print ("I am going to write these lines to the file")

target.write(line1)
target.write("\n")
target.write(line2)
target.write("\n")
target.write(line3)
target.write("\n")

print ("And finally we close it.")
target.close()

f = open(filename)
print ("File %s now contains:" % filename)
print (f.read())
