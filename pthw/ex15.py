from sys import argv

script, filename = argv

f = open(filename)

print ("Outputting %r:" % filename)
print (f.read())
f.close()

#print ("Enter filename to display it:")
#otherfile = input("> ")

#f = open(otherfile)
#print (f.read())
#f.close()
