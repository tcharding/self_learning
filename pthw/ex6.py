# Joke
x = "There are %d types of people." % 10
binary = 'binary'
do_not = "don't"
y = "Those who know %s and those who %s." % (binary, do_not)

print (x)
print (y)

print ("I said: %r." % x)
print ("I also said: '%s'." % y)

hilarious = False
evaluate = "Isn't that joke so funny? %r"

print (evaluate % hilarious)

w = "This is the left side ..."
e = "a string with a right side"

print (w + e) # concatenate strings
print (w, e) # print things seperated by space
