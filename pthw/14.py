from sys import argv

script, user, age = argv
prompt = 'Answer: '

print ("Hi %s, I'm the %s script." % (user, script))
#if age < 18 print ("you are just a wee baby")
print ("I'd like to ask you a few questions.")
print ("Do you like me %s?" % user)
likes = input(prompt)

print ("Where do you live %s?" % user)
lives = input(prompt)

print ("What kind of computer do you have?")
computer = input(prompt)

print ("""
Alright, so you said %r about liking me.
You live in %r. Not sure where that is.
And you have a %r computer, Nice.
""" % (likes, lives, computer))
