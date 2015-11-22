# functions
def print_two( *args ):
    arg1, arg2 = args
    print ("arg1: %r, arg2: %r" % (arg1, arg2))

# *args is pointless
def print_two_again(arg1, arg2):
    print ("arg1: %r, arg2: %r" % (arg1, arg2))

# one argument
def print_one(arg):
    print ("arg: %r" % arg)

# no arguments
def print_none():
    print ("we got nothin'.")

print_two("Tobin", "Harding")
print_two_again("Tobin", "Harding")
print_one("one")
print_none()
