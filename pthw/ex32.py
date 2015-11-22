# lists
count = [1, 2, 3, 4, 5]
fruits = ['apples', 'oranges', 'pears', 'grapes']
change = [1, 'pennies', 2, 'dimes', 3, 'quarters']

# iterate list
for number in count:
    print ("This is count %d" % number)

for fruit in fruits:
    print("A fruit of type: %s" % fruit)

# mixed list (use %r)
for i in change:
    print("I got %r" % i)

# build list
elements = []
for i in range(0, 6):
    print("Adding %d to the list." % i)
    # append
    elements.append(i)

# print new list
for i in elements:
    print("Element was: %d" % i)
