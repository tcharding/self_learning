ten_things = 'Apples Oranges Pears Crows Telephone Light Sugar'

print("Wait there is not 10 things in tha list, let us fix it.")

stuff = ten_things.split(' ')
more_stuff = ['Day', 'Night', 'Song', 'Frisbee', 'Corn', 'Banana', 'Girl']

while len(stuff) != 10:
    next_one = more_stuff.pop()
    print('Adding : ', next_one)
    stuff.append(next_one)
    print('There is now %d items.' % len(stuff))

print('There we go: ', stuff)
print('Let us do some things with stuff.')

print(stuff[1])
print(stuff[-1])
print(stuff.pop())
print(' '.join(stuff))
print('#'.join(stuff[3:5]))
