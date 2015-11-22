i = 0
numbers = []

def add_nums(max):
    i = 0
    while i < max:
        print("At the top i is %d" % i)
        numbers.append(i)
        i += 1

    for i in range(0, max):
        print("it works with %d also" %i)
#    print("Numbers now: ", numbers)
#    print("At the bottom i is %d" % i)

add_nums(10)

print("The numbers: ")
for num in numbers:
    print(num)
