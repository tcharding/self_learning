class TheThing(object):

    def __init__(self):
        self.number = 0

    def some_function(self):
        print("I got called.")

    def add_me_up(self, more):
        self.number += more
        return self.number

# two different things
a = TheThing()
b = TheThing()

print(a.add_me_up(20))
print(a.add_me_up(20))
print(b.add_me_up(30))
print(b.add_me_up(30))

print(a.number)
print(b.number)

        
