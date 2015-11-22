#from Room import start
from sys import exit

class Game(object):

    def __init__(self, title):
        self.title = title
        self.rooms = ['front', 'back']
        self.animals = ['bear', 'tiger', 'cat']
        self.actions = ['eating', 'sleeping', 'sitting quietly']
        
    def play(self):
        print("game yeah %s" % self.title)
        exit(1)
#        print("Welcome to %s" % self.title)
#        room = Room(self.rooms(randint(0 - len(self.rooms)-1)))
#        room.start
#        animal = self.animals(randint(0 - len(self.animals)-1))

#game = Game("inside module")
#game.play()
