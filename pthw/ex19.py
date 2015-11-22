# variable scope

def cheese_and_crackers(ncheese, nboxes):
    print ("You have %d cheeses!" % ncheese)
    print ("And %d boxes" % nboxes)

print ("call function")
cheese_and_crackers(10, 20)

print ("OR, pass by value")

cheese = 3
boxes = 1
cheese_and_crackers(cheese, boxes)

print ("arguments can contain expressions")
cheese_and_crackers(cheese + 1, boxes)
