cars = 100
spaces = 4.0
drivers = 30
passengers = 90
cars_not_driven = cars - drivers
cars_driven = drivers
carpool = cars_driven * spaces
average_passengers_per_car = passengers / cars_driven

print ("There are", cars, " cars available");
print ("There are only", drivers, " drivers available")
