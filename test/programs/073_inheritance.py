class Animal:
    def __init__(self, name):
        self.name = name
    def speak(self):
        return self.name + " makes a sound"
    def intro(self):
        return "I am " + self.name
class Dog(Animal):
    def speak(self):
        return self.name + " barks"
d = Dog("rex")
print(d.speak())
print(d.intro())
print(isinstance(d, Dog), isinstance(d, Animal), isinstance("x", Animal))
