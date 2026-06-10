class Celsius:
    def __init__(self, deg):
        self._deg = deg
    @property
    def fahrenheit(self):
        return self._deg * 9 / 5 + 32
    @fahrenheit.setter
    def fahrenheit(self, f):
        self._deg = (f - 32) * 5 / 9
c = Celsius(100)
print(c.fahrenheit)
c.fahrenheit = 32
print(c._deg)
print(Celsius(0).fahrenheit)
