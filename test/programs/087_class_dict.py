class Config:
    a = 1
    b = 2
    def method(self):
        pass
names = sorted(k for k in Config.__dict__ if not k.startswith("__"))
print(names)
c = Config()
c.x = 10
c.y = 20
print(sorted(c.__dict__.items()))
c.__dict__["z"] = 30
print(c.z)
print(vars(c) == c.__dict__)
