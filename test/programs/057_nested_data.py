data = {"users": [{"name": "ann", "age": 30}, {"name": "bob", "age": 25}]}
for u in data["users"]:
    print(u["name"], u["age"])
data["users"][1]["age"] += 1
print(data["users"][1])
matrix = [[1, 2], [3, 4]]
print([sum(row) for row in matrix])
print(sum(matrix, []))
