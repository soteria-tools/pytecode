xs = [5, 2, 8, 1, 9]
print(sorted(xs), xs)
print(sorted(xs, reverse=True))
words = ["banana", "kiwi", "apple"]
print(sorted(words))
print(sorted(words, key=len))
pairs = [(1, "b"), (2, "a"), (1, "a")]
print(sorted(pairs))
print(sorted(pairs, key=lambda p: p[1]))
print(min(words, key=len), max(words, key=len))
