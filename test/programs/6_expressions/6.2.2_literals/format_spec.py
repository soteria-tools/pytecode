pi = 3.14159265
print(f"{pi:.2f}")
print(f"{pi:10.3f}|")
print(f"{42:5d}|")
print(f"{42:<5}|{42:>5}|{42:^6}|")
print(f"{42:05d}")
print(f"{255:x} {255:X} {255:o} {255:b}")
print(f"{1234567:,}")
print(f"{0.25:%}")
print("{}-{}".format(1, 2))
print("{1}{0}".format("a", "b"))

# sign options: + always, space for non-negative, - (default) only negatives
print(f"{42:+} {-42:+} {42: } {-5: } {0:+}")
# thousands separators , and _ (every 3 for decimals/floats, every 4 for x/b/o)
print(f"{1000000:,} {1234567:_} {1234.5:,.1f} {-1234567:,} {0xABCDEF:_X}")
# alternate form # adds the base prefix; combines with zero-padding and sign
print(f"{255:#x} {255:#X} {8:#o} {5:#b}")
print(f"{255:#010x} {-255:#010x} {42:08} {-42:08} {42:+08}")
# exponential and general presentations
print(f"{3.14159:.2e} {3.14159:.2E} {1234.5:g} {0.0001234:.3g} {123456789:.3g}")
# default float presentation is the shortest round-trip repr
print(f"{0.1 + 0.2} {2 ** 0.5}")
print(f"{3.5:%} {-3.5:+.1%}")
# alignment combined with alternate form / sign / fill
print(f"{255:>#10x}|{42:*^+10}|")
