class Card:
    def __init__(self, rank):
        self.rank = rank
    def __eq__(self, other):
        return isinstance(other, Card) and self.rank == other.rank
    def __lt__(self, other):
        return self.rank < other.rank
    def __repr__(self):
        return f"Card({self.rank})"
cards = [Card(3), Card(1), Card(2)]
print(sorted(cards))
print(Card(2) == Card(2), Card(2) == 2)
print(Card(1) < Card(2), Card(3) > Card(2))
print(max(cards))
