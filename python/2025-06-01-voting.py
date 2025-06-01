import random
import uuid

class BallotBox:
    def __init__(self, candidates: list[str]):
        self.candidates = candidates
        self.votes = {c.id: 0 for c in candidates}

    def vote(self, uuids: list[str]):
        for u in set(uuids):
            self.votes[u] += 1

    def get_winners(self):
        max_votes = max(self.votes.values())
        return [c for c in self.candidates if self.votes[c.id] == max_votes]
    
    def get_scores(self):
        return {c.id: self.votes[c.id] for c in self.candidates}
    
class Candidate:
    def __init__(self, name: str):
        self.name = name
        self.id = uuid.uuid4()

    def __str__(self):
        return f"{self.name} ({self.id})"
    
    def __repr__(self):
        return f"{self.name} ({self.id})"


def main():
    candidates = [Candidate("John"), Candidate("Jane"), Candidate("Jim")]
    ballot_box = BallotBox(candidates)

    for _ in range(100000):
        likes = [random.random() < 0.5 for _ in range(3)]
        ballot_box.vote([c.id for c, i in zip(candidates, range(3)) if likes[i]])

    print(ballot_box.get_scores())

if __name__ == "__main__":
    main()