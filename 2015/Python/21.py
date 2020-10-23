from dataclasses import dataclass

WEAPONS = [
     (8, 4, 0),
    (10, 5, 0),
    (25, 6, 0),
    (40, 7, 0),
    (74, 8, 0)]

ARMOR = [
     (13, 0, 1),
     (31, 0, 2),
     (53, 0, 3),
     (75, 0, 4),
     (102, 0, 5)]

RINGS = [
    (25, 1, 0),
    (50, 2, 0),
    (100, 3, 0),
    (20, 0, 1),
    (40, 0, 2),
    (80, 0, 3)]

def simulate(player, boss):
    while True:
        d = player.dmg - boss.armor
        boss.hp -= d if d > 1 else 1
        if boss.hp <= 0:
            return True
        d = boss.dmg - player.armor
        player.hp -= d if d > 1 else 1
        if player.hp <= 0:
            return False

@dataclass(frozen=False)
class Character:
    hp: int
    dmg: int
    armor: int

best = float('inf')
worst = 0
for w in WEAPONS:
    for a in [(0, 0, 0)] + ARMOR:
        for r in [(0, 0, 0)] + RINGS:
            for s in [(0, 0, 0)] + RINGS:
                if s != (0, 0, 0) and s == r:
                    continue
                BOSS = Character(103, 9, 2)
                PLAYER = Character(100, 0, 0)
                cost = w[0] + a[0] + r[0] + s[0]
                PLAYER.dmg = w[1] + a[1] + r[1] + s[1]
                PLAYER.armor = w[2] + a[2] + r[2] + s[2]
                outcome = simulate(PLAYER, BOSS)
                if outcome and cost < best:
                    best = cost
                    continue
                if not outcome and cost > worst:
                    worst = cost

print('Part 1:', best)
print('Part 2:', worst)
