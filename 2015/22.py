from dataclasses import dataclass
from random import choice
from collections import namedtuple

@dataclass
class Character:
    hp: int
    dmg: int
    buffs: dict
    debuffs: dict
    mana: int = 0
    armor: int = 0

@dataclass
class Spell:
    name: str
    mana: int = 0
    dmg: int = 0
    buff: tuple = None
    debuff: tuple = None
    time: int = 0
    heal: int = 0


SPELLS = [
    Spell(name='Magic Missile', mana=53, dmg=4),
    Spell(name='Drain', mana=73, dmg=2, heal=2),
    Spell(name='Shield', mana=113, time=6, buff=(0, 0, 0, 7)),
    Spell(name='Poison', mana=173, time=6, debuff=(3, 0, 0, 0)),
    Spell(name='Recharge', mana=229, time=5, buff=(0, 0, 101, 0))
]

def simulate(player, boss, penalty=0):
    spend = 0
    while True:
        # Player turn
        if penalty:
            player.hp -= penalty
            if player.hp <= 0:
                return 0
        apply_debuffs(player, boss)
        if boss.hp <= 0:
            return spend
        ss = [spell for spell in SPELLS if spell.mana <= player.mana and
              spell.name not in player.buffs and
              spell.name not in boss.debuffs]
        if not ss:
            return 0
        s = choice(ss)
        spend += apply_spell(player, boss, s)
        if boss.hp <= 0:
            return spend
        # Boss turn
        apply_debuffs(player, boss)
        if boss.hp <= 0:
            return spend
        curr_armor = player.armor
        if 'Shield' in player.buffs:
            curr_armor += 7
        d = boss.dmg - curr_armor
        player.hp -= d if d > 0 else 1
        if player.hp <= 0:
            return 0

def apply_debuffs(player, boss):
    if player.buffs:
        if 'Recharge' in player.buffs:
            player.mana += 101
        for i in list(player.buffs):
            player.buffs[i] -= 1
            if player.buffs[i] == 0:
                player.buffs.pop(i)
    if boss.debuffs:
        boss.hp -= 3
        boss.debuffs['Poison'] -= 1
        if boss.debuffs['Poison'] == 0:
            boss.debuffs = {}

def apply_spell(player, boss, spell):
    player.mana -= spell.mana
    boss.hp -= spell.dmg
    player.hp += spell.heal
    if spell.time != 0:
        if spell.debuff:
            boss.debuffs['Poison'] = spell.time
        if spell.buff:
            player.buffs[spell.name] = spell.time
    return spell.mana

def run_simulation(penalty=0):
    best = float('inf')
    for _ in range(10000):
        boss = Character(55, 8, buffs={}, debuffs={})
        player = Character(50, 0, mana=500, buffs={}, debuffs={})
        spent = simulate(player, boss, penalty=penalty)
        if spent > 0:
            if spent < best:
                best = spent
    return best

def main():
    rv = run_simulation()
    print('Part 1:', rv)
    rv = run_simulation(penalty=1)
    print('Part 2:', rv)

if __name__ == "__main__":
    main()

# Attempts: 670 (Too low)
