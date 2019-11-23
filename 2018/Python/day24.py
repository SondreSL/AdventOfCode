from dataclasses import dataclass
from itertools import count
import enum
import re
import math

class Team(enum.Enum):
    Infection = enum.auto()
    Immune = enum.auto()

@dataclass(frozen=False)
class Unit:
    team: Team
    units: int
    hp: int
    ws: [str]
    ims: [str]
    att_type: str
    dmg: int
    init: int
    num: int
    target = None
    dam = 0

    def power(self):
        return self.dmg * self.units

    def __str__(self):
        return f'{self.team.name}: Units: {self.units} | HP: {self.hp}'

def read_file(filename):
    units = []
    with open(filename) as f:
        f.readline()
        team = Team.Immune
        for i, line in enumerate(f):
            line = line.strip()
            if line == '':
                continue
            if line == 'Infection:':
                team = Team.Infection
                continue
            n, hp, dmg, init = (int(i) for i in re.findall(r'\d+', line))
            ws = re.findall(r'(\(.*\))', line)
            ws, im = parse_types(ws)
            att_type = re.findall(r'\d+\s(\w+)\sdamage', line)[0]
            units.append(Unit(team, n, hp, tuple(ws), im,  att_type, dmg, init, i))
    return units

def parse_types(s):
    ws, im = tuple(), tuple()
    if not s:
        return tuple(), tuple()
    s = s[0][1:-1].replace(',', '').split(';')
    s = map(str.split, s)
    for ss in s:
        if ss[0] == 'immune':
            im = tuple(ss[2:])
        else:
            ws = tuple(ss[2:])
    return ws, im

def target_order(units):
    return sorted(units, key=lambda x: (x.units * x.dmg, x.init), reverse=True)

def sort_targets(a, units):
    return list(sorted((t for t in units if a.team != t.team and damage(a, t) > 0), key=lambda x: (damage(a, x), x.units * x.dmg, x.init), reverse=True))

def attack_order(units):
    return sorted((u for u in units if u.target != None), key=lambda x: (x.init, x.units * x.dmg), reverse=True)

def damage(attacker, target):
    if attacker.att_type in target.ims:
        return 0
    if attacker.att_type in target.ws:
        return 2 * attacker.power()
    return attacker.power()

def select_targets(units):
    targets = set()
    for u in target_order(units):
        pot = sort_targets(u, units)
        while pot and pot[0].num in targets:
            pot.pop(0)
        if pot:
            tar = pot[0]
            u.target = tar
            targets.add(tar.num)
        else:
            u.target = None

def attack(units):
    count = 0
    select_targets(units)
    for unit in attack_order(units):
        if unit not in units:
            continue
        t = unit.target
        dmg = damage(unit, t)
        if dmg // t.hp > 0:
            count += 1
        t.units -= dmg // t.hp
        if t.units <= 0:
            units.remove(t)
        unit.target = None
    if count == 0:
        raise ValueError()

def battle(units):
    while len(set(u.team.name for u in units)) > 1:
        attack(units)

def solveA(filename):
    units = read_file(filename)
    battle(units)
    print('Part 1:', sum(u.units for u in units))

def solveB(filename):
    for boost in count():
        units = read_file(filename)
        for u in units:
            if u.team.name == 'Immune':
                u.dmg += boost
        try:
            battle(units)
        except ValueError:
            continue
        if units[0].team.name == 'Immune':
            print(f'Part 2: {sum(u.units for u in units)} (at boost {boost})')
            return

def main():
    solveA('input')
    solveB('input')

if __name__ == "__main__":
    main()
