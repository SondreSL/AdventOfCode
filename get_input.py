import argparse
import subprocess
import datetime

parser = argparse.ArgumentParser(description='Read input')
parser.add_argument('--day', type=int, default=datetime.datetime.now().day)
parser.add_argument('--year', type=int, default=2019)
args = parser.parse_args()

# Ensure you set the $AOC variable to your aoc cookie.
cmd = f'curl https://adventofcode.com/{args.year}/day/{args.day}/input --cookie session=$AOC'

output = subprocess.check_output(cmd, shell=True)

print(output.decode('ascii'), end='')
