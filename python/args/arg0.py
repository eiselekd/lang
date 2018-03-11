# https://docs.python.org/3/library/argparse.html
import argparse
from pprint import pprint

def a():
    print("a");

def b():
    print("b");

parser = argparse.ArgumentParser(prog='PROG')
parser.add_argument('--verbose', action='store_true', help='verbose')
subparsers = parser.add_subparsers(help='sub-commands help')

# create the parser for the "a" command
parser_a = subparsers.add_parser('a', help='a help')
parser_a.add_argument('bar', type=int, help='bar help')
parser_a.set_defaults(func=a)

# create the parser for the "b" command
parser_b = subparsers.add_parser('b', help='b help')
parser_b.add_argument('--baz', choices='XYZ', help='baz help')
parser_b.set_defaults(func=b)

# parse some argument lists
a = ['a', '12']
opt = parser.parse_args(a)
pprint(opt);

opt = parser.parse_args(['--verbose', 'b', '--baz', 'Z'])
pprint(opt);
