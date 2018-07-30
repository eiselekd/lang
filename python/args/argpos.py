# https://docs.python.org/3/library/argparse.html
import argparse
from pprint import pprint

def a():
    print("a");

def b():
    print("b");

parser = argparse.ArgumentParser(prog='PROG')
parser.add_argument('--verbose', action='store_true', help='verbose')
parser.add_argument('files', nargs='*')
args = parser.parse_args()

a = ['f1', f2']
opt = parser.parse_args(a)
pprint(opt);
