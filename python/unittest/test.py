#!/usr/bin/python
# Local Variables:
# checker-enable:'1'
# End:

# https://docs.python.org/3/library/unittest.html

import unittest

class TestStringMethods(unittest.TestCase):

    def subf(self):
        self.assertEqual('foo'.upper(), 'FOo')

    def test_upper(self):
        self.subf()

    def test_isupper(self):
        self.assertTrue('FOO'.isupper())
        self.assertFalse('Foo'.isupper())

    def test_split(self):
        s = 'hello world'
        self.assertEqual(s.split(), ['hello', 'world'])
        # check that s.split fails when the separator is not a string
        with self.assertRaises(TypeError):
            s.split(2)
