#!/usr/bin/python

import pexpect
import time

cling_command="cling"

def start_cling ():

    try:
        cling = pexpect.spawn (cling_command % globals())
        cling.expect ('\[cling\]\$')
        time.sleep (0.1)
        print(cling.before)
        cling.sendline(".L cling.cpp\n")
        time.sleep (0.2)
        cling.expect ('\[cling\]\$')
        print(cling.before)
        cling.sendline("cling_main();\n")
        i = cling.expect ('\[cling\]\$', timeout=30)
        print(cling.before)
        cling.sendline(".q\n")
        print(cling.read())

    except Exception as e:
        print(str(e))

start_cling()
