import sys, os, time
from serial import *;
from pexpect import *; 
import pexpect.fdpexpect as fdpexpect;

ser = Serial("/dev/pts/21",115200);
reader = fdpexpect.fdspawn(ser);
reader.logfile = sys.stdout

reader.send("\n");
state = reader.expect(["root@.*", TIMEOUT], timeout=1)
if (state == 1):
    print ("not powered on, please reset");
    try:
        state = reader.expect(["Please press Enter to activate this console"])
        if (state == 0):
            print ("++++++ Please press Enter to activate this console");
        elif (state == 1):
            print ("proto");
    except TIMEOUT:
        print("Got timeout")
        exit(1)

reader.send("\n");
reader.expect(["root@.*"])

reader.send("netstat -n -t\n");
reader.expect(["root@.*"])

print("netstat on target:");
print(reader.before)
