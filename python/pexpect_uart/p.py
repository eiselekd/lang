import sys, os
from serial import *;
from pexpect import *; 
import pexpect.fdpexpect as fdpexpect;

ser = Serial("/dev/ttyUSB0",115200);
reader = fdpexpect.fdspawn(ser);
reader.logfile = sys.stdout

reader.send("\n");
state = reader.expect(["root@Carambola2", TIMEOUT], timeout=1)
if (state == 1):
    print ("not powered on, please reset");
    try:
        state = reader.expect(["ieee80211 phy0", "Registered protocol family", TIMEOUT], timeout=30)
        if (state == 0):
            print ("start");
        elif (state == 1):
            print ("proto");
    except TIMEOUT:
        print("Got timeout")
        exit(1)

print("Ready for pexpect");

reader.send("\n");
reader.expect(["root@Carambola2.*", TIMEOUT], timeout=1)
reader.send("netstat -n -t\n");
reader.expect(["root@Carambola2.*", TIMEOUT], timeout=1)

print("netstat on target:");
print(reader.before)
