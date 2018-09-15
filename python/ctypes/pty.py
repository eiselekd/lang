import os, re, termios
import threading, os, re, sys, select, math, binascii, errno, time
from ctypes import *
import errno

class pty():
    def __init__(self, n="undef"):
        self.cmds = {}
        self.createpty()
        self.name = n
        self.goon = 1
        self.input = ""
        self.output = []

    def createpty(self):

        self.fd3 = os.open("/dev/ptmx", os.O_RDWR | os.O_NONBLOCK);
        if self.fd3 < 0:
            print("Couldn't open output /dev/ptmx\n")
        libc = cdll.LoadLibrary("libc.so.6")
        libc.grantpt(self.fd3);
 	libc.unlockpt(self.fd3);
        libc.ptsname.restype = c_char_p
        self.slave = libc.ptsname(self.fd3)
        print("Use: " + self.slave);

        self.old = termios.tcgetattr(self.fd3)
        n = termios.tcgetattr(self.fd3)

        n[3] = n[3] & ~(termios.ECHO|termios.ICANON) # c_lflag
        n[3] = n[3] & 0

#define VTIME 5
#define VMIN 6
#struct termios
#  {
#    tcflag_t c_iflag;		/* input mode flags */
#    tcflag_t c_oflag;		/* output mode flags */
#    tcflag_t c_cflag;		/* control mode flags */
#    tcflag_t c_lflag;		/* local mode flags */
#    cc_t c_line;			/* line discipline */
#    cc_t c_cc[NCCS];		/* control characters */
#    speed_t c_ispeed;		/* input speed */
#    speed_t c_ospeed;		/* output speed */
##define _HAVE_STRUCT_TERMIOS_C_ISPEED 1
##define _HAVE_STRUCT_TERMIOS_C_OSPEED 1
#  };

        n[4+1] = n[4+1] & 0xffff0000;
 	#ioc.c_cc[VMIN] = 0;  # byte 6 + 7
 	#ioc.c_cc[VTIME] = 0;

        termios.tcsetattr(self.fd3, termios.TCSANOW, n)

        #self.io = os.fdopen(self.fd3, "r+");


# 	    fd3io = fdopen (fd3, "r+");
# 	    fflush (fd3io);
# 	    setbuf (fd3io, NULL);
# 	  }


#typedef long int __fd_mask;
#define __NFDBITS	(8 * (int) sizeof (__fd_mask))
#define	__FD_ELT(d)	((d) / __NFDBITS)
#define	__FD_MASK(d)	((__fd_mask) (1UL << ((d) % __NFDBITS)))

__FD_SETSIZE=1024

def fdmask():
    l = __FD_SETSIZE/64
    r = (c_ulong*l)()
    for i in range(l):
        r[i] = 0;
    return r

def fdset(a,b):
    a[b>>6] = a[b>>6] | (1 << (b % 64))

def fdisset(a,b):
    return (a[b>>6] & (1 << (b % 64)))

def setset(b, a):
    for i in a:
        fdset(b, i)

def setget(b, a):
    return [ i for i in a if fdisset(b,i) ]

def cselect(r,w,x,timeout):

    class timeval(Structure):
        _fields_ = [("tv_sec", c_long), ("tv_usec", c_long)]
    ra = fdmask();
    wa = fdmask();
    xa = fdmask();
    m = max(r+w+x)
    setset(ra, r),
    setset(wa, w),
    setset(xa, x),
    tv = timeval()
    tv.tv_sec = int(timeout)
    tv.tv_usec = int(1000000.0 * (timeout%1.0))
    libc = cdll.LoadLibrary("libc.so.6")
    #print ("Select on " + str(ra))
    ret = libc.select(m+1, ra, wa, xa, pointer(tv));
    if (ret < 0):
        print ("Error %d" %(ret));
    elif (ret == 0):
        return ([],[],[])
    else:
        return (setget(ra,r),setget(wa,w),setget(xa,x))

def main():
    a = pty();
    while True:
        try:
            (r,w,x) = cselect([a.fd3],[],[a.fd3],1)
            if (len(x) > 0):
                time.sleep(0.1);
                print("x");
                continue
            if (len(r) > 0):
                v = os.read(a.fd3, 1)
                print (".");
                os.write(a.fd3,v)
        except pty as e:
            print (str(e));
            raise(e)

if __name__ == "__main__":
    main()
