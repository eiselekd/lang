import threading, os, re, sys, select, math, binascii, errno, time
import signal
from  Queue import Empty
from  multiprocessing import Queue, Process, Event

class threadtest():
    def __init__(self):
        self.rstop = Event();
        self.wstop = Event();
        self.rcmd = Queue();
        self.wcmd = Queue();
        self.rreply = Queue();
        self.wreply = Queue();
        
    def fiforeader(self,rq,reply):
        
        while not self.rstop.is_set():
            try:
                e = rq.get(timeout=0.1)
                if (e == "stop"):
                    print("Got stop");
                    break;
                reply.put("done")
            except Empty as e:
                pass;
        print("Exit reader");
            
    def fifowriter(self,wq,reply):
        
        while not self.wstop.is_set():
            try:
                e = wq.get(timeout=0.1)
                if (e == "stop"):
                    print("Got stop");
                    break;
                reply.put("done")
            except Empty as e:
                pass;
        print("Exit writer");

    def startreader(self):
        self.r = Process(target=self.fiforeader, args = (self.rcmd,self.rreply))
        self.r.start()

    def startwriter(self):
        self.w = Process(target=self.fifowriter, args = (self.wcmd,self.wreply))
        self.w.start()

a = threadtest()
a.startreader();
a.startwriter();
time.sleep(0.01);

time.sleep(1);
for i in range(100):
    a.rcmd.put("test");
    a.wcmd.put("test");
    r0 = a.rreply.get();
    r1 = a.wreply.get();
    print (r0, r1);
    
time.sleep(1);

print("try stop thread:")

a.r.terminate();
a.w.terminate();

print("Exit main thread ")
