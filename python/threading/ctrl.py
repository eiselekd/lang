import threading, os, re, sys, select, math, binascii, errno, time
import Queue, signal

class threadtest():
    def __init__(self):
        self.rstop = threading.Event();
        self.wstop = threading.Event();
        self.rcmd = Queue.Queue();
        self.wcmd = Queue.Queue();
        self.rreply = Queue.Queue();
        self.wreply = Queue.Queue();
        
    def fiforeader(self,rq,reply):
        while not self.rstop.isSet():
            try:
                e = rq.get(timeout=0.1)
                if (e == "stop"):
                    print("Got stop");
                    break;
                reply.put("done")
            except Queue.Empty as e:
                pass;
        print("Exit reader");
            
    def fifowriter(self,wq,reply):
        while not self.wstop.isSet():
            try:
                e = wq.get(timeout=0.1)
                if (e == "stop"):
                    print("Got stop");
                    break;
                reply.put("done")
            except Queue.Empty as e:
                pass;
        print("Exit writer");

    def startreader(self):
        self.r = threading.Thread(target=self.fiforeader, args = (self.rcmd,self.rreply))
        self.r.start()

    def startwriter(self):
        self.w = threading.Thread(target=self.fifowriter, args = (self.wcmd,self.wreply))
        self.w.start()


# https://www.g-loaded.eu/2016/11/24/how-to-terminate-running-python-threads-using-signals/        
class ServiceExit(Exception):
    """
    Custom exception which is used to trigger the clean exit
    of all running threads and the main program.
    """
    pass
 
 
def service_shutdown(signum, frame):
    print('Caught signal %d' % signum)
    raise ServiceExit
        
# Register the signal handlers
signal.signal(signal.SIGTERM, service_shutdown)
signal.signal(signal.SIGINT, service_shutdown)
        
a = threadtest()
a.startreader();
a.startwriter();

try:
    time.sleep(1);
    for i in range(100):
        a.rcmd.put("test");
        a.wcmd.put("test");
        r0 = a.rreply.get();
        r1 = a.wreply.get();
        print (r0, r1);
    
    time.sleep(1);
    
except ServiceExit as e:
    pass

print("try stop thread:")
a.rstop.set()
a.wstop.set()

while True:
    a.r.join(500);
    a.w.join(500);
    if not (a.r.isAlive() or a.w.isAlive()):
        break
print("Exit main thread ")
