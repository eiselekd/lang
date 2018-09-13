import threading, os, re, sys, select, math, binascii, errno, time

class threadtest():
    def __init__(self):
        self.stop = 0;
        self.fifofn = '/tmp/fifo'
        if os.path.exists(self.fifofn):
            os.unlink(self.fifofn)
        os.mkfifo(self.fifofn)
        
    def fiforeader(self,arg0):
        self.readfd = open(self.fifofn,"r");
        while not self.stop:
            reads,writes,excp = select.select([self.readfd], [], [], 0.1)
            if (len(reads) == 0):
                continue
            v = self.readfd.read(1)
            print(v)
        print("Exit reader");
            
    def fifowriter(self,arg0):
        self.writefd = open(self.fifofn,"w");
        while not self.stop:
            reads,writes,excp = select.select([], [self.writefd], [], 0.1)
            if (len(writes) == 0):
                continue
            v = self.writefd.write('a')
            self.writefd.flush();
        print("Exit writer");

    def startreader(self):
        self.r = threading.Thread(target=self.fiforeader, args = (self,))
        self.r.start()

    def startwriter(self):
        self.w = threading.Thread(target=self.fifowriter, args = (self,))
        self.w.start()

a = threadtest()
a.startreader();
a.startwriter();

time.sleep(1);
a.stop = 1;
while True:
    a.r.join(500);
    a.w.join(500);
    if not (a.r.isAlive() or a.w.isAlive()):
        break
