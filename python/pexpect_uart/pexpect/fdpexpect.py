'''This is like pexpect, but it will work with any file descriptor that you
pass it. You are responsible for opening and close the file descriptor.
This allows you to use Pexpect with sockets and named pipes (FIFOs).

PEXPECT LICENSE

    This license is approved by the OSI and FSF as GPL-compatible.
        http://opensource.org/licenses/isc-license.txt

    Copyright (c) 2012, Noah Spurrier <noah@noah.org>
    PERMISSION TO USE, COPY, MODIFY, AND/OR DISTRIBUTE THIS SOFTWARE FOR ANY
    PURPOSE WITH OR WITHOUT FEE IS HEREBY GRANTED, PROVIDED THAT THE ABOVE
    COPYRIGHT NOTICE AND THIS PERMISSION NOTICE APPEAR IN ALL COPIES.
    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
    WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
    ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
    WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
    ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

'''

from .spawnbase import SpawnBase
from .exceptions import ExceptionPexpect, TIMEOUT
from .utils import select_ignore_interrupts
import os, time, threading

__all__ = ['fdspawn', 'fdspawn_readerthread']

def getnow():
    return time.time()

class fdspawn_readerthread(fdspawn):

    def __init__ (self, fd, args=None, timeout=30, maxread=2000, searchwindowsize=None,
                  logfile=None, encoding=None, codec_errors='strict'):

        fdspawn.__init__(self, fd, args, timeout, maxread, searchwindowsize,
                  logfile, encoding, codec_errors)

        self.l = threading.Lock()
        self.e = threading.Event()
        self.r = threading.Thread(target=self.reader, args = (self,))
        self.data = b'';

    def reader(self, arg0):

        while not self.closed :
            try:
                data = fdspawn.read_nonblocking(32, 0.1)
            except serial.SerialException as e:
                print("-------- SerialException -------" + str(e))
                error = e
                break
            else:
                if data:
                    self.l.aquire(True);
                    self.data += data
                    self.l.release();
                    self.e.set()

    def read_nonblocking(self, size=1, timeout=-1):
        if (timeout == -1):
            timeout = self.timeout
        i0 = time.time() + timeout
        while len(self.data) < size:
            c = time.time()
            if (c > i0):
                break;
            if self.e.wait(i0-c):
                continue;
            else:
                break;
        r = b'';
        self.l.aquire(True);
        r = self.data[0:size];
        self.data = data[size:]
        self.l.release();
        return r;


class fdspawn(SpawnBase):
    '''This is like pexpect.spawn but allows you to supply your own open file
    descriptor. For example, you could use it to read through a file looking
    for patterns, or to control a modem or serial device. '''

    def __init__ (self, fd, args=None, timeout=30, maxread=2000, searchwindowsize=None,
                  logfile=None, encoding=None, codec_errors='strict'):
        '''This takes a file descriptor (an int) or an object that support the
        fileno() method (returning an int). All Python file-like objects
        support fileno(). '''

        if type(fd) != type(0) and hasattr(fd, 'fileno'):
            fd = fd.fileno()

        if type(fd) != type(0):
            raise ExceptionPexpect('The fd argument is not an int. If this is a command string then maybe you want to use pexpect.spawn.')

        try: # make sure fd is a valid file descriptor
            os.fstat(fd)
        except OSError:
            raise ExceptionPexpect('The fd argument is not a valid file descriptor.')

        self.args = None
        self.command = None
        SpawnBase.__init__(self, timeout, maxread, searchwindowsize, logfile,
                           encoding=encoding, codec_errors=codec_errors)
        self.child_fd = fd
        self.own_fd = False
        self.closed = False
        self.name = '<file descriptor %d>' % fd

    def close (self):
        """Close the file descriptor.

        Calling this method a second time does nothing, but if the file
        descriptor was closed elsewhere, :class:`OSError` will be raised.
        """
        if self.child_fd == -1:
            return

        self.flush()
        os.close(self.child_fd)
        self.child_fd = -1
        self.closed = True

    def isalive (self):
        '''This checks if the file descriptor is still valid. If :func:`os.fstat`
        does not raise an exception then we assume it is alive. '''

        if self.child_fd == -1:
            return False
        try:
            os.fstat(self.child_fd)
            return True
        except:
            return False

    def terminate (self, force=False):  # pragma: no cover
        '''Deprecated and invalid. Just raises an exception.'''
        raise ExceptionPexpect('This method is not valid for file descriptors.')
    
    # These four methods are left around for backwards compatibility, but not
    # documented as part of fdpexpect. You're encouraged to use os.write
    # directly.
    def send(self, s):
        "Write to fd, return number of bytes written"
        s = self._coerce_send_string(s)
        self._log(s, 'send')
        
        b = self._encoder.encode(s, final=False)
        return os.write(self.child_fd, b)
    
    def sendline(self, s):
        "Write to fd with trailing newline, return number of bytes written"
        s = self._coerce_send_string(s)
        return self.send(s + self.linesep)
    
    def write(self, s):
        "Write to fd, return None"
        self.send(s)
    
    def writelines(self, sequence):
        "Call self.write() for each item in sequence"
        for s in sequence:
            self.write(s)

    def read_nonblocking(self, size=1, timeout=-1):
        """
        Read from the file descriptor and return the result as a string.

        The read_nonblocking method of :class:`SpawnBase` assumes that a call
        to os.read will not block (timeout parameter is ignored). This is not
        the case for POSIX file-like objects such as sockets and serial ports.

        Use :func:`select.select`, timeout is implemented conditionally for
        POSIX systems.

        :param int size: Read at most *size* bytes.
        :param int timeout: Wait timeout seconds for file descriptor to be
            ready to read. When -1 (default), use self.timeout. When 0, poll.
        :return: String containing the bytes read
        """
        if os.name == 'posix':
            if timeout == -1:
                timeout = self.timeout
            rlist = [self.child_fd]
            wlist = []
            xlist = []
            rlist, wlist, xlist = select_ignore_interrupts(rlist, wlist, xlist, timeout)
            if self.child_fd not in rlist:
                raise TIMEOUT('Timeout exceeded.')
        return super(fdspawn, self).read_nonblocking(size)

data=[]

class fdspawn_readerthread(fdspawn):

    def __init__ (self, fd, args=None, timeout=30, maxread=2000, searchwindowsize=None,
                  logfile=None, encoding=None, codec_errors='strict'):

        fdspawn.__init__(self, fd, args, timeout, maxread, searchwindowsize,
                  logfile, encoding, codec_errors)

        global data
        self.l = threading.Lock()
        self.e = threading.Event()
        self.dataidx = len(data)
        data.append( b'');
        self.r = threading.Thread(target=self.reader, args = (self.dataidx,))
        self.r.start()
        print(self.dataidx)

    def reader(self, dataidx):
        global data
        print("start thread with index %d"%(dataidx))
        while not self.goon.is_set():
            try:
                v = fdspawn.read_nonblocking(self, 32, 0.1)
            except TIMEOUT as e:
                pass
            except Exception as e:
                print("-------- SerialException -------" + str(type(e)))
                error = e
                break
            else:
                if v:
                    self.l.acquire(True);
                    data[dataidx] = data[dataidx] + v
                    self.l.release();
                    self.e.set()
                    #print(data);


    def read_nonblocking(self, size=1, timeout=-1):
        global data
        if (timeout == -1):
            timeout = self.timeout
        i0 = time.time() + timeout
        while len(data[self.dataidx]) == 0:
            c = time.time()
            if (c > i0):
                raise TIMEOUT('Timeout exceeded.')
            if self.e.wait(i0-c):
                self.e.clear()
                continue;
            else:
                raise TIMEOUT('Timeout exceeded.')
        r = b'';
        self.l.acquire(True);
        r = data[self.dataidx][0:size];
        data[self.dataidx] = data[self.dataidx][size:]
        self.l.release();

        #print(data[self.dataidx][0:10])
        #print(self.dataidx)
        return r;
