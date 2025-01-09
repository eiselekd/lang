const std = @import("../index.zig");
const c = std.c;
const assert = std.debug.assert;

pub use @import("darwin_errno.zig");

pub const PATH_MAX = 1024;

pub const STDIN_FILENO = 0;
pub const STDOUT_FILENO = 1;
pub const STDERR_FILENO = 2;

pub const PROT_NONE   = 0x00; /// [MC2] no permissions
pub const PROT_READ   = 0x01; /// [MC2] pages can be read
pub const PROT_WRITE  = 0x02; /// [MC2] pages can be written
pub const PROT_EXEC   = 0x04; /// [MC2] pages can be executed

pub const MAP_ANONYMOUS = 0x1000; /// allocated from memory, swap space
pub const MAP_FILE = 0x0000; /// map from file (default)
pub const MAP_FIXED = 0x0010; /// interpret addr exactly
pub const MAP_HASSEMAPHORE = 0x0200; /// region may contain semaphores
pub const MAP_PRIVATE = 0x0002; /// changes are private
pub const MAP_SHARED = 0x0001; /// share changes
pub const MAP_NOCACHE = 0x0400; /// don't cache pages for this mapping
pub const MAP_NORESERVE = 0x0040; /// don't reserve needed swap area
pub const MAP_FAILED = @maxValue(usize);

pub const WNOHANG   = 0x00000001; /// [XSI] no hang in wait/no child to reap
pub const WUNTRACED = 0x00000002; /// [XSI] notify on stop, untraced child

pub const SA_ONSTACK   = 0x0001; /// take signal on signal stack
pub const SA_RESTART   = 0x0002; /// restart system on signal return
pub const SA_RESETHAND = 0x0004; /// reset to SIG_DFL when taking signal
pub const SA_NOCLDSTOP = 0x0008; /// do not generate SIGCHLD on child stop
pub const SA_NODEFER   = 0x0010; /// don't mask the signal we're delivering
pub const SA_NOCLDWAIT = 0x0020; /// don't keep zombies around
pub const SA_SIGINFO   = 0x0040; /// signal handler with SA_SIGINFO args
pub const SA_USERTRAMP = 0x0100; /// do not bounce off kernel's sigtramp
pub const SA_64REGSET  = 0x0200; /// signal handler with SA_SIGINFO args with 64bit   regs information

pub const O_LARGEFILE = 0x0000;
pub const O_PATH = 0x0000;

pub const O_RDONLY   = 0x0000; /// open for reading only
pub const O_WRONLY   = 0x0001; /// open for writing only
pub const O_RDWR     = 0x0002; /// open for reading and writing
pub const O_NONBLOCK = 0x0004; /// do not block on open or for data to become available
pub const O_APPEND   = 0x0008; /// append on each write
pub const O_CREAT    = 0x0200; /// create file if it does not exist
pub const O_TRUNC    = 0x0400; /// truncate size to 0
pub const O_EXCL     = 0x0800; /// error if O_CREAT and the file exists
pub const O_SHLOCK   = 0x0010; /// atomically obtain a shared lock
pub const O_EXLOCK   = 0x0020; /// atomically obtain an exclusive lock
pub const O_NOFOLLOW = 0x0100; /// do not follow symlinks
pub const O_SYMLINK  = 0x200000; /// allow open of symlinks
pub const O_EVTONLY  = 0x8000; /// descriptor requested for event notifications only
pub const O_CLOEXEC  = 0x1000000; /// mark as close-on-exec

pub const SEEK_SET = 0x0;
pub const SEEK_CUR = 0x1;
pub const SEEK_END = 0x2;

pub const SIG_BLOCK   = 1; /// block specified signal set
pub const SIG_UNBLOCK = 2; /// unblock specified signal set
pub const SIG_SETMASK = 3; /// set specified signal set

pub const SIGHUP    = 1; /// hangup
pub const SIGINT    = 2; /// interrupt
pub const SIGQUIT   = 3; /// quit
pub const SIGILL    = 4; /// illegal instruction (not reset when caught)
pub const SIGTRAP   = 5; /// trace trap (not reset when caught)
pub const SIGABRT   = 6; /// abort()
pub const SIGPOLL   = 7; /// pollable event ([XSR] generated, not supported)
pub const SIGIOT    = SIGABRT; /// compatibility
pub const SIGEMT    = 7;  /// EMT instruction
pub const SIGFPE    = 8;  /// floating point exception
pub const SIGKILL   = 9;  /// kill (cannot be caught or ignored)
pub const SIGBUS    = 10; /// bus error
pub const SIGSEGV   = 11; /// segmentation violation
pub const SIGSYS    = 12; /// bad argument to system call
pub const SIGPIPE   = 13; /// write on a pipe with no one to read it
pub const SIGALRM   = 14; /// alarm clock
pub const SIGTERM   = 15; /// software termination signal from kill
pub const SIGURG    = 16; /// urgent condition on IO channel
pub const SIGSTOP   = 17; /// sendable stop signal not from tty
pub const SIGTSTP   = 18; /// stop signal from tty
pub const SIGCONT   = 19; /// continue a stopped process
pub const SIGCHLD   = 20; /// to parent on child stop or exit
pub const SIGTTIN   = 21; /// to readers pgrp upon background tty read
pub const SIGTTOU   = 22; /// like TTIN for output if (tp->t_local&LTOSTOP)
pub const SIGIO     = 23; /// input/output possible signal
pub const SIGXCPU   = 24; /// exceeded CPU time limit
pub const SIGXFSZ   = 25; /// exceeded file size limit
pub const SIGVTALRM = 26; /// virtual time alarm
pub const SIGPROF   = 27; /// profiling time alarm
pub const SIGWINCH  = 28; /// window size changes
pub const SIGINFO   = 29; /// information request
pub const SIGUSR1   = 30; /// user defined signal 1
pub const SIGUSR2   = 31; /// user defined signal 2

fn wstatus(x: i32) i32 { return x & 0o177; }
const wstopped = 0o177;
pub fn WEXITSTATUS(x: i32) i32 { return x >> 8; }
pub fn WTERMSIG(x: i32) i32 { return wstatus(x); }
pub fn WSTOPSIG(x: i32) i32 { return x >> 8; }
pub fn WIFEXITED(x: i32) bool { return wstatus(x) == 0; }
pub fn WIFSTOPPED(x: i32) bool { return wstatus(x) == wstopped and WSTOPSIG(x) != 0x13; }
pub fn WIFSIGNALED(x: i32) bool { return wstatus(x) != wstopped and wstatus(x) != 0; }

/// Get the errno from a syscall return value, or 0 for no error.
pub fn getErrno(r: usize) usize {
    const signed_r = @bitCast(isize, r);
    return if (signed_r > -4096 and signed_r < 0) usize(-signed_r) else 0;
}

pub fn close(fd: i32) usize {
    return errnoWrap(c.close(fd));
}

pub fn abort() noreturn {
    c.abort();
}

pub fn exit(code: i32) noreturn {
    c.exit(code);
}

pub fn isatty(fd: i32) bool {
    return c.isatty(fd) != 0;
}

pub fn fstat(fd: i32, buf: &c.Stat) usize {
    return errnoWrap(c.@"fstat$INODE64"(fd, buf));
}

pub fn lseek(fd: i32, offset: isize, whence: c_int) usize {
    return errnoWrap(c.lseek(fd, offset, whence));
}

pub fn open(path: &const u8, flags: u32, mode: usize) usize {
    return errnoWrap(c.open(path, @bitCast(c_int, flags), mode));
}

pub fn raise(sig: i32) usize {
    return errnoWrap(c.raise(sig));
}

pub fn read(fd: i32, buf: &u8, nbyte: usize) usize {
    return errnoWrap(c.read(fd, @ptrCast(&c_void, buf), nbyte));
}

pub fn stat(noalias path: &const u8, noalias buf: &stat) usize {
    return errnoWrap(c.stat(path, buf));
}

pub fn write(fd: i32, buf: &const u8, nbyte: usize) usize {
    return errnoWrap(c.write(fd, @ptrCast(&const c_void, buf), nbyte));
}

pub fn mmap(address: ?&u8, length: usize, prot: usize, flags: usize, fd: i32,
    offset: isize) usize
{
    const ptr_result = c.mmap(@ptrCast(&c_void, address), length,
        @bitCast(c_int, c_uint(prot)), @bitCast(c_int, c_uint(flags)), fd, offset);
    const isize_result = @bitCast(isize, @ptrToInt(ptr_result));
    return errnoWrap(isize_result);
}

pub fn munmap(address: &u8, length: usize) usize {
    return errnoWrap(c.munmap(@ptrCast(&c_void, address), length));
}

pub fn unlink(path: &const u8) usize {
    return errnoWrap(c.unlink(path));
}

pub fn getcwd(buf: &u8, size: usize) usize {
    return if (c.getcwd(buf, size) == null) @bitCast(usize, -isize(*c._errno())) else 0;
}

pub fn waitpid(pid: i32, status: &i32, options: u32) usize {
    comptime assert(i32.bit_count == c_int.bit_count);
    return errnoWrap(c.waitpid(pid, @ptrCast(&c_int, status), @bitCast(c_int, options)));
}

pub fn fork() usize {
    return errnoWrap(c.fork());
}

pub fn pipe(fds: &[2]i32) usize {
    comptime assert(i32.bit_count == c_int.bit_count);
    return errnoWrap(c.pipe(@ptrCast(&c_int, fds)));
}

pub fn mkdir(path: &const u8, mode: u32) usize {
    return errnoWrap(c.mkdir(path, mode));
}

pub fn symlink(existing: &const u8, new: &const u8) usize {
    return errnoWrap(c.symlink(existing, new));
}

pub fn rename(old: &const u8, new: &const u8) usize {
    return errnoWrap(c.rename(old, new));
}

pub fn chdir(path: &const u8) usize {
    return errnoWrap(c.chdir(path));
}

pub fn execve(path: &const u8, argv: &const ?&const u8, envp: &const ?&const u8) usize {
    return errnoWrap(c.execve(path, argv, envp));
}

pub fn dup2(old: i32, new: i32) usize {
    return errnoWrap(c.dup2(old, new));
}

pub fn readlink(noalias path: &const u8, noalias buf_ptr: &u8, buf_len: usize) usize {
    return errnoWrap(c.readlink(path, buf_ptr, buf_len));
}

pub fn nanosleep(req: &const timespec, rem: ?&timespec) usize {
    return errnoWrap(c.nanosleep(req, rem));
}

pub fn realpath(noalias filename: &const u8, noalias resolved_name: &u8) usize {
    return if (c.realpath(filename, resolved_name) == null) @bitCast(usize, -isize(*c._errno())) else 0;
}

pub fn setreuid(ruid: u32, euid: u32) usize {
    return errnoWrap(c.setreuid(ruid, euid));
}

pub fn setregid(rgid: u32, egid: u32) usize {
    return errnoWrap(c.setregid(rgid, egid));
}

pub fn sigprocmask(flags: u32, noalias set: &const sigset_t, noalias oldset: ?&sigset_t) usize {
    return errnoWrap(c.sigprocmask(@bitCast(c_int, flags), set, oldset));
}

pub fn sigaction(sig: u5, noalias act: &const Sigaction, noalias oact: ?&Sigaction) usize {
    assert(sig != SIGKILL);
    assert(sig != SIGSTOP);
    var cact = c.Sigaction {
        .handler = @ptrCast(extern fn(c_int)void, act.handler),
        .sa_flags = @bitCast(c_int, act.flags),
        .sa_mask = act.mask,
    };
    var coact: c.Sigaction = undefined;
    const result = errnoWrap(c.sigaction(sig, &cact, &coact));
    if (result != 0) {
        return result;
    }
    if (oact) |old| {
        *old = Sigaction {
            .handler = @ptrCast(extern fn(i32)void, coact.handler),
            .flags = @bitCast(u32, coact.sa_flags),
            .mask = coact.sa_mask,
        };
    }
    return result;
}

pub const sigset_t = c.sigset_t;
pub const empty_sigset = sigset_t(0);

pub const timespec = c.timespec;
pub const Stat = c.Stat;

/// Renamed from `sigaction` to `Sigaction` to avoid conflict with the syscall.
pub const Sigaction = struct {
    handler: extern fn(i32)void,
    mask: sigset_t,
    flags: u32,
};

pub fn sigaddset(set: &sigset_t, signo: u5) void {
    *set |= u32(1) << (signo - 1);
}

/// Takes the return value from a syscall and formats it back in the way
/// that the kernel represents it to libc. Errno was a mistake, let's make
/// it go away forever.
fn errnoWrap(value: isize) usize {
    return @bitCast(usize, if (value == -1) -isize(*c._errno()) else value);
}
