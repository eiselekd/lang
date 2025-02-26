;; Foreign functions provided by the Linux C library version 6,
;; i.e. the GNU C library version 2.0.7.
;; Bruno Haible 10.4.1998, 19.4.1998

(defpackage "LINUX"
  (:case-sensitive t)
  (:nicknames "UNIX" "GLIBC")
  (:use)
)

;; This requires linking with NEW_LIBS='linux.o -lm'.


(lisp:in-package "LISP")

(eval-when (compile eval)
  ; A temporary package, case-insensitive, so that we don't need to prefix
  ; everything with "lisp:" or "ffi:".
  (defpackage "LINUX-AUX"
    (:use "LISP" "FFI")
  )
)

(eval-when (compile eval)
  (in-package "LINUX-AUX")
)

(eval-when (compile eval)
  ; Symbols to be substituted
  (defconstant substitution
    '((linux::aref . lisp:aref)
      (linux::AREF . lisp:aref)
      (linux::ash . lisp:ash)
      (linux::ASH . lisp:ash)
      (linux::coerce . lisp:coerce)
      (linux::compile . lisp:compile)
      (linux::dotimes . lisp:dotimes)
      (linux::eval . lisp:eval)
      (linux::fill . lisp:fill)
      (linux::FLOOR . lisp:floor)
      (linux::gensym . lisp:gensym)
      (linux::let . lisp:let)
      (linux::load . lisp:load)
      (linux::load-time-value . lisp:load-time-value)
      (linux::logand . lisp:logand)
      (linux::logbitp . lisp:logbitp)
      (linux::logior . lisp:logior)
      (linux::lognot . lisp:lognot)
      (linux::mod . lisp:mod)
      (linux::multiple-value-bind . lisp:multiple-value-bind)
      (linux::MULTIPLE-VALUE-BIND . lisp:multiple-value-bind)
      (linux::progn . lisp:progn)
      (linux::setf . lisp:setf)
      (linux::t . lisp:t)
      (linux::+ . lisp:+)
      (linux::- . lisp:-)
      (linux::* . lisp:*)
      (linux::= . lisp:=)
      (linux::1- . lisp:1-)
      (linux::bitsizeof . ffi:bitsizeof)
      (linux::boolean . ffi:boolean)
      (linux::char . ffi:char)
      (linux::character . ffi:character)
      (linux::c-array . ffi:c-array)
      (linux::c-array-max . ffi:c-array-max)
      (linux::c-array-ptr . ffi:c-array-ptr)
      (linux::c-function . ffi:c-function)
      (linux::c-ptr . ffi:c-ptr)
      (linux::c-pointer . ffi:c-pointer)
      (linux::c-string . ffi:c-string)
      (linux::c-struct . ffi:c-struct)
      (linux::deref . ffi:deref)
      (linux::double-float . ffi:double-float)
      (linux::element . ffi:element)
      (linux::int . ffi:int)
      (linux::long . ffi:long)
      (linux::nil . ffi:nil)
      (linux::short . ffi:short)
      (linux::sint8 . ffi:sint8)
      (linux::sint16 . ffi:sint16)
      (linux::sint32 . ffi:sint32)
      (linux::sint64 . ffi:sint64)
      (linux::single-float . ffi:single-float)
      (linux::sizeof . ffi:sizeof)
      (linux::slot . ffi:slot)
      (linux::uchar . ffi:uchar)
      (linux::uint . ffi:uint)
      (linux::uint8 . ffi:uint8)
      (linux::uint16 . ffi:uint16)
      (linux::uint32 . ffi:uint32)
      (linux::uint64 . ffi:uint64)
      (linux::ulong . ffi:ulong)
      (linux::ushort . ffi:ushort)
  )  )
)

(eval-when (compile eval)
; We want to export all the symbols defined in this file.
  (macrolet ((exporting (defining-macro-name)
               (let ((original-macro-name (intern (string-upcase defining-macro-name) "FFI"))
                     (new-macro-name (intern defining-macro-name "LINUX")))
                 `(progn
                    (defmacro ,new-macro-name (name &rest more)
                      `(progn
                         (export ',name)
                         (,',original-macro-name ,name ,@(sublis substitution more))
                       )
                  ) )
             ) )
             (normal (defining-macro-name)
               (let ((original-macro-name (intern (string-upcase defining-macro-name) "FFI"))
                     (new-macro-name (intern defining-macro-name "LINUX")))
                 `(progn
                    (defmacro ,new-macro-name (&rest more)
                      `(,',original-macro-name ,@(sublis substitution more))
                  ) )
            )) )
    (exporting "defconstant")
    (exporting "defun")
    (exporting "defmacro")
    (exporting "define-modify-macro")
    (exporting "define-symbol-macro")
    (exporting "def-c-type")
    (exporting "def-c-enum")
    (exporting "def-c-struct")
    (exporting "def-c-var")
    (exporting "def-c-call-out")
    (normal "c-lines")
    (normal "eval-when")
  )
)

(in-package "LINUX")

(def-c-type longlong sint64)
(def-c-type ulonglong uint64)
(define-modify-macro orf () logior)
(define-modify-macro andf () logand)

; ============================= <stddef.h> ====================================

(c-lines "#include <stddef.h>~%")

(def-c-type ptrdiff_t long)

(def-c-type size_t uint)  ; uint or ulong, doesn't matter

(def-c-type wchar_t int)

; =========================== <sys/types.h> ===================================

(c-lines "#include <sys/types.h>~%")

; --------------------------- <gnu/types.h> -----------------------------------

(def-c-type __u_char uchar)
(def-c-type __u_short ushort)
(def-c-type __u_int uint)
(def-c-type __u_long ulong)
(def-c-type __u_quad_t uint64)
(def-c-type __quad_t sint64)
(def-c-type __qaddr_t (c-ptr __quad_t))

(def-c-type __dev_t __u_quad_t)
(def-c-type __uid_t __u_int)
(def-c-type __gid_t __u_int)
(def-c-type __ino_t __u_long)
(def-c-type __mode_t __u_int)
(def-c-type __nlink_t __u_int)
(def-c-type __off_t long)
(def-c-type __loff_t __quad_t)
(def-c-type __pid_t int)
(def-c-type __ssize_t int)

(def-c-struct __fsid_t
  (__val (c-array int 2))
)

(def-c-type __daddr_t int)
(def-c-type __caddr_t c-pointer)
(def-c-type __time_t long)
(def-c-type __swblk_t long)

(def-c-type __fd_mask ulong)
(eval-when (load compile eval)
  (defconstant __FD_SETSIZE 1024)
)

(eval-when (load compile eval)
  (defconstant __NFDBITS (* 8 (sizeof '__fd_mask)))
)
(defmacro __FDELT (d) `(FLOOR ,d __NFDBITS))
(defmacro __FDMASK (d) `(ash 1 (mod ,d __NFDBITS)))

(def-c-struct __fd_set
  (fds_bits (c-array __fd_mask #.(lisp:/ __FD_SETSIZE __NFDBITS)))
)

(def-c-type __key_t int)

(def-c-type __ipc_pid_t ushort)

; --------------------------- <sys/types.h> -----------------------------------

(def-c-type dev_t __dev_t)
(def-c-type gid_t __gid_t)
(def-c-type ino_t __ino_t)
(def-c-type mode_t __mode_t)
(def-c-type nlink_t __nlink_t)
(def-c-type off_t __off_t)
(def-c-type loff_t __loff_t)
(def-c-type pid_t __pid_t)
(def-c-type uid_t __uid_t)

(def-c-type ssize_t __ssize_t)

(def-c-type daddr_t __daddr_t)
(def-c-type caddr_t __caddr_t)

(def-c-type key_t __key_t)

(def-c-type time_t __time_t)   ; in <time.h>

(def-c-type size_t uint)       ; in <stddef.h>

(def-c-type int8_t sint8)
(def-c-type u_int8_t uint8)
(def-c-type int16_t sint16)
(def-c-type u_int16_t uint16)
(def-c-type int32_t sint32)
(def-c-type u_int32_t uint32)
(def-c-type int64_t sint64)
(def-c-type u_int64_t uint64)

; -------------------------- <selectbits.h> -----------------------------------

(defmacro __FD_ZERO (set)
  `(dotimes (i #.(lisp:/ __FD_SETSIZE __NFDBITS))
     (setf (element (slot (deref ,set) fds_bits) i) 0)
   )
)
(defmacro __FD_SET (d set)
  `(orf (element (slot (deref ,set) fds_bits) (__FDELT ,d)) (__FDMASK ,d))
)
(defmacro __FD_CLR (d set)
  `(andf (element (slot (deref ,set) fds_bits) (__FDELT ,d)) (lognot (__FDMASK ,d)))
)
(defmacro __FD_ISSET (d set)
  `(logbitp (mod ,d __NFDBITS) (element (slot (deref ,set) fds_bits) (__FDELT ,d)))
)

; -------------------------- <sys/select.h> -----------------------------------

(def-c-type fd_mask __fd_mask)

(def-c-type fd_set __fd_set)

(defconstant FD_SETSIZE __FD_SETSIZE)

(defmacro FD_SET (fd fdset) `(__FD_SET ,fd ,fdset))
(defmacro FD_CLR (fd fdset) `(__FD_CLR ,fd ,fdset))
(defmacro FD_ISSET (fd fdset) `(__FD_ISSET ,fd ,fdset))
(defmacro FD_ZERO (fdset) `(__FD_ZERO ,fdset))

; ============================ <sys/cdefs.h> ==================================

; ============================ <features.h> ===================================

; ============================== <errno.h> ====================================

; --------------------------- <linux/errno.h> ---------------------------------

; ---------------------------- <asm/errno.h> ----------------------------------

(defconstant    EPERM            1)     ; Operation not permitted
(defconstant    ENOENT           2)     ; No such file or directory
(defconstant    ESRCH            3)     ; No such process
(defconstant    EINTR            4)     ; Interrupted system call
(defconstant    EIO              5)     ; I/O error
(defconstant    ENXIO            6)     ; No such device or address
(defconstant    E2BIG            7)     ; Arg list too long
(defconstant    ENOEXEC          8)     ; Exec format error
(defconstant    EBADF            9)     ; Bad file number
(defconstant    ECHILD          10)     ; No child processes
(defconstant    EAGAIN          11)     ; Try again
(defconstant    ENOMEM          12)     ; Out of memory
(defconstant    EACCES          13)     ; Permission denied
(defconstant    EFAULT          14)     ; Bad address
(defconstant    ENOTBLK         15)     ; Block device required
(defconstant    EBUSY           16)     ; Device or resource busy
(defconstant    EEXIST          17)     ; File exists
(defconstant    EXDEV           18)     ; Cross-device link
(defconstant    ENODEV          19)     ; No such device
(defconstant    ENOTDIR         20)     ; Not a directory
(defconstant    EISDIR          21)     ; Is a directory
(defconstant    EINVAL          22)     ; Invalid argument
(defconstant    ENFILE          23)     ; File table overflow
(defconstant    EMFILE          24)     ; Too many open files
(defconstant    ENOTTY          25)     ; Not a typewriter
(defconstant    ETXTBSY         26)     ; Text file busy
(defconstant    EFBIG           27)     ; File too large
(defconstant    ENOSPC          28)     ; No space left on device
(defconstant    ESPIPE          29)     ; Illegal seek
(defconstant    EROFS           30)     ; Read-only file system
(defconstant    EMLINK          31)     ; Too many links
(defconstant    EPIPE           32)     ; Broken pipe
(defconstant    EDOM            33)     ; Math argument out of domain of func
(defconstant    ERANGE          34)     ; Math result not representable
(defconstant    EDEADLK         35)     ; Resource deadlock would occur
(defconstant    ENAMETOOLONG    36)     ; File name too long
(defconstant    ENOLCK          37)     ; No record locks available
(defconstant    ENOSYS          38)     ; Function not implemented
(defconstant    ENOTEMPTY       39)     ; Directory not empty
(defconstant    ELOOP           40)     ; Too many symbolic links encountered
(defconstant    EWOULDBLOCK     EAGAIN) ; Operation would block
(defconstant    ENOMSG          42)     ; No message of desired type
(defconstant    EIDRM           43)     ; Identifier removed
(defconstant    ECHRNG          44)     ; Channel number out of range
(defconstant    EL2NSYNC        45)     ; Level 2 not synchronized
(defconstant    EL3HLT          46)     ; Level 3 halted
(defconstant    EL3RST          47)     ; Level 3 reset
(defconstant    ELNRNG          48)     ; Link number out of range
(defconstant    EUNATCH         49)     ; Protocol driver not attached
(defconstant    ENOCSI          50)     ; No CSI structure available
(defconstant    EL2HLT          51)     ; Level 2 halted
(defconstant    EBADE           52)     ; Invalid exchange
(defconstant    EBADR           53)     ; Invalid request descriptor
(defconstant    EXFULL          54)     ; Exchange full
(defconstant    ENOANO          55)     ; No anode
(defconstant    EBADRQC         56)     ; Invalid request code
(defconstant    EBADSLT         57)     ; Invalid slot
(defconstant    EDEADLOCK       EDEADLK); File locking deadlock error
(defconstant    EBFONT          59)     ; Bad font file format
(defconstant    ENOSTR          60)     ; Device not a stream
(defconstant    ENODATA         61)     ; No data available
(defconstant    ETIME           62)     ; Timer expired
(defconstant    ENOSR           63)     ; Out of streams resources
(defconstant    ENONET          64)     ; Machine is not on the network
(defconstant    ENOPKG          65)     ; Package not installed
(defconstant    EREMOTE         66)     ; Object is remote
(defconstant    ENOLINK         67)     ; Link has been severed
(defconstant    EADV            68)     ; Advertise error
(defconstant    ESRMNT          69)     ; Srmount error
(defconstant    ECOMM           70)     ; Communication error on send
(defconstant    EPROTO          71)     ; Protocol error
(defconstant    EMULTIHOP       72)     ; Multihop attempted
(defconstant    EDOTDOT         73)     ; RFS specific error
(defconstant    EBADMSG         74)     ; Not a data message
(defconstant    EOVERFLOW       75)     ; Value too large for defined data type
(defconstant    ENOTUNIQ        76)     ; Name not unique on network
(defconstant    EBADFD          77)     ; File descriptor in bad state
(defconstant    EREMCHG         78)     ; Remote address changed
(defconstant    ELIBACC         79)     ; Can not access a needed shared library
(defconstant    ELIBBAD         80)     ; Accessing a corrupted shared library
(defconstant    ELIBSCN         81)     ; .lib section in a.out corrupted
(defconstant    ELIBMAX         82)     ; Attempting to link in too many shared libraries
(defconstant    ELIBEXEC        83)     ; Cannot exec a shared library directly
(defconstant    EILSEQ          84)     ; Illegal byte sequence
(defconstant    ERESTART        85)     ; Interrupted system call should be restarted
(defconstant    ESTRPIPE        86)     ; Streams pipe error
(defconstant    EUSERS          87)     ; Too many users
(defconstant    ENOTSOCK        88)     ; Socket operation on non-socket
(defconstant    EDESTADDRREQ    89)     ; Destination address required
(defconstant    EMSGSIZE        90)     ; Message too long
(defconstant    EPROTOTYPE      91)     ; Protocol wrong type for socket
(defconstant    ENOPROTOOPT     92)     ; Protocol not available
(defconstant    EPROTONOSUPPORT 93)     ; Protocol not supported
(defconstant    ESOCKTNOSUPPORT 94)     ; Socket type not supported
(defconstant    EOPNOTSUPP      95)     ; Operation not supported on transport endpoint
(defconstant    EPFNOSUPPORT    96)     ; Protocol family not supported
(defconstant    EAFNOSUPPORT    97)     ; Address family not supported by protocol
(defconstant    EADDRINUSE      98)     ; Address already in use
(defconstant    EADDRNOTAVAIL   99)     ; Cannot assign requested address
(defconstant    ENETDOWN        100)    ; Network is down
(defconstant    ENETUNREACH     101)    ; Network is unreachable
(defconstant    ENETRESET       102)    ; Network dropped connection because of reset
(defconstant    ECONNABORTED    103)    ; Software caused connection abort
(defconstant    ECONNRESET      104)    ; Connection reset by peer
(defconstant    ENOBUFS         105)    ; No buffer space available
(defconstant    EISCONN         106)    ; Transport endpoint is already connected
(defconstant    ENOTCONN        107)    ; Transport endpoint is not connected
(defconstant    ESHUTDOWN       108)    ; Cannot send after transport endpoint shutdown
(defconstant    ETOOMANYREFS    109)    ; Too many references: cannot splice
(defconstant    ETIMEDOUT       110)    ; Connection timed out
(defconstant    ECONNREFUSED    111)    ; Connection refused
(defconstant    EHOSTDOWN       112)    ; Host is down
(defconstant    EHOSTUNREACH    113)    ; No route to host
(defconstant    EALREADY        114)    ; Operation already in progress
(defconstant    EINPROGRESS     115)    ; Operation now in progress
(defconstant    ESTALE          116)    ; Stale NFS file handle
(defconstant    EUCLEAN         117)    ; Structure needs cleaning
(defconstant    ENOTNAM         118)    ; Not a XENIX named type file
(defconstant    ENAVAIL         119)    ; No XENIX semaphores available
(defconstant    EISNAM          120)    ; Is a named type file
(defconstant    EREMOTEIO       121)    ; Remote I/O error
(defconstant    EDQUOT          122)    ; Quota exceeded
(defconstant    ENOMEDIUM       123)    ; No medium found
(defconstant    EMEDIUMTYPE     124)    ; Wrong medium type

; ------------------------------ <errnos.h> -----------------------------------

(def-c-call-out __errno_location (:arguments)
                                 (:return-type (c-ptr int))
)
(define-symbol-macro errno (deref (__errno_location)))

; ------------------------------ <errno.h> ------------------------------------

; ============================ <sys/errno.h> ==================================

; ============================== <varargs.h> ==================================
; C compiler dependent

; ============================== <stdarg.h> ===================================
; C compiler dependent

; ============================== <stdlib.h> ===================================

(def-c-struct div_t
  (quot int)
  (rem int)
)
(def-c-struct ldiv_t
  (quot long)
  (rem long)
)
(def-c-struct lldiv_t
  (quot longlong)
  (rem longlong)
)

(defconstant rand-max 2147483647)

(defconstant EXIT_FAILURE 1)
(defconstant EXIT_SUCCESS 0)

(def-c-call-out __ctype_get_mb_cur_max (:arguments)
                                       (:return-type int)
)
(define-symbol-macro MB_CUR_MAX (__ctype_get_mb_cur_max))

(def-c-call-out atof (:arguments (nptr c-string))
                     (:return-type double-float)
)
(def-c-call-out atoi (:arguments (nptr c-string))
                     (:return-type int)
)
(def-c-call-out atol (:arguments (nptr c-string))
                     (:return-type long)
)
(def-c-call-out atoll (:arguments (nptr c-string))
                      (:return-type longlong)
)
(def-c-call-out strtod (:arguments (nptr c-string)
                                   (endptr (c-ptr c-string) :out)
                       )
                       (:return-type double-float)
)
(def-c-call-out strtof (:arguments (nptr c-string)
                                   (endptr (c-ptr c-string) :out)
                       )
                       (:return-type single-float)
)
(def-c-call-out strtol (:arguments (nptr c-string)
                                   (endptr (c-ptr c-string) :out)
                                   (base int)
                       )
                       (:return-type long)
)
(def-c-call-out strtoul (:arguments (nptr c-string)
                                    (endptr (c-ptr c-string) :out)
                                    (base int)
                        )
                        (:return-type ulong)
)
(def-c-call-out strtoq (:arguments (nptr c-string)
                                   (endptr (c-ptr c-string) :out)
                                   (base int)
                       )
                       (:return-type longlong)
)
(def-c-call-out strtouq (:arguments (nptr c-string)
                                    (endptr (c-ptr c-string) :out)
                                    (base int)
                        )
                        (:return-type ulonglong)
)
(def-c-call-out strtoll (:arguments (nptr c-string)
                                    (endptr (c-ptr c-string) :out)
                                    (base int)
                        )
                        (:return-type longlong)
)
(def-c-call-out strtoull (:arguments (nptr c-string)
                                     (endptr (c-ptr c-string) :out)
                                     (base int)
                         )
                         (:return-type ulonglong)
)

(def-c-call-out l64a (:arguments (n long))
                     (:return-type c-string :none)
)

(def-c-call-out a64l (:arguments (s c-string))
                     (:return-type long)
)

(def-c-call-out random (:arguments)
                       (:return-type int32_t)
)
(def-c-call-out srandom (:arguments (seed uint))
                        (:return-type nil)
)
(def-c-call-out initstate (:arguments (seed uint) (statebuf c-pointer) (statelen size_t))
                          (:return-type c-pointer)
)
(def-c-call-out setstate (:arguments (statebuf c-pointer))
                         (:return-type c-pointer)
)

;(def-c-struct random_data ...)
(def-c-call-out random_r (:arguments (buf c-pointer) (result (c-ptr int32_t) :out))
                         (:return-type int)
)
(def-c-call-out srandom_r (:arguments (seed uint) (buf c-pointer))
                          (:return-type int)
)
(def-c-call-out initstate_r (:arguments (seed uint) (statebuf c-pointer) (statelen size_t) (buf c-pointer))
                            (:return-type int)
)
(def-c-call-out setstate_r (:arguments (statebuf c-pointer) (buf c-pointer))
                           (:return-type int)
)

(def-c-call-out rand (:arguments)
                     (:return-type int)
)
(def-c-call-out srand (:arguments (seed uint))
                      (:return-type nil)
)
(def-c-call-out rand_r (:arguments (seed (c-ptr uint)))
                       (:return-type int)
)

(def-c-call-out drand48 (:arguments)
                        (:return-type double-float)
)
(def-c-call-out erand48 (:arguments (xsubi (c-ptr (c-array ushort 3))))
                        (:return-type double-float)
)
(def-c-call-out lrand48 (:arguments)
                        (:return-type long)
)
(def-c-call-out nrand48 (:arguments (xsubi (c-ptr (c-array ushort 3))))
                        (:return-type long)
)
(def-c-call-out mrand48 (:arguments)
                        (:return-type long)
)
(def-c-call-out jrand48 (:arguments (xsubi (c-ptr (c-array ushort 3))))
                        (:return-type long)
)
(def-c-call-out srand48 (:arguments (seedval long))
                        (:return-type nil)
)
(def-c-call-out seed48 (:arguments (seed16v (c-ptr (c-array ushort 3))))
                       (:return-type (c-ptr (c-array ushort 3)) :none)
)
(def-c-call-out lcong48 (:arguments (param (c-ptr (c-array ushort 7))))
                        (:return-type nil)
)

(def-c-call-out drand48_r (:arguments (buffer c-pointer) (result (c-ptr double-float) :out))
                          (:return-type int)
)
(def-c-call-out erand48_r (:arguments (xsubi (c-ptr (c-array ushort 3))) (buffer c-pointer) (result (c-ptr double-float) :out))
                          (:return-type int)
)
(def-c-call-out lrand48_r (:arguments (buffer c-pointer) (result (c-ptr long) :out))
                          (:return-type int)
)
(def-c-call-out nrand48_r (:arguments (xsubi (c-ptr (c-array ushort 3))) (buffer c-pointer) (result (c-ptr long) :out))
                          (:return-type int)
)
(def-c-call-out mrand48_r (:arguments (buffer c-pointer) (result (c-ptr long) :out))
                          (:return-type int)
)
(def-c-call-out jrand48_r (:arguments (xsubi (c-ptr (c-array ushort 3))) (buffer c-pointer) (result (c-ptr long) :out))
                          (:return-type int)
)
(def-c-call-out srand48_r (:arguments (seedval long) (buffer c-pointer))
                          (:return-type int)
)
(def-c-call-out seed48_r (:arguments (seed16v (c-ptr (c-array ushort 3))) (buffer c-pointer))
                         (:return-type int)
)
(def-c-call-out lcong48_r (:arguments (param (c-ptr (c-array ushort 7))) (buffer c-pointer))
                          (:return-type int)
)

(def-c-call-out malloc (:arguments (size size_t))
                       (:return-type c-pointer)
)
(def-c-call-out realloc (:arguments (ptr c-pointer) (size size_t))
                        (:return-type c-pointer)
)
(def-c-call-out calloc (:arguments (nmemb size_t) (size size_t))
                       (:return-type c-pointer)
)
(def-c-call-out free (:arguments (ptr c-pointer))
                     (:return-type nil)
)
(def-c-call-out cfree (:arguments (ptr c-pointer))
                      (:return-type nil)
)
(def-c-call-out valloc (:arguments (size size_t))
                       (:return-type c-pointer)
)

(def-c-call-out abort (:arguments)
                      (:return-type nil)
)

(def-c-call-out atexit (:arguments (func (c-function (:arguments))))
                       (:return-type int)
)

(def-c-call-out on_exit (:arguments (func (c-function (:arguments (status int) (arg c-pointer)))) (arg c-pointer))
                        (:return-type int)
)

(def-c-call-out exit (:arguments (status int))
                     (:return-type nil)
)

(def-c-call-out getenv (:arguments (name c-string))
                       (:return-type c-string)
)

(def-c-call-out putenv (:arguments (string c-string))
                       (:return-type int)
)

(def-c-call-out setenv (:arguments (name c-string) (value c-string) (replace boolean))
                       (:return-type int)
)
(def-c-call-out unsetenv (:arguments (name c-string))
                         (:return-type nil)
)

(def-c-call-out clearenv (:arguments)
                         (:return-type int)
)

(def-c-call-out mktemp (:arguments (template c-string :in :alloca)) ; actually :in-out
                       (:return-type c-string)
)
;(def-c-call-out mkstemp (:arguments (template c-string :in :alloca)) ; actually :in-out
;                        (:return-type int)
;)

(def-c-call-out system (:arguments (command c-string))
                       (:return-type int)
)
(def-c-call-out system? (:arguments (null c-string))
                        (:return-type boolean)
                        (:name "system")
)

(def-c-call-out canonicalize_file_name (:arguments (name c-string))
                                       (:return-type c-string :malloc-free)
)

;(def-c-call-out realpath (:arguments (name c-string) (resolved (c-array char ...)))
;                         (:return-type c-string)
;)

(def-c-type comparison_fn_t (c-function (:arguments (p1 c-pointer) (p2 c-pointer))
                                        (:return-type int)
)                           )

(def-c-call-out bsearch (:arguments (key c-pointer) (base c-pointer) (nmemb size_t) (size size_t) (compar comparison_fn_t))
                        (:return-type c-pointer)
)

(def-c-call-out qsort (:arguments (base c-pointer) (nmemb size_t) (size size_t) (compar comparison_fn_t))
                      (:return-type nil)
)

(def-c-call-out abs (:arguments (x int))
                    (:return-type int)
)
(def-c-call-out labs (:arguments (x long))
                     (:return-type long)
)
(def-c-call-out llabs (:arguments (x longlong))
                      (:return-type longlong)
)

(def-c-call-out div (:arguments (numer int) (denom int))
                    (:return-type div_t)
)
(def-c-call-out ldiv (:arguments (numer long) (denom long))
                     (:return-type ldiv_t)
)
(def-c-call-out lldiv (:arguments (numer longlong) (denom longlong))
                      (:return-type lldiv_t))

(def-c-call-out ecvt (:arguments (value double-float) (ndigit size_t) (decpt (c-ptr int) :out) (sign (c-ptr int) :out))
                     (:return-type c-string :none)
)
(def-c-call-out fcvt (:arguments (value double-float) (ndigit size_t) (decpt (c-ptr int) :out) (sign (c-ptr int) :out))
                     (:return-type c-string :none)
)
;(def-c-call-out gcvt (:arguments (value double-float) (ndigit size_t) (buf (c-array character) :out)) ; ??
;                (:return-type c-string)
;)

(def-c-call-out mblen (:arguments (s c-string) (n size_t))
                      (:return-type int)
)
(def-c-call-out mbtowc (:arguments (pwc (c-ptr wchar_t) :out) (s c-string) (n size_t))
                       (:return-type int)
)
;(def-c-call-out wctomb (:arguments (s (c-ptr (c-array character 10)) :out) (wchar wchar_t)) ; ??
;                       (:return-type int)
;)

;(def-c-call-out mbstowcs (:arguments (pwcs (c-ptr (c-array wchar_t)) :out) (s c-string) (n size_t)) ; ??
;                         (:return-type size_t)
;)
;(def-c-call-out wcstombs (:arguments (s c-string :out) (pwcs (c-ptr (c-array wchar_t))) (n size_t)) ; ??
;                         (:return-type size_t)
;)

(def-c-call-out rpmatch (:arguments (response c-string))
                        (:return-type int)
)

;(def-c-call-out getsubopt (:arguments (optionp (c-ptr c-string)) (tokens (c-array-ptr c-string)) (valuep (c-ptr c-string))) ; ??
;                          (:return-type int)
;)

; ============================== <ctype.h> ====================================

(def-c-call-out isalnum (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isalpha (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out iscntrl (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isdigit (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out islower (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isgraph (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isprint (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out ispunct (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isspace (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isupper (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out isxdigit (:arguments (c int))
                         (:return-type boolean)
)
(def-c-call-out isblank (:arguments (c int))
                        (:return-type boolean)
)

(def-c-call-out tolower (:arguments (c int))
                        (:return-type int)
)
(def-c-call-out toupper (:arguments (c int))
                        (:return-type int)
)

(def-c-call-out isascii (:arguments (c int))
                        (:return-type boolean)
)
(def-c-call-out toascii (:arguments (c int))
                        (:return-type int)
)

(def-c-call-out _tolower (:arguments (c int))
                         (:return-type int)
)
(def-c-call-out _toupper (:arguments (c int))
                         (:return-type int)
)

; ============================== <float.h> ====================================

(defconstant FLT_RADIX 2)
(defconstant FLT_MANT_DIG 24)
(defconstant FLT_DIG 6)
(defconstant FLT_ROUNDS 1)
(defconstant FLT_EPSILON 1.19209290f-07)
(defconstant FLT_MIN_EXP -125)
(defconstant FLT_MIN 1.17549435f-38)
(defconstant FLT_MIN_10_EXP -37)
(defconstant FLT_MAX_EXP 128)
(defconstant FLT_MAX 3.40282347f+38)
(defconstant FLT_MAX_10_EXP 38)

(defconstant DBL_MANT_DIG 53)
(defconstant DBL_DIG 15)
(defconstant DBL_EPSILON 2.2204460492503131d-16)
(defconstant DBL_MIN_EXP -1021)
(defconstant DBL_MIN 2.2250738585072014d-308)
(defconstant DBL_MIN_10_EXP -307)
(defconstant DBL_MAX_EXP 1024)
(defconstant DBL_MAX 1.7976931348623157d+308)
(defconstant DBL_MAX_10_EXP 308)

(defconstant LDBL_MANT_DIG 64)
(defconstant LDBL_DIG 18)
(defconstant LDBL_EPSILON 1.084202172485504434L-19) ; ??
(defconstant LDBL_MIN_EXP -16381)
(defconstant LDBL_MIN 3.3621031431120935063L-4932) ; ??
(defconstant LDBL_MIN_10_EXP -4931)
(defconstant LDBL_MAX_EXP 16384)
(defconstant LDBL_MAX 1.189731495357231765L4932) ; ??
(defconstant LDBL_MAX_10_EXP 4932)

; ============================== <limits.h> ===================================

; ---------------------------- gcc's <limits.h> -------------------------------

(defconstant CHAR_BIT 8)
(defconstant MB_LEN_MAX 1)
(defconstant SCHAR_MIN -128)
(defconstant SCHAR_MAX 127)
(defconstant UCHAR_MAX 255)
(defconstant CHAR_MIN -128)
(defconstant CHAR_MAX 127)
(defconstant SHRT_MIN -32768)
(defconstant SHRT_MAX 32767)
(defconstant USHRT_MAX 65535)
(defconstant INT_MIN -2147483648)
(defconstant INT_MAX 2147483647)
(defconstant UINT_MAX 4294967295)
(defconstant LONG_MIN -2147483648)
(defconstant LONG_MAX 2147483647)
(defconstant ULONG_MAX 4294967295)
(defconstant LONG_LONG_MIN -9223372036854775808)
(defconstant LONG_LONG_MAX 9223372036854775807)
(defconstant ULONG_LONG_MAX 18446744073709551615)

; ---------------------------- <posix1_lim.h> ---------------------------------

(defconstant _POSIX_ARG_MAX 4096)
(defconstant _POSIX_CHILD_MAX 6)
(defconstant _POSIX_LINK_MAX 8)
(defconstant _POSIX_MAX_CANON 255)
(defconstant _POSIX_MAX_INPUT 255)
(defconstant _POSIX_NGROUPS_MAX 0)
(defconstant _POSIX_OPEN_MAX 16)
(defconstant _POSIX_FD_SETSIZE _POSIX_OPEN_MAX)
(defconstant _POSIX_NAME_MAX 14)
(defconstant _POSIX_PATH_MAX 255)
(defconstant _POSIX_PIPE_BUF 512)
(defconstant _POSIX_SSIZE_MAX 32767)
(defconstant _POSIX_STREAM_MAX 8)
(defconstant _POSIX_TZNAME_MAX 3)
(defconstant _POSIX_QLIMIT 1)
(defconstant _POSIX_HIWAT _POSIX_PIPE_BUF)
(defconstant _POSIX_UIO_MAXIOV 16)
(defconstant _POSIX_TTY_NAME_MAX 9)
(defconstant _POSIX_LOGIN_NAME_MAX 9)

; ---------------------------- <linux/limits.h> -------------------------------

(eval-when (load compile eval)
(defconstant NR_OPEN 256)
(defconstant NGROUPS_MAX 32)
(defconstant ARG_MAX 131072)
(defconstant CHILD_MAX 999)
(defconstant OPEN_MAX 256)
(defconstant LINK_MAX 127)
(defconstant MAX_CANON 255)
(defconstant MAX_INPUT 255)
(defconstant NAME_MAX 255)
(defconstant PATH_MAX 1024)
(defconstant PIPE_BUF 4096)
)

; ----------------------------- <local_lim.h> ---------------------------------

(defconstant _POSIX_THREAD_KEYS_MAX 128)
(defconstant PTHREAD_KEYS_MAX 1024)
(defconstant _POSIX_THREAD_DESTRUCTOR_ITERATIONS 4)
(defconstant PTHREAD_DESTRUCTOR_ITERATIONS _POSIX_THREAD_DESTRUCTOR_ITERATIONS)
(defconstant _POSIX_THREAD_THREADS_MAX 64)
(defconstant PTHREAD_THREADS_MAX 1024)

; ---------------------------- <posix1_lim.h> ---------------------------------

(defconstant SSIZE_MAX INT_MAX)
(defconstant NGROUPS_MAX _POSIX_NGROUPS_MAX)

; ---------------------------- <posix2_lim.h> ---------------------------------

(defconstant _POSIX2_BC_BASE_MAX 99)
(defconstant _POSIX2_BC_DIM_MAX 2048)
(defconstant _POSIX2_BC_SCALE_MAX 99)
(defconstant _POSIX2_BC_STRING_MAX 1000)
(defconstant _POSIX2_COLL_WEIGHTS_MAX 255)
(defconstant _POSIX2_EQUIV_CLASS_MAX 255)
(defconstant _POSIX2_EXPR_NEST_MAX 32)
(defconstant _POSIX2_LINE_MAX 2048)
(defconstant _POSIX2_RE_DUP_MAX 255)
(defconstant _POSIX2_CHARCLASS_NAME_MAX 2048)

(defconstant BC_BASE_MAX _POSIX2_BC_BASE_MAX)
(defconstant BC_DIM_MAX _POSIX2_BC_DIM_MAX)
(defconstant BC_SCALE_MAX _POSIX2_BC_SCALE_MAX)
(defconstant BC_STRING_MAX _POSIX2_BC_STRING_MAX)
(defconstant COLL_WEIGHTS_MAX _POSIX2_COLL_WEIGHTS_MAX)
(defconstant EQUIV_CLASS_MAX _POSIX2_EQUIV_CLASS_MAX)
(defconstant EXPR_NEST_MAX _POSIX2_EXPR_NEST_MAX)
(defconstant LINE_MAX _POSIX2_LINE_MAX)
(defconstant RE_DUP_MAX _POSIX2_RE_DUP_MAX)
(defconstant CHARCLASS_NAME_MAX _POSIX2_CHARCLASS_NAME_MAX)

; ----------------------------- <stdio_lim.h> ---------------------------------

(defconstant L_tmpnam 19)
(defconstant TMP_MAX 238328)
(defconstant L_ctermid 9)
(defconstant L_cuserid 9)
(defconstant FILENAME_MAX 1024)
(defconstant FOPEN_MAX 256)

; ----------------------------- <xopen_lim.h> ---------------------------------

(defconstant STREAM_MAX FOPEN_MAX)
(defconstant TZNAME_MAX _POSIX_TZNAME_MAX)
(defconstant _XOPEN_IOV_MAX _POSIX_UIO_MAXIOV)
(defconstant IOV_MAX _XOPEN_IOV_MAX)
(defconstant NL_ARGMAX _POSIX_ARG_MAX)
(defconstant NL_LANGMAX _POSIX2_LINE_MAX)
(defconstant NL_MSGMAX INT_MAX)
(defconstant NL_NMAX INT_MAX)
(defconstant NL_SETMAX INT_MAX)
(defconstant NL_TEXTMAX INT_MAX)
(defconstant NZERO 20)

; ============================== <values.h> ===================================

(defconstant CHARBITS (bitsizeof 'char))
(defconstant SHORTBITS (bitsizeof 'short))
(defconstant INTBITS (bitsizeof 'int))
(defconstant LONGBITS (bitsizeof 'long))
(defconstant PTRBITS (bitsizeof 'c-pointer))
(defconstant DOUBLEBITS (bitsizeof 'double-float))
(defconstant FLOATBITS (bitsizeof 'single-float))
(defconstant MINSHORT -32768)
(defconstant MININT -2147483648)
(defconstant MINLONG -2147483648)
(defconstant MAXSHORT 32767)
(defconstant MAXINT 2147483647)
(defconstant MAXLONG 2147483647)
(defconstant HIBITS -32768)
(defconstant HIBITL -2147483648)
(defconstant MAXDOUBLE DBL_MAX)
(defconstant MAXFLOAT FLT_MAX)
(defconstant MINDOUBLE DBL_MIN)
(defconstant MINFLOAT FLT_MIN)
(defconstant DMINEXP DBL_MIN_EXP)
(defconstant FMINEXP FLT_MIN_EXP)
(defconstant DMAXEXP DBL_MAX_EXP)
(defconstant FMAXEXP FLT_MAX_EXP)
(defconstant BITSPERBYTE CHAR_BIT)

; =============================== <math.h> ====================================

; ----------------------------- <mathcalls.h> ---------------------------------

(def-c-call-out acos (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out asin (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out atan (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out atan2 (:arguments (y double-float) (x double-float))
                      (:return-type double-float)
)
(def-c-call-out cos (:arguments (x double-float))
                    (:return-type double-float)
)
(def-c-call-out sin (:arguments (x double-float))
                    (:return-type double-float)
)
(def-c-call-out tan (:arguments (x double-float))
                    (:return-type double-float)
)

(def-c-call-out cosh (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out sinh (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out tanh (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out acosh (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out asinh (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out atanh (:arguments (x double-float))
                      (:return-type double-float)
)

(def-c-call-out exp (:arguments (x double-float))
                    (:return-type double-float)
)
(def-c-call-out frexp (:arguments (x double-float) (exp (c-ptr int) :out))
                      (:return-type double-float)
)
(def-c-call-out ldexp (:arguments (x double-float) (exp int))
                      (:return-type double-float)
)
(def-c-call-out log (:arguments (x double-float))
                    (:return-type double-float)
)
(def-c-call-out log10 (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out expm1 (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out log1p (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out logb (:arguments (x double-float))
                     (:return-type double-float)
)

(def-c-call-out modf (:arguments (x double-float) (iptr (c-ptr double-float) :out))
                     (:return-type double-float)
)

(def-c-call-out pow (:arguments (x double-float) (y double-float))
                    (:return-type double-float)
)
(def-c-call-out sqrt (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out cbrt (:arguments (x double-float))
                     (:return-type double-float)
)

(def-c-call-out ceil (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out fabs (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out floor (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out fmod (:arguments (x double-float) (y double-float))
                     (:return-type double-float)
)

(def-c-call-out isinf (:arguments (x double-float))
                      (:return-type boolean)
)
(def-c-call-out finite (:arguments (x double-float))
                       (:return-type boolean)
)
(def-c-call-out copysign (:arguments (x double-float) (y double-float))
                         (:return-type double-float)
)
(def-c-call-out scalbn (:arguments (x double-float) (n int))
                       (:return-type double-float)
)
(def-c-call-out drem (:arguments (x double-float) (y double-float))
                     (:return-type double-float)
)
(def-c-call-out significand (:arguments (x double-float))
                            (:return-type double-float)
)

(def-c-call-out isnan (:arguments (x double-float))
                      (:return-type boolean)
)
(def-c-call-out ilogb (:arguments (x double-float))
                      (:return-type int)
)
(def-c-call-out hypot (:arguments (x double-float) (y double-float))
                      (:return-type double-float)
)

(def-c-call-out erf (:arguments (x double-float))
                    (:return-type double-float)
)
(def-c-call-out erfc (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out gamma (:arguments (x double-float))
                      (:return-type double-float)
)
(def-c-call-out j0 (:arguments (x double-float))
                   (:return-type double-float)
)
(def-c-call-out j1 (:arguments (x double-float))
                   (:return-type double-float)
)
(def-c-call-out jn (:arguments (n int) (x double-float))
                   (:return-type double-float)
)
(def-c-call-out lgamma (:arguments (x double-float))
                       (:return-type double-float)
)
(def-c-call-out y0 (:arguments (x double-float))
                   (:return-type double-float)
)
(def-c-call-out y1 (:arguments (x double-float))
                   (:return-type double-float)
)
(def-c-call-out yn (:arguments (n int) (x double-float))
                   (:return-type double-float)
)
;(def-c-call-out gamma_r (:arguments (x double-float)
;                                    (iptr (c-ptr double-float) :out))
;                        (:return-type double-float))
;(def-c-call-out lgamma_r (:arguments (x double-float)
;                                     (iptr (c-ptr double-float) :out))
;                         (:return-type double-float))
(def-c-call-out rint (:arguments (x double-float))
                     (:return-type double-float)
)
(def-c-call-out scalb (:arguments (x double-float) (n double-float))
                      (:return-type double-float)
)
(def-c-call-out nextafter (:arguments (x double-float) (y double-float))
                          (:return-type double-float)
)
(def-c-call-out remainder (:arguments (x double-float) (y double-float))
                          (:return-type double-float)
)

; ----------------------------- <mathcalls.h> ---------------------------------

(def-c-call-out acosf (:arguments (x single-float))
                      (:return-type single-float)
)
(def-c-call-out asinf (:arguments (x single-float))
                      (:return-type single-float)
)
(def-c-call-out atanf (:arguments (x single-float))
                      (:return-type single-float)
)
(def-c-call-out atan2f (:arguments (y single-float) (x single-float))
                       (:return-type single-float)
)
(def-c-call-out cosf (:arguments (x single-float))
                     (:return-type single-float)
)
(def-c-call-out sinf (:arguments (x single-float))
                     (:return-type single-float)
)
(def-c-call-out tanf (:arguments (x single-float))
                     (:return-type single-float)
)

(def-c-call-out coshf (:arguments (x single-float))
                      (:return-type single-float)
)
(def-c-call-out sinhf (:arguments (x single-float))
                      (:return-type single-float)
)
(def-c-call-out tanhf (:arguments (x single-float))
                      (:return-type single-float)
)
(def-c-call-out acoshf (:arguments (x single-float))
                       (:return-type single-float)
)
(def-c-call-out asinhf (:arguments (x single-float))
                       (:return-type single-float)
)
(def-c-call-out atanhf (:arguments (x single-float))
                       (:return-type single-float)
)

(def-c-call-out expf (:arguments (x single-float))
                     (:return-type single-float)
)
(def-c-call-out frexpf (:arguments (x single-float) (exp (c-ptr int) :out))
                       (:return-type single-float)
)
(def-c-call-out ldexpf (:arguments (x single-float) (exp int))
                       (:return-type single-float)
)
(def-c-call-out logf (:arguments (x single-float))
                     (:return-type single-float)
)
(def-c-call-out log10f (:arguments (x single-float))
                       (:return-type single-float)
)
(def-c-call-out expm1f (:arguments (x single-float))
                       (:return-type single-float)
)
(def-c-call-out log1pf (:arguments (x single-float))
                       (:return-type single-float)
)
(def-c-call-out logbf (:arguments (x single-float))
                      (:return-type single-float)
)

(def-c-call-out modff (:arguments (x single-float) (iptr (c-ptr single-float) :out))
                      (:return-type single-float)
)

(def-c-call-out powf (:arguments (x single-float) (y single-float))
                     (:return-type single-float)
)
(def-c-call-out sqrtf (:arguments (x single-float))
                      (:return-type single-float)
)
(def-c-call-out cbrtf (:arguments (x single-float))
                      (:return-type single-float)
)

(def-c-call-out ceilf (:arguments (x single-float))
                      (:return-type single-float)
)
(def-c-call-out fabsf (:arguments (x single-float))
                      (:return-type single-float)
)
(def-c-call-out floorf (:arguments (x single-float))
                       (:return-type single-float)
)
(def-c-call-out fmodf (:arguments (x single-float) (y single-float))
                      (:return-type single-float)
)

(def-c-call-out isinff (:arguments (x single-float))
                       (:return-type boolean)
)
(def-c-call-out finitef (:arguments (x single-float))
                        (:return-type boolean)
)
(def-c-call-out copysignf (:arguments (x single-float) (y single-float))
                          (:return-type single-float)
)
(def-c-call-out scalbnf (:arguments (x single-float) (n int))
                        (:return-type single-float)
)
(def-c-call-out dremf (:arguments (x single-float) (y single-float))
                      (:return-type single-float)
)
(def-c-call-out significandf (:arguments (x single-float))
                             (:return-type single-float)
)

(def-c-call-out isnanf (:arguments (x single-float))
                       (:return-type boolean)
)
(def-c-call-out ilogbf (:arguments (x single-float))
                       (:return-type int)
)
(def-c-call-out hypotf (:arguments (x single-float) (y single-float))
                       (:return-type single-float)
)

(def-c-call-out erff (:arguments (x single-float))
                     (:return-type single-float)
)
(def-c-call-out erfcf (:arguments (x single-float))
                      (:return-type single-float)
)
(def-c-call-out gammaf (:arguments (x single-float))
                       (:return-type single-float)
)
(def-c-call-out j0f (:arguments (x single-float))
                    (:return-type single-float)
)
(def-c-call-out j1f (:arguments (x single-float))
                    (:return-type single-float)
)
(def-c-call-out jnf (:arguments (n int) (x single-float))
                    (:return-type single-float)
)
(def-c-call-out lgammaf (:arguments (x single-float))
                        (:return-type single-float)
)
(def-c-call-out y0f (:arguments (x single-float))
                    (:return-type single-float)
)
(def-c-call-out y1f (:arguments (x single-float))
                    (:return-type single-float)
)
(def-c-call-out ynf (:arguments (n int) (x single-float))
                    (:return-type single-float)
)
;(def-c-call-out gammaf_r (:arguments (x single-float)
;                                     (iptr (c-ptr single-float) :out))
;                         (:return-type single-float))
;(def-c-call-out lgammaf_r (:arguments (x single-float)
;                                      (iptr (c-ptr single-float) :out))
;                          (:return-type single-float))
(def-c-call-out rintf (:arguments (x single-float))
                      (:return-type single-float)
)
(def-c-call-out scalbf (:arguments (x single-float) (n single-float))
                       (:return-type single-float)
)
(def-c-call-out nextafterf (:arguments (x single-float) (y single-float))
                           (:return-type single-float)
)
(def-c-call-out remainderf (:arguments (x single-float) (y single-float))
                           (:return-type single-float)
)

; ------------------------------- <math.h> ------------------------------------

(def-c-var signgam (:type int))

(defconstant HUGE FLT_MAX)

(defconstant M_E         2.7182818284590452354d0)
(defconstant M_LOG2E     1.4426950408889634074d0)
(defconstant M_LOG10E    0.43429448190325182765d0)
(defconstant M_LN2       0.69314718055994530942d0)
(defconstant M_LN10      2.30258509299404568402d0)
(defconstant M_PI        3.14159265358979323846d0)
(defconstant M_PI_2      1.57079632679489661923d0)
(defconstant M_1_PI      0.31830988618379067154d0)
(defconstant M_PI_4      0.78539816339744830962d0)
(defconstant M_2_PI      0.63661977236758134308d0)
(defconstant M_2_SQRTPI  1.12837916709551257390d0)
(defconstant M_SQRT2     1.41421356237309504880d0)
(defconstant M_SQRT1_2   0.70710678118654752440d0)

; ============================= <posix_opt.h> =================================

(defconstant _POSIX_JOB_CONTROL t)
(defconstant _POSIX_SAVED_IDS t)
(defconstant _POSIX_PRIORITY_SCHEDULING t)
(defconstant _POSIX_SYNCHRONIZED_IO t)
(defconstant _POSIX_FSYNC t)
(defconstant _POSIX_MAPPED_FILES t)
(defconstant _POSIX_MEMLOCK t)
(defconstant _POSIX_MEMLOCK_RANGE t)
(defconstant _POSIX_MEMORY_PROTECTION t)
(defconstant _POSIX_POLL t)
(defconstant _POSIX_SELECT t)
(defconstant _POSIX_CHOWN_RESTRICTED t)
(defconstant _POSIX_VDISABLE #\Null)
(defconstant _POSIX_NO_TRUNC t)
(defconstant _XOPEN_SHM t)
(defconstant _POSIX_THREADS t)
(defconstant _POSIX_REENTRANT_FUNCTIONS t)
(defconstant _POSIX_THREAD_SAFE_FUNCTIONS t)
(defconstant _POSIX_THREAD_PRIORITY_SCHEDULING t)

; ============================== <unistd.h> ===================================

; ------------------------------ <unistd.h> -----------------------------------

(defconstant _POSIX_VERSION 199309)
(defconstant _POSIX2_C_VERSION 199912)
(defconstant _POSIX2_C_BIND t)
(defconstant _POSIX2_C_DEV t)
(defconstant _POSIX2_SW_DEV t)
(defconstant _POSIX2_LOCALEDEF t)
(defconstant _XOPEN_VERSION t)
(defconstant _XOPEN_XCU_VERSION t)
(defconstant _XOPEN_XPG2 t)
(defconstant _XOPEN_XPG3 t)
(defconstant _XOPEN_XPG4 t)
(defconstant _XOPEN_UNIX t)
(defconstant _XOPEN_CRYPT t)
(defconstant _XOPEN_ENH_I18N t)

(defconstant STDIN_FILENO 0)
(defconstant STDOUT_FILENO 1)
(defconstant STDERR_FILENO 2)

(defconstant R_OK 4)
(defconstant W_OK 2)
(defconstant X_OK 1)
(defconstant F_OK 0)

(def-c-call-out access (:arguments (name c-string) (type int))
                       (:return-type int)
)

(def-c-call-out euidaccess (:arguments (name c-string) (type int))
                           (:return-type int)
)

(defconstant SEEK_SET 0)
(defconstant SEEK_CUR 1)
(defconstant SEEK_END 2)

(def-c-call-out lseek (:arguments (fd int) (offset off_t) (whence int))
                      (:return-type off_t)
)

(def-c-call-out close (:arguments (fd int))
                      (:return-type int)
)

(def-c-call-out read (:arguments (fd int) (buf c-pointer) (nbytes size_t))
                     (:return-type ssize_t)
                     (:name "full_read")
)
(def-c-call-out write (:arguments (fd int) (buf c-pointer) (nbytes size_t))
                      (:return-type ssize_t)
                      (:name "full_write")
)

(def-c-call-out pipe (:arguments (pipedes (c-ptr (c-array int 2)) :out))
                     (:return-type int)
)

(def-c-call-out alarm (:arguments (seconds uint))
                      (:return-type uint)
)

(def-c-call-out sleep (:arguments (seconds uint))
                      (:return-type uint)
)

(def-c-call-out ualarm (:arguments (value uint) (interval uint))
                       (:return-type uint)
)

(def-c-call-out usleep (:arguments (useconds uint))
                       (:return-type nil)
)

(def-c-call-out pause (:arguments)
                      (:return-type int)
)

(def-c-call-out chown (:arguments (file c-string) (owner uid_t) (group gid_t))
                      (:return-type int)
)
(def-c-call-out fchown (:arguments (fd int) (owner uid_t) (group gid_t))
                       (:return-type int)
)
;(def-c-call-out lchown (:arguments (file c-string) (owner uid_t) (group gid_t))
;                       (:return-type int)
;) ; is a stub (see <gnu/stubs.h>)

(def-c-call-out chdir (:arguments (path c-string))
                      (:return-type int)
)
(def-c-call-out fchdir (:arguments (fd int))
                       (:return-type int)
)

;(def-c-call-out getcwd (:arguments (buf c-string :out) (size size_t)) ; ??
;                       (:return-type c-string)
;)

(def-c-call-out get_current_dir_name (:arguments)
                                     (:return-type c-string :malloc-free)
)

;(def-c-call-out getwd (:arguments (buf c-string :out)) ; ??
;                      (:return-type c-string)
;)

(def-c-call-out dup (:arguments (fd int))
                    (:return-type int)
)

(def-c-call-out dup2 (:arguments (fd int) (fd2 int))
                     (:return-type int)
)

(def-c-var environ (:type (c-array-ptr c-string)) (:read-only t))

;(def-c-call-out execve (:arguments (path c-string) (argv c-pointer) (envp c-pointer)) ; ??
;                       (:return-type int)
;)
;(def-c-call-out fexecve (:arguments (fd int) (argv c-pointer) (envp c-pointer)) ; ??
;                        (:return-type int)
;) ; is a stub (see <gnu/stubs.h>)
;(def-c-call-out execv (:arguments (path c-string) (argv c-pointer)) ; ??
;                      (:return-type int)
;)
(def-c-call-out execle0 (:arguments (path c-string) (argv0 c-string) (null c-string) (envp c-pointer))
                        (:return-type int)
                        (:name "execle")
)
(def-c-call-out execle1 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (null c-string) (envp c-pointer))
                        (:return-type int)
                        (:name "execle")
)
(def-c-call-out execle2 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (argv2 c-string) (null c-string) (envp c-pointer))
                        (:return-type int)
                        (:name "execle")
)
(def-c-call-out execle3 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (argv2 c-string) (argv3 c-string) (null c-string) (envp c-pointer))
                        (:return-type int)
                        (:name "execle")
)
(def-c-call-out execl0 (:arguments (path c-string) (argv0 c-string) (null c-string))
                       (:return-type int)
                       (:name "execl")
)
(def-c-call-out execl1 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (null c-string))
                       (:return-type int)
                       (:name "execl")
)
(def-c-call-out execl2 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (argv2 c-string) (null c-string))
                       (:return-type int)
                       (:name "execl")
)
(def-c-call-out execl3 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (argv2 c-string) (argv3 c-string) (null c-string))
                       (:return-type int)
                       (:name "execl")
)
;(def-c-call-out execvp (:arguments (path c-string) (argv c-pointer)) ; ??
;                       (:return-type int)
;)
(def-c-call-out execlp0 (:arguments (path c-string) (argv0 c-string) (null c-string))
                        (:return-type int)
                        (:name "execlp")
)
(def-c-call-out execlp1 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (null c-string))
                        (:return-type int)
                        (:name "execlp")
)
(def-c-call-out execlp2 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (argv2 c-string) (null c-string))
                        (:return-type int)
                        (:name "execlp")
)
(def-c-call-out execlp3 (:arguments (path c-string) (argv0 c-string) (argv1 c-string) (argv2 c-string) (argv3 c-string) (null c-string))
                        (:return-type int)
                        (:name "execlp")
)

(def-c-call-out nice (:arguments (increment int))
                     (:return-type int)
)

(def-c-call-out _exit (:arguments (status int))
                      (:return-type nil)
)

; ----------------------------- <confname.h> ----------------------------------

(def-c-enum pathconf-name
  _PC_LINK_MAX
  _PC_MAX_CANON
  _PC_MAX_INPUT
  _PC_NAME_MAX
  _PC_PATH_MAX
  _PC_PIPE_BUF
  _PC_CHOWN_RESTRICTED
  _PC_NO_TRUNC
  _PC_VDISABLE
  _PC_SYNC_IO
  _PC_ASYNC_IO
  _PC_PRIO_IO
  _PC_SOCK_MAXBUF
)
(def-c-enum sysconf-name
  _SC_ARG_MAX
  _SC_CHILD_MAX
  _SC_CLK_TCK
  _SC_NGROUPS_MAX
  _SC_OPEN_MAX
  _SC_STREAM_MAX
  _SC_TZNAME_MAX
  _SC_JOB_CONTROL
  _SC_SAVED_IDS
  _SC_REALTIME_SIGNALS
  _SC_PRIORITY_SCHEDULING
  _SC_TIMERS
  _SC_ASYNCHRONOUS_IO
  _SC_PRIORITIZED_IO
  _SC_SYNCHRONIZED_IO
  _SC_FSYNC
  _SC_MAPPED_FILES
  _SC_MEMLOCK
  _SC_MEMLOCK_RANGE
  _SC_MEMORY_PROTECTION
  _SC_MESSAGE_PASSING
  _SC_SEMAPHORES
  _SC_SHARED_MEMORY_OBJECTS
  _SC_AIO_LIST_MAX
  _SC_AIO_MAX
  _SC_AIO_PRIO_DELTA_MAX
  _SC_DELAYTIMER_MAX
  _SC_MQ_OPEN_MAX
  _SC_MQ_PRIO_MAX
  _SC_VERSION
  _SC_PAGESIZE
  _SC_RTSIG_MAX
  _SC_SEM_NSEMS_MAX
  _SC_SEM_VALUE_MAX
  _SC_SIGQUEUE_MAX
  _SC_TIMER_MAX
  _SC_BC_BASE_MAX
  _SC_BC_DIM_MAX
  _SC_BC_SCALE_MAX
  _SC_BC_STRING_MAX
  _SC_COLL_WEIGHTS_MAX
  _SC_EQUIV_CLASS_MAX
  _SC_EXPR_NEST_MAX
  _SC_LINE_MAX
  _SC_RE_DUP_MAX
  _SC_CHARCLASS_NAME_MAX
  _SC_2_VERSION
  _SC_2_C_BIND
  _SC_2_C_DEV
  _SC_2_FORT_DEV
  _SC_2_FORT_RUN
  _SC_2_SW_DEV
  _SC_2_LOCALEDEF
  _SC_PII
  _SC_PII_XTI
  _SC_PII_SOCKET
  _SC_PII_INTERNET
  _SC_PII_OSI
  _SC_POLL
  _SC_SELECT
  _SC_UIO_MAXIOV
  _SC_PII_INTERNET_STREAM
  _SC_PII_INTERNET_DGRAM
  _SC_PII_OSI_COTS
  _SC_PII_OSI_CLTS
  _SC_PII_OSI_M
  _SC_T_IOV_MAX
  _SC_THREADS
  _SC_THREAD_SAFE_FUNCTIONS
  _SC_GETGR_R_SIZE_MAX
  _SC_GETPW_R_SIZE_MAX
  _SC_LOGIN_NAME_MAX
  _SC_TTY_NAME_MAX
  _SC_THREAD_DESTRUCTOR_ITERATIONS
  _SC_THREAD_KEYS_MAX
  _SC_THREAD_STACK_MIN
  _SC_THREAD_THREADS_MAX
  _SC_THREAD_ATTR_STACKADDR
  _SC_THREAD_ATTR_STACKSIZE
  _SC_THREAD_PRIORITY_SCHEDULING
  _SC_THREAD_PRIO_INHERIT
  _SC_THREAD_PRIO_PROTECT
  _SC_THREAD_PROCESS_SHARED
  _SC_NPROCESSORS_CONF
  _SC_NPROCESSORS_ONLN
  _SC_PHYS_PAGES
  _SC_AVPHYS_PAGES
  _SC_ATEXIT_MAX
  _SC_PASS_MAX
  _SC_XOPEN_VERSION
  _SC_XOPEN_XCU_VERSION
  _SC_XOPEN_UNIX
  _SC_XOPEN_CRYPT
  _SC_XOPEN_ENH_I18N
  _SC_XOPEN_SHM
  _SC_2_CHAR_TERM
  _SC_2_C_VERSION
  _SC_2_UPE
  _SC_XOPEN_XPG2
  _SC_XOPEN_XPG3
  _SC_XOPEN_XPG4
  _SC_CHAR_BIT
  _SC_CHAR_MAX
  _SC_CHAR_MIN
  _SC_INT_MAX
  _SC_INT_MIN
  _SC_LONG_BIT
  _SC_WORD_BIT
  _SC_MB_LEN_MAX
  _SC_NZERO
  _SC_SSIZE_MAX
  _SC_SCHAR_MAX
  _SC_SCHAR_MIN
  _SC_SHRT_MAX
  _SC_SHRT_MIN
  _SC_UCHAR_MAX
  _SC_UINT_MAX
  _SC_ULONG_MAX
  _SC_USHRT_MAX
  _SC_NL_ARGMAX
  _SC_NL_LANGMAX
  _SC_NL_MSGMAX
  _SC_NL_NMAX
  _SC_NL_SETMAX
  _SC_NL_TEXTMAX
)
(def-c-enum confstr-name
  _CS_PATH
)

; ------------------------------ <unistd.h> -----------------------------------

(def-c-call-out pathconf (:arguments (path c-string) (name int))
                         (:return-type long)
)
(def-c-call-out fpathconf (:arguments (fd int) (name int))
                          (:return-type long)
)

(def-c-call-out sysconf (:arguments (name int))
                        (:return-type long)
)

;(def-c-call-out confstr (:arguments (name int) (buf c-pointer) (len size_t)) ; ??
;                        (:return-type size_t)
;)

(def-c-call-out getpid (:arguments)
                       (:return-type pid_t)
)

(def-c-call-out getppid (:arguments)
                        (:return-type pid_t)
)

(def-c-call-out getpgrp (:arguments)
                        (:return-type pid_t)
)

(def-c-call-out setpgid (:arguments (pid pid_t) (pgid pid_t))
                        (:return-type int)
)

(def-c-call-out getpgid (:arguments (pid pid_t))
                        (:return-type pid_t)
)

(def-c-call-out setpgrp (:arguments)
                        (:return-type int)
)

(def-c-call-out setsid (:arguments)
                       (:return-type pid_t)
)

(def-c-call-out getsid (:arguments (pid pid_t))
                       (:return-type pid_t)
)

(def-c-call-out getuid (:arguments)
                       (:return-type uid_t)
)

(def-c-call-out geteuid (:arguments)
                        (:return-type uid_t)
)

(def-c-call-out getgid (:arguments)
                       (:return-type gid_t)
)

(def-c-call-out getegid (:arguments)
                        (:return-type gid_t)
)

;(def-c-call-out getgroups (:arguments (size int) (list (c-ptr (c-array gid_t ??)) :out)) ; ??
;                          (:return-type int)
;)

(def-c-call-out group_member (:arguments (gid gid_t))
                             (:return-type boolean)
)

(def-c-call-out setuid (:arguments (uid uid_t))
                       (:return-type int)
)

(def-c-call-out setreuid (:arguments (ruid uid_t) (euid uid_t))
                         (:return-type int)
)

(def-c-call-out seteuid (:arguments (uid uid_t))
                        (:return-type int)
)

(def-c-call-out setgid (:arguments (gid gid_t))
                       (:return-type int)
)

(def-c-call-out setregid (:arguments (rgid gid_t) (egid gid_t))
                         (:return-type int)
)

(def-c-call-out setegid (:arguments (gid gid_t))
                        (:return-type int)
)

(def-c-call-out fork (:arguments)
                     (:return-type pid_t)
)

(def-c-call-out vfork (:arguments)
                      (:return-type pid_t)
)

(def-c-call-out ttyname (:arguments (fd int))
                        (:return-type c-string)
)
;(def-c-call-out ttyname_r (:arguments (fd int) (buf c-pointer) (buflen size_t)) ; ??
;                          (:return-type int)
;)

(def-c-call-out isatty (:arguments (fd int))
                       (:return-type boolean)
)

(def-c-call-out ttyslot (:arguments)
                        (:return-type int)
)

(def-c-call-out link (:arguments (from c-string) (to c-string))
                     (:return-type int)
)

(def-c-call-out symlink (:arguments (from c-string) (to c-string))
                        (:return-type int)
)

;(def-c-call-out readlink (:arguments (path c-string) (buf c-pointer) (len size_t)) ; ??
;                         (:return-type int)
;)

(def-c-call-out unlink (:arguments (path c-string))
                       (:return-type int)
)

(def-c-call-out rmdir (:arguments (path c-string))
                      (:return-type int)
)

(def-c-call-out tcgetpgrp (:arguments (fd int))
                          (:return-type pid_t)
)

(def-c-call-out tcsetpgrp (:arguments (fd int) (pgrp_id pid_t))
                          (:return-type int)
)

(def-c-call-out getlogin (:arguments)
                         (:return-type c-string)
)
;(def-c-call-out getlogin_r (:arguments (name c-pointer) (name_len size_t)) ; ??
;                           (:return-type c-string)
;)

;(def-c-call-out setlogin (:arguments (name c-string))
;                         (:return-type int)
;) ; is a stub (see <gnu/stubs.h>)

(def-c-call-out getopt (:arguments (argc int) (argv c-pointer) (opts c-string)) ; ??
                       (:return-type int)
)
(def-c-var opterr (:type int))
(def-c-var optind (:type int))
(def-c-var optopt (:type int))
(def-c-var optarg (:type c-string))

;(def-c-call-out gethostname (:arguments (name c-pointer) (len size_t)) ; ??
;                            (:return-type int)
;)

;(def-c-call-out sethostname (:arguments (name (c-pointer) (len sizet)) ; ??
;                            (:return-type int)
;)

(def-c-call-out sethostid (:arguments (id long))
                          (:return-type int)
)

;(def-c-call-out getdomainname (:arguments (name c-pointer) (len size_t)) ; ??
;                              (:return-type int)
;)

;(def-c-call-out setdomainname (:arguments (name (c-pointer) (len sizet)) ; ??
;                              (:return-type int)
;)

(def-c-call-out fsync (:arguments (fd int))
                      (:return-type int)
)

(def-c-call-out vhangup (:arguments)
                        (:return-type int)
)

;(def-c-call-out revoke (:arguments (file c-string))
;                       (:return-type int)
;) ; is a stub (see <gnu/stubs.h>)

;(def-c-call-out profil (:arguments (sample_buffer c-pointer) (size size_t) (offset size_t) (scale uint)) ; ??
;                       (:return-type int)
;)

(def-c-call-out acct (:arguments (name c-string))
                     (:return-type int)
)

(def-c-call-out chroot (:arguments (path c-string))
                       (:return-type int)
)

(def-c-call-out getusershell (:arguments)
                             (:return-type c-string)
)
(def-c-call-out endusershell (:arguments)
                             (:return-type nil)
)
(def-c-call-out setusershell (:arguments)
                             (:return-type nil)
)

(def-c-call-out getpass (:arguments (prompt c-string))
                        (:return-type c-string)
)

(def-c-call-out daemon (:arguments (nochdir boolean) (noclose boolean))
                       (:return-type int)
)

(def-c-call-out gethostid (:arguments)
                          (:return-type long)
)

(def-c-call-out sync (:arguments)
                     (:return-type int)
)

(def-c-call-out getpagesize (:arguments)
                            (:return-type size_t)
)

(def-c-call-out truncate (:arguments (file c-string) (length off_t))
                         (:return-type int)
)
(def-c-call-out ftruncate (:arguments (fd int) (length off_t))
                          (:return-type int)
)

(def-c-call-out getdtablesize (:arguments)
                              (:return-type int)
)

(def-c-call-out brk (:arguments (end_data_segment c-pointer))
                    (:return-type int)
)
(def-c-call-out sbrk (:arguments (delta ptrdiff_t))
                     (:return-type int)
)

(def-c-call-out fdatasync (:arguments (fd int))
                          (:return-type int)
)

;(def-c-call-out crypt (:arguments (key c-string) (salt c-string))
;                      (:return-type c-string))
;(def-c-call-out setkey (:arguments (key c-string))
;                       (:return-type nil))
;(def-c-call-out encrypt (:arguments (block (c-ptr (c-array character 64)))
;                                    (edflag boolean))
;                        (:return-type nil))

;(def-c-call-out swab (:arguments (from c-pointer) (to c-pointer) (n ssize_t)) ; ??
;                     (:return-type nil)
;)

; ============================== <fcntl.h> ====================================

; ---------------------------- <linux/fcntl.h> --------------------------------

(defconstant O_ACCMODE    #o003)
(defconstant O_RDONLY       #o0)
(defconstant O_WRONLY       #o1)
(defconstant O_RDWR         #o2)
(defconstant O_CREAT      #o100)
(defconstant O_EXCL       #o200)
(defconstant O_NOCTTY     #o400)
(defconstant O_TRUNC     #o1000)
(defconstant O_APPEND    #o2000)
(defconstant O_NONBLOCK  #o4000)
(defconstant O_NDELAY    O_NONBLOCK)
(defconstant O_SYNC     #o10000)
(defconstant O_FSYNC    O_SYNC)
(defconstant O_ASYNC    #o20000)
(defconstant F_DUPFD    0)
(defconstant F_GETFD    1)
(defconstant F_SETFD    2)
(defconstant F_GETFL    3)
(defconstant F_SETFL    4)
(defconstant F_GETLK    5)
(defconstant F_SETLK    6)
(defconstant F_SETLKW   7)
(defconstant F_SETOWN   8)
(defconstant F_GETOWN   9)

(defconstant FD_CLOEXEC 1)

(defconstant F_RDLCK    0)
(defconstant F_WRLCK    1)
(defconstant F_UNLCK    2)

(defconstant F_EXLCK    4)
(defconstant F_SHLCK    8)

(defconstant LOCK_SH    1)
(defconstant LOCK_EX    2)
(defconstant LOCK_NB    4)
(defconstant LOCK_UN    8)

(def-c-struct flock
  (l_type short)
  (l_whence short)
  (l_start off_t)
  (l_len off_t)
  (l_pid pid_t)
)

; ------------------------------ <fcntl.h> ------------------------------------

(defconstant FNDELAY O_NDELAY)

(def-c-call-out fcntl (:arguments (fildes int) (cmd int) (arg c-pointer)) ; ??
                      (:return-type int)
)

(def-c-call-out open (:arguments (filename c-string) (flags int) (mode mode_t))
                     (:return-type int)
)

(def-c-call-out creat (:arguments (filename c-string) (mode mode_t))
                      (:return-type int)
)

(defconstant F_ULOCK 0)
(defconstant F_LOCK  1)
(defconstant F_TLOCK 2)
(defconstant F_TEST  3)
(def-c-call-out lockf (:arguments (fd int) (cmd int) (len off_t))
                      (:return-type int)
)

; ============================= <sys/stat.h> ==================================

(c-lines "#include <sys/stat.h>~%")

; ------------------------------ <statbuf.h> ----------------------------------

(def-c-struct stat
  (st_dev dev_t)
  (__pad1 ushort)
  (st_ino ino_t)
  (st_mode mode_t)
  (st_nlink nlink_t)
  (st_uid uid_t)
  (st_gid gid_t)
  (st_rdev dev_t)
  (__pad2 ushort)
  (st_size off_t)
  (st_blksize ulong)
  (st_blocks ulong)
  (st_atime time_t)
  (__unused1 ulong)
  (st_mtime time_t)
  (__unused2 ulong)
  (st_ctime time_t)
  (__unused3 ulong)
  (__unused4 ulong)
  (__unused5 ulong)
)

; ----------------------------- <sys/stat.h> ----------------------------------

(defconstant S_IFMT  #o0170000)
(defconstant S_IFDIR  #o040000)
(defconstant S_IFCHR  #o020000)
(defconstant S_IFBLK  #o060000)
(defconstant S_IFREG  #o100000)
(defconstant S_IFIFO  #o010000)
(defconstant S_IFLNK  #o120000)
(defconstant S_IFSOCK #o140000)
(defmacro S_ISDIR (m) `(= (logand ,m S_IFMT) S_IFDIR))
(defmacro S_ISCHR (m) `(= (logand ,m S_IFMT) S_IFCHR))
(defmacro S_ISBLK (m) `(= (logand ,m S_IFMT) S_IFBLK))
(defmacro S_ISREG (m) `(= (logand ,m S_IFMT) S_IFREG))
(defmacro S_ISFIFO (m) `(= (logand ,m S_IFMT) S_IFFIFO))
(defmacro S_ISLNK (m) `(= (logand ,m S_IFMT) S_IFLNK))
(defmacro S_ISSOCK (m) `(= (logand ,m S_IFMT) S_IFSOCK))

(defconstant S_ISUID  #o004000)
(defconstant S_ISGID  #o002000)
(defconstant S_ISVTX  #o001000)

(define-symbol-macro S_IREAD S_IRUSR)
(define-symbol-macro S_IWRITE S_IWUSR)
(define-symbol-macro S_IEXEC S_IXUSR)

(defconstant S_IRUSR  #o000400)
(defconstant S_IWUSR  #o000200)
(defconstant S_IXUSR  #o000100)
(defconstant S_IRWXU  (logior S_IRUSR S_IWUSR S_IXUSR))
(defconstant S_IRGRP  #o000040)
(defconstant S_IWGRP  #o000020)
(defconstant S_IXGRP  #o000010)
(defconstant S_IRWXG  (logior S_IRGRP S_IWGRP S_IXGRP))
(defconstant S_IROTH  #o000004)
(defconstant S_IWOTH  #o000002)
(defconstant S_IXOTH  #o000001)
(defconstant S_IRWXO  (logior S_IROTH S_IWOTH S_IXOTH))

(defconstant ACCESSPERMS (logior S_IRWXU S_IRWXG S_IRWXO))
;(defconstant ALLPERMS (logior S_ISUID S_ISGID S_ISTXT S_IRWXU S_IRWXG S_IRWXO)) ; S_ISTXT is undefined
(defconstant DEFFILEMODE (logior S_IRUSR S_IWUSR S_IRGRP S_IWGRP S_IROTH S_IWOTH))
(defconstant S_BLKSIZE 512)

(def-c-call-out stat (:arguments (filename c-string) (stat_buf (c-ptr stat) :out))
                     (:return-type int)
)

(def-c-call-out fstat (:arguments (fildes int) (stat_buf (c-ptr stat) :out))
                      (:return-type int)
)

(def-c-call-out lstat (:arguments (filename c-string) (stat_buf (c-ptr stat) :out))
                      (:return-type int)
)

(def-c-call-out chmod (:arguments (path c-string) (mode mode_t))
                      (:return-type int)
)

(def-c-call-out fchmod (:arguments (fildes int) (mode mode_t))
                       (:return-type int)
)

(def-c-call-out umask (:arguments (mask mode_t))
                      (:return-type mode_t)
)

(def-c-call-out mkdir (:arguments (path c-string) (mode mode_t))
                      (:return-type int)
)

(def-c-call-out mknod (:arguments (path c-string) (mode mode_t) (dev dev_t))
                      (:return-type int)
)

(def-c-call-out mkfifo (:arguments (path c-string) (mode mode_t))
                       (:return-type int)
)

; ============================== <stdio.h> ====================================

; ---------------------------- <_G_config.h> ----------------------------------

(def-c-type _G_fpos_t off_t)

; ------------------------------ <libio.h> ------------------------------------

(def-c-type _IO_pos_t _G_fpos_t)

; ------------------------------ <stdio.h> ------------------------------------

(defconstant EOF -1)

(defconstant _IOFBF 0)
(defconstant _IOLBF 1)
(defconstant _IONBF 2)

(defconstant SEEK_SET 0)
(defconstant SEEK_CUR 1)
(defconstant SEEK_END 2)

(def-c-type FILE
  (c-struct vector
    ; components unknown
) )
(def-c-type fpos_t _G_fpos_t)

(defconstant P_tmpdir "/tmp")

(def-c-var _IO_stdin_ (:type c-pointer))
(def-c-var _IO_stdout_ (:type c-pointer))
(def-c-var _IO_stderr_ (:type c-pointer))
(define-symbol-macro stdin _IO_stdin_)
(define-symbol-macro stdout _IO_stdout_)
(define-symbol-macro stderr _IO_stderr_)

(def-c-call-out clearerr (:arguments (fp c-pointer))
                         (:return-type nil)
)
(def-c-call-out fclose (:arguments (fp c-pointer))
                       (:return-type int)
)
(def-c-call-out feof (:arguments (fp c-pointer))
                     (:return-type int)
)
(def-c-call-out ferror (:arguments (fp c-pointer))
                       (:return-type int)
)
(def-c-call-out fflush (:arguments (fp c-pointer))
                       (:return-type int)
)
(def-c-call-out fgetc (:arguments (fp c-pointer))
                      (:return-type int)
)
(def-c-call-out fgetpos (:arguments (fp c-pointer) (pos (c-ptr fpos_t) :out))
                        (:return-type int)
)
;(def-c-call-out fgets (:arguments (buf c-pointer) (size int) (fp c-pointer)) ; ??
;                      (:return-type c-string)
;)
(def-c-call-out fopen (:arguments (path c-string) (mode c-string))
                      (:return-type c-pointer)
)
(def-c-call-out fprintf0 (:arguments (fp c-pointer) (format c-string))
                         (:return-type int)
                         (:name "fprintf")
)
(def-c-call-out fprintf1i (:arguments (fp c-pointer) (format c-string) (arg int))
                          (:return-type int)
                          (:name "fprintf")
)
(def-c-call-out fprintf1l (:arguments (fp c-pointer) (format c-string) (arg long))
                          (:return-type int)
                          (:name "fprintf")
)
(def-c-call-out fprintf1d (:arguments (fp c-pointer) (format c-string) (arg double-float))
                          (:return-type int)
                          (:name "fprintf")
)
(def-c-call-out fputc (:arguments (c int) (fp c-pointer))
                      (:return-type int)
)
(def-c-call-out fputs (:arguments (str c-string) (fp c-pointer))
                      (:return-type int)
)
;(def-c-call-out fread (:arguments (ptr c-pointer) (size size_t) (nmemb size_t) (fp c-pointer)) ; ??
;                      (:return-type size_t)
;)
;(def-c-call-out freopen (:arguments (path c-string) (mode c-string) (fp c-pointer :in-out)) ; ??
;                        (:return-type c-pointer)
;)
(def-c-call-out fscanf0 (:arguments (fp c-pointer) (format c-string))
                        (:return-type int)
                        (:name "fscanf")
)
(def-c-call-out fscanf1i (:arguments (fp c-pointer) (format c-string) (arg (c-ptr int) :out))
                         (:return-type int)
                         (:name "fscanf")
)
(def-c-call-out fscanf1l (:arguments (fp c-pointer) (format c-string) (arg (c-ptr long) :out))
                         (:return-type int)
                         (:name "fscanf")
)
(def-c-call-out fscanf1d (:arguments (fp c-pointer) (format c-string) (arg (c-ptr double-float) :out))
                         (:return-type int)
                         (:name "fscanf")
)
(def-c-call-out fseek (:arguments (fp c-pointer) (offset long) (whence int))
                      (:return-type int)
)
(def-c-call-out fsetpos (:arguments (fp c-pointer) (pos (c-ptr fpos_t)))
                        (:return-type int)
)
(def-c-call-out ftell (:arguments (fp c-pointer))
                      (:return-type long)
)
;(def-c-call-out fwrite (:arguments (ptr c-pointer) (size size_t) (nmemb size_t) (fp c-pointer)) ; ??
;                       (:return-type size_t)
;)
(def-c-call-out getc (:arguments (fp c-pointer))
                     (:return-type int)
)
(def-c-call-out getchar (:arguments)
                        (:return-type int)
)
;(def-c-call-out gets (:arguments (buf c-pointer)) ; ??
;                     (:return-type c-string)
;)
(def-c-call-out perror (:arguments (s c-string))
                       (:return-type nil)
)
(def-c-call-out printf0 (:arguments (format c-string))
                        (:return-type int)
                        (:name "printf")
)
(def-c-call-out printf1i (:arguments (format c-string) (arg int))
                         (:return-type int)
                         (:name "printf")
)
(def-c-call-out printf1l (:arguments (format c-string) (arg long))
                         (:return-type int)
                         (:name "printf")
)
(def-c-call-out printf1d (:arguments (format c-string) (arg double-float))
                         (:return-type int)
                         (:name "printf")
)
(def-c-call-out putc (:arguments (c int) (fp c-pointer))
                     (:return-type int)
)
(def-c-call-out putchar (:arguments (c int))
                        (:return-type int)
)
(def-c-call-out puts (:arguments (str c-string))
                     (:return-type int)
)
(def-c-call-out remove (:arguments (path c-string))
                       (:return-type int)
)
(def-c-call-out rename (:arguments (old c-string) (new c-string))
                       (:return-type int)
)
(def-c-call-out rewind (:arguments (fp c-pointer))
                       (:return-type nil)
)
(def-c-call-out scanf0 (:arguments (format c-string))
                       (:return-type int)
                       (:name "scanf")
)
(def-c-call-out scanf1i (:arguments (format c-string) (arg (c-ptr int) :out))
                        (:return-type int)
                        (:name "scanf")
)
(def-c-call-out scanf1l (:arguments (format c-string) (arg (c-ptr long) :out))
                        (:return-type int)
                        (:name "scanf")
)
(def-c-call-out scanf1d (:arguments (format c-string) (arg (c-ptr double-float) :out))
                        (:return-type int)
                        (:name "scanf")
)
(def-c-call-out setbuf (:arguments (fp c-pointer) (buf c-pointer))
                       (:return-type nil)
)
(def-c-call-out setlinebuf (:arguments (fp c-pointer))
                           (:return-type nil)
)
(def-c-call-out setbuffer (:arguments (fp c-pointer) (buf c-pointer) (size int))
                          (:return-type nil)
)
(def-c-call-out setvbuf (:arguments (fp c-pointer) (buf c-pointer) (mode int) (size size_t))
                        (:return-type int)
)
;(def-c-call-out sprintf0 (:arguments (str c-pointer :out) (format c-string)) ; ??
;                         (:return-type int)
;                         (:name "sprintf")
;)
(def-c-call-out sscanf0 (:arguments (str c-string) (format c-string))
                        (:return-type int)
                        (:name "sscanf")
)
(def-c-call-out sscanf1i (:arguments (str c-string) (format c-string) (arg (c-ptr int) :out))
                         (:return-type int)
                         (:name "sscanf")
)
(def-c-call-out sscanf1l (:arguments (str c-string) (format c-string) (arg (c-ptr long) :out))
                         (:return-type int)
                         (:name "sscanf")
)
(def-c-call-out sscanf1d (:arguments (str c-string) (format c-string) (arg (c-ptr double-float) :out))
                         (:return-type int)
                         (:name "sscanf")
)
(def-c-call-out tmpfile (:arguments)
                        (:return-type c-pointer)
)
(def-c-call-out tmpnam (:arguments (s c-string :in :alloca)) ; :in-out ??
                       (:return-type c-string)
)
(def-c-call-out tmpnam_r (:arguments (s c-string :in :alloca)) ; :in-out ??
                         (:return-type c-string)
)
(def-c-call-out tempnam (:arguments (dir c-string) (prefix c-string))
                        (:return-type c-string :malloc-free)
)
(def-c-call-out ungetc (:arguments (c int) (fp c-pointer))
                       (:return-type int)
)
(def-c-call-out fcloseall (:arguments)
                          (:return-type int)
)
(def-c-call-out fdopen (:arguments (fildes int) (mode c-string))
                       (:return-type c-pointer)
)
(def-c-call-out fileno (:arguments (fp c-pointer))
                       (:return-type int)
)
(def-c-call-out popen (:arguments (command c-string) (mode c-string))
                      (:return-type c-pointer)
)
(def-c-call-out pclose (:arguments (fp c-pointer))
                       (:return-type int)
)

(def-c-call-out ctermid (:arguments (null c-string)) ; ??
                        (:return-type c-string)
)
(def-c-call-out cuserid (:arguments (null c-string)) ; ??
                        (:return-type c-string)
)

; getdelim
; getline
; open_memstream

(def-c-var sys_nerr (:type int) (:read-only t))
(def-c-var sys_errlist (:type (c-array c-string 122)) (:read-only t))

; and lots of lock/unlock functions

; ============================== <dirent.h> ===================================

; ----------------------------- <direntry.h> ----------------------------------

(def-c-struct dirent
  (d_ino long)
  (d_off off_t)
  (d_reclen ushort)
  (d_type uchar)
  (d_name (c-array-max character #.(lisp:+ NAME_MAX 1)))
)

; ------------------------------ <dirent.h> -----------------------------------

(defconstant DT_UNKNOWN 0)
(defconstant DT_FIFO 1)
(defconstant DT_CHR 2)
(defconstant DT_DIR 4)
(defconstant DT_BLK 6)
(defconstant DT_REG 8)
(defconstant DT_LNK 10)
(defconstant DT_SOCK 12)

(defmacro IFTODT (mode) `(ash (logand ,mode #o170000) -12))
(defmacro DTTOIF (dirtype) `(ash ,dirtype 12))

(def-c-type DIR
  (c-struct vector
    ; components unknown
) )

(def-c-call-out opendir (:arguments (name c-string))
                        (:return-type c-pointer)
)

(def-c-call-out closedir (:arguments (dirp c-pointer))
                         (:return-type int)
)

(def-c-call-out readdir (:arguments (dirp c-pointer))
                        (:return-type (c-ptr dirent))
)
(def-c-call-out readdir_r (:arguments (dirp c-pointer) (entry (c-ptr dirent) :out :alloca) (result (c-ptr (c-ptr dirent)) :out :alloca)) ; ??
                          (:return-type int)
)

(def-c-call-out rewinddir (:arguments (dirp c-pointer))
                          (:return-type nil)
)

(def-c-call-out dirfd (:arguments (dirp c-pointer))
                      (:return-type int)
)

(defconstant MAXNAMLEN 255)

(def-c-call-out seekdir (:arguments (dirp c-pointer) (pos off_t))
                        (:return-type nil)
)

(def-c-call-out telldir (:arguments (dirp c-pointer))
                        (:return-type off_t)
)

(def-c-call-out scandir (:arguments (dir c-string)
                                    (namelist (c-ptr (c-ptr (c-ptr dirent))) :out)
                                    (select (c-function (:arguments (d c-string))
                                                        (:return-type boolean)
                                    )       )
                                    (compar (c-function (:arguments (d1 (c-ptr (c-ptr dirent))) (d2 (c-ptr (c-ptr dirent))))
                                                        (:return-type int)
                                    )       )
                        )
                        (:return-type int)
)

(def-c-call-out alphasort (:arguments (d1 (c-ptr (c-ptr dirent))) (d2 (c-ptr (c-ptr dirent))))
                          (:return-type int)
)

;(def-c-call-out getdirentries (:arguments (fd int) (buf c-pointer) (nbytes size_t) (basep (c-ptr off_t) :in-out))
;                              (:return-type ssize_t)
;)

; ================================ <pwd.h> ====================================

(def-c-struct passwd
  (pw_name c-string)
  (pw_passwd c-string)
  (pw_uid uid_t)
  (pw_gid gid_t)
  (pw_gecos c-string)
  (pw_dir c-string)
  (pw_shell c-string)
)

;(def-c-call-out __pwdopen (:arguments)
;                          (:return-type c-pointer))

;(def-c-call-out __pwdread (:arguments (stream c-pointer) (p c-pointer))
;                          (:return-type (c-ptr passwd)))

;(def-c-call-out __pwdalloc (:arguments)
;                           (:return-type c-pointer))

;(def-c-call-out __pwdscan (:arguments (p c-pointer)
;                                      (selector
;                                       (c-function
;                                        (:arguments (pwd (c-ptr passwd)))
;                                        (:return-type int))))
;                          (:return-type (c-ptr passwd)))

(def-c-call-out setpwent (:arguments)
                         (:return-type nil)
)

(def-c-call-out endpwent (:arguments)
                         (:return-type nil)
)

(def-c-call-out getpwent (:arguments)
                         (:return-type (c-ptr passwd))
)

(def-c-call-out fgetpwent (:arguments (stream c-pointer))
                          (:return-type (c-ptr passwd))
)

(def-c-call-out putpwent (:arguments (c-ptr passwd) (stream c-pointer))
                         (:return-type int)
)

(def-c-call-out getpwuid (:arguments (uid uid_t))
                         (:return-type (c-ptr passwd))
)

(def-c-call-out getpwnam (:arguments (name c-string))
                         (:return-type (c-ptr passwd))
)

; ... lots of reentrant variants ...

; ================================ <grp.h> ====================================

(def-c-struct group
  (gr_name c-string)
  (gr_passwd c-string)
  (gr_gid gid_t)
  (gr_mem (c-ptr c-string)) ; ??
)

;(def-c-call-out __grpopen (:arguments)
;                          (:return-type c-pointer))

;(def-c-call-out __grpread (:arguments (stream c-pointer) (p c-pointer))
;                          (:return-type (c-ptr group)))

;(def-c-call-out __grpalloc (:arguments)
;                           (:return-type c-pointer))

;(def-c-call-out __grpscan (:arguments (p c-pointer)
;                                      (selector
;                                       (c-function
;                                        (:arguments (grp (c-ptr passwd)))
;                                        (:return-type int))))
;                          (:return-type (c-ptr group)))

(def-c-call-out setgrent (:arguments)
                         (:return-type nil)
)

(def-c-call-out endgrent (:arguments)
                         (:return-type nil)
)

(def-c-call-out getgrent (:arguments)
                         (:return-type (c-ptr group))
)

(def-c-call-out fgetgrent (:arguments (stream c-pointer))
                          (:return-type (c-ptr group))
)

(def-c-call-out getgrgid (:arguments (gid gid_t))
                         (:return-type (c-ptr group))
)

(def-c-call-out getgrnam (:arguments (name c-string))
                         (:return-type (c-ptr group))
)

; ... lots of reentrant variants ...

(def-c-call-out setgroups (:arguments (n size_t) (groups (c-ptr gid_t)))
                          (:return-type int)
)

(def-c-call-out initgroups (:arguments (user c-string) (group gid_t))
                           (:return-type int)
)

; ============================ <sys/utsname.h> ================================

; ---------------------------- <utsnamelen.h> ---------------------------------

(eval-when (load compile eval)
  (defconstant _UTSNAME_LENGTH 65)
  (defconstant _UTSNAME_DOMAIN_LENGTH _UTSNAME_LENGTH)
)

; ---------------------------- <sys/utsname.h> --------------------------------

(eval-when (load compile eval)
  (defconstant _UTSNAME_NODENAME_LENGTH _UTSNAME_LENGTH)
)

(def-c-struct utsname
  (sysname (c-array-max character #._UTSNAME_LENGTH))
  (nodename (c-array-max character #._UTSNAME_NODENAME_LENGTH))
  (release (c-array-max character #._UTSNAME_LENGTH))
  (version (c-array-max character #._UTSNAME_LENGTH))
  (machine (c-array-max character #._UTSNAME_LENGTH))
  (domainname (c-array-max character #._UTSNAME_DOMAIN_LENGTH))
)

(defconstant SYS_NMLN _UTSNAME_LENGTH)

(def-c-call-out uname (:arguments (utsbuf (c-ptr utsname) :out))
                      (:return-type int)
)

; ============================= <termios.h> ===================================

; ----------------------------- <termbits.h> ----------------------------------

(def-c-type cc_t uchar)
(def-c-type speed_t uint)
(def-c-type tcflag_t uint)

(eval-when (load compile eval)
  (defconstant NCCS 32)
)
(def-c-struct termios
  (c_iflag tcflag_t)
  (c_oflag tcflag_t)
  (c_cflag tcflag_t)
  (c_lflag tcflag_t)
  (c_line cc_t)
  (c_cc (c-array cc_t #.NCCS))
  (c_ispeed speed_t)
  (c_ospeed speed_t)
)

; c_cc characters
(defconstant VINTR 0)
(defconstant VQUIT 1)
(defconstant VERASE 2)
(defconstant VKILL 3)
(defconstant VEOF 4)
(defconstant VTIME 5)
(defconstant VMIN 6)
(defconstant VSWTC 7)
(defconstant VSTART 8)
(defconstant VSTOP 9)
(defconstant VSUSP 10)
(defconstant VEOL 11)
(defconstant VREPRINT 12)
(defconstant VDISCARD 13)
(defconstant VWERASE 14)
(defconstant VLNEXT 15)
(defconstant VEOL2 16)

; c_iflag bits
(defconstant IGNBRK  #o000001)
(defconstant BRKINT  #o000002)
(defconstant IGNPAR  #o000004)
(defconstant PARMRK  #o000010)
(defconstant INPCK   #o000020)
(defconstant ISTRIP  #o000040)
(defconstant INLCR   #o000100)
(defconstant IGNCR   #o000200)
(defconstant ICRNL   #o000400)
(defconstant IUCLC   #o001000)
(defconstant IXON    #o002000)
(defconstant IXANY   #o004000)
(defconstant IXOFF   #o010000)
(defconstant IMAXBEL #o020000)

; c_oflag bits
(defconstant OPOST   #o000001)
(defconstant OLCUC   #o000002)
(defconstant ONLCR   #o000004)
(defconstant OCRNL   #o000010)
(defconstant ONOCR   #o000020)
(defconstant ONLRET  #o000040)
(defconstant OFILL   #o000100)
(defconstant OFDEL   #o000200)
(defconstant NLDLY   #o000400)
(defconstant   NL0   #o000000)
(defconstant   NL1   #o000400)
(defconstant CRDLY   #o003000)
(defconstant   CR0   #o000000)
(defconstant   CR1   #o001000)
(defconstant   CR2   #o002000)
(defconstant   CR3   #o003000)
(defconstant TABDLY  #o014000)
(defconstant   TAB0  #o000000)
(defconstant   TAB1  #o004000)
(defconstant   TAB2  #o010000)
(defconstant   TAB3  #o014000)
(defconstant   XTABS #o014000)
(defconstant BSDLY   #o020000)
(defconstant   BS0   #o000000)
(defconstant   BS1   #o020000)
(defconstant VTDLY   #o040000)
(defconstant   VT0   #o000000)
(defconstant   VT1   #o040000)
(defconstant FFDLY   #o100000)
(defconstant   FF0   #o000000)
(defconstant   FF1   #o100000)

; c_cflag bit meaning
(defconstant CBAUD   #o010017)
(defconstant  B0     #o000000)
(defconstant  B50    #o000001)
(defconstant  B75    #o000002)
(defconstant  B110   #o000003)
(defconstant  B134   #o000004)
(defconstant  B150   #o000005)
(defconstant  B200   #o000006)
(defconstant  B300   #o000007)
(defconstant  B600   #o000010)
(defconstant  B1200  #o000011)
(defconstant  B1800  #o000012)
(defconstant  B2400  #o000013)
(defconstant  B4800  #o000014)
(defconstant  B9600  #o000015)
(defconstant  B19200 #o000016)
(defconstant  B38400 #o000017)
(defconstant EXTA B19200)
(defconstant EXTB B38400)
(defconstant CSIZE   #o000060)
(defconstant   CS5   #o000000)
(defconstant   CS6   #o000020)
(defconstant   CS7   #o000040)
(defconstant   CS8   #o000060)
(defconstant CSTOPB  #o000100)
(defconstant CREAD   #o000200)
(defconstant PARENB  #o000400)
(defconstant PARODD  #o001000)
(defconstant HUPCL   #o002000)
(defconstant CLOCAL  #o004000)
(defconstant CBAUDEX #o010000)
(defconstant  B57600  #o010001)
(defconstant  B115200 #o010002)
(defconstant  B230400 #o010003)
(defconstant CIBAUD    #o02003600000)
(defconstant CRTSCTS   #o20000000000)

; c_lflag bits
(defconstant ISIG    #o000001)
(defconstant ICANON  #o000002)
(defconstant XCASE   #o000004)
(defconstant ECHO    #o000010)
(defconstant ECHOE   #o000020)
(defconstant ECHOK   #o000040)
(defconstant ECHONL  #o000100)
(defconstant NOFLSH  #o000200)
(defconstant TOSTOP  #o000400)
(defconstant ECHOCTL #o001000)
(defconstant ECHOPRT #o002000)
(defconstant ECHOKE  #o004000)
(defconstant FLUSHO  #o010000)
(defconstant PENDIN  #o040000)
(defconstant IEXTEN  #o100000)

; tcflow() and TCXONC use these
(defconstant TCOOFF          0)
(defconstant TCOON           1)
(defconstant TCIOFF          2)
(defconstant TCION           3)

; tcflush() and TCFLSH use these
(defconstant TCIFLUSH        0)
(defconstant TCOFLUSH        1)
(defconstant TCIOFLUSH       2)

; tcsetattr uses these
(defconstant TCSANOW         0)
(defconstant TCSADRAIN       1)
(defconstant TCSAFLUSH       2)

; ----------------------------- <termios.h> -----------------------------------

(defmacro CCEQ (val c) `(and (= ,c ,val) (/= ,val _POSIX_VDISABLE)))

(def-c-call-out cfgetospeed (:arguments (termios_p (c-ptr termios)))
                            (:return-type speed_t)
)

(def-c-call-out cfgetispeed (:arguments (termios_p (c-ptr termios)))
                            (:return-type speed_t)
)

(def-c-call-out cfsetospeed (:arguments (termios_p (c-ptr termios) :in-out) (speed speed_t))
                            (:return-type int)
)

(def-c-call-out cfsetispeed (:arguments (termios_p (c-ptr termios) :in-out) (speed speed_t))
                            (:return-type int)
)

(def-c-call-out cfsetspeed (:arguments (termios_p (c-ptr termios) :in-out) (speed speed_t))
                           (:return-type int)
)

(def-c-call-out tcgetattr (:arguments (fildes int) (termios_p (c-ptr termios) :out))
                          (:return-type int)
)

(def-c-call-out tcsetattr (:arguments (fildes int) (optional_actions int) (termios_p (c-ptr termios)))
                          (:return-type int)
)

(def-c-call-out cfmakeraw (:arguments (t (c-ptr termios) :in-out))
                          (:return-type nil)
)

(def-c-call-out tcsendbreak (:arguments (fildes int) (duration int))
                            (:return-type int)
)

(def-c-call-out tcdrain (:arguments (fildes int))
                        (:return-type int)
)

(def-c-call-out tcflush (:arguments (fildes int) (queue_selector int))
                        (:return-type int)
)

(def-c-call-out tcflow (:arguments (fildes int) (action int))
                       (:return-type int)
)

; ------------------------- <sys/ttydefaults.h> -------------------------------

; lots of old stuff

; ============================== <string.h> ===================================

(def-c-call-out strerror (:arguments (errnum int))
                         (:return-type c-string :none)
)

; ============================= <sys/ioctl.h> =================================

; ---------------------------- <ioctl-types.h> --------------------------------

(def-c-struct winsize
  (ws_row ushort)
  (ws_col ushort)
  (ws_xpixel ushort)
  (ws_ypixel ushort)
)

(eval-when (load compile eval)
  (defconstant NCC 8)
)
(def-c-struct termio
  (c_iflag ushort)
  (c_oflag ushort)
  (c_cflag ushort)
  (c_lflag ushort)
  (c_line uchar)
  (c_cc (c-array uchar #.NCC))
)

; modem lines
(defconstant TIOCM_LE        #x001)
(defconstant TIOCM_DTR       #x002)
(defconstant TIOCM_RTS       #x004)
(defconstant TIOCM_ST        #x008)
(defconstant TIOCM_SR        #x010)
(defconstant TIOCM_CTS       #x020)
(defconstant TIOCM_CAR       #x040)
(defconstant TIOCM_RNG       #x080)
(defconstant TIOCM_DSR       #x100)
(defconstant TIOCM_CD        TIOCM_CAR)
(defconstant TIOCM_RI        TIOCM_RNG)

; line disciplines
(defconstant N_TTY           0)
(defconstant N_SLIP          1)
(defconstant N_MOUSE         2)
(defconstant N_PPP           3)
(defconstant N_STRIP         4)
(defconstant N_AX25          5)

; ------------------------- <sys/ttydefaults.h> -------------------------------

; lots of old stuff

; ----------------------------- <sys/ioctl.h> ---------------------------------

(def-c-call-out ioctl (:arguments (fd int) (request int) (arg c-pointer))
                      (:return-type int)
)

#|
; ============================== <.h> ====================================
; =============================================================================

(def-c-call-out (:arguments
                (:return-type
)

(defconstant )
|#
; =============================================================================

(lisp:in-package "LISP")

(eval-when (compile eval)
  (delete-package "LINUX-AUX")
)

