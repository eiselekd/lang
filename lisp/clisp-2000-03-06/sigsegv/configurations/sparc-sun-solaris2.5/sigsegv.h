#ifndef _SIGSEGV_H
#define _SIGSEGV_H

/*
 * Copyright 1998-1999 Bruno Haible, <haible@clisp.cons.org>
 *
 * This is free software distributed under the GNU General Public Licence
 * described in the file COPYING. Contact the author if you don't have this
 * or can't live with it. There is ABSOLUTELY NO WARRANTY, explicit or implied,
 * on this software.
 */

/*
 * Portability section:
 * HAVE_SIGSEGV_RECOVERY  is defined if the system supports catching SIGSEGV.
 * HAVE_STACK_OVERFLOW_RECOVERY  is defined if stack overflow can be caught.
 */
#if defined(__linux__) && (defined(i386) || defined(__i386)) /* Linux */
#define HAVE_SIGSEGV_RECOVERY
#endif
#if defined(__NetBSD__) || defined(__FreeBSD__) /* NetBSD, FreeBSD */
#define HAVE_SIGSEGV_RECOVERY
#endif
#if defined(__linux__) && defined(sparc) && 0 /* Linux */
/* This used to work. But in Linux-2.0.33 and 2.2.0 a task's tss.sig_address
 * is passed to the signal handler only if old SunOS signal frames are in use,
 * and there is no reliable way to force this. Blech. */
#define HAVE_SIGSEGV_RECOVERY
#endif
#if defined(sun) && defined(unix) && (defined(mc68020) || defined(sparc) || (defined(i386) || defined(__i386))) /* SunOS */
#define HAVE_SIGSEGV_RECOVERY
#endif
#if (defined(sgi) || defined(__sgi)) && (defined(SYSTYPE_SVR4) || defined(__SYSTYPE_SVR4)) /* Irix 5 */
#define HAVE_SIGSEGV_RECOVERY
#endif
#if defined(__osf__) /* OSF/1 */
#define HAVE_SIGSEGV_RECOVERY
#endif
#if defined(_AIX) /* AIX */
#define HAVE_SIGSEGV_RECOVERY
#endif
#if defined(NeXT) /* NeXTstep */
#define HAVE_SIGSEGV_RECOVERY
#endif
#if defined(_WIN32) && !defined(__BORLANDC__) /* Win32, but not Borland C */
#define HAVE_SIGSEGV_RECOVERY
#define HAVE_STACK_OVERFLOW_RECOVERY
#endif

#undef HAVE_SIGSEGV_RECOVERY

#if defined(__linux__) || (defined(sun) && defined(unix) && (defined(mc68020) || defined(sparc) || (defined(i386) || defined(__i386)))) || ((defined(sgi) || defined(__sgi)) && (defined(SYSTYPE_SVR4) || defined(__SYSTYPE_SVR4))) || defined(__osf__) /* Linux, SunOS, Irix, OSF/1 */
#define HAVE_STACK_OVERFLOW_RECOVERY
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* -------------------------------------------------------------------------- */

/*
 * The type of a global SIGSEGV handler.
 * The fault address is passed as argument.
 * The access type (read access or write access) is not passed; your handler
 * has to know itself how to distinguish these two cases.
 * The second argument is 0, meaning it could also be a stack overflow, or 1,
 * meaning the handler should seriously try to fix the fault.
 * The return value should be nonzero if the handler has done its job
 * and no other handler should be called, or 0 if the handler declines
 * responsibility for the given address.
 */
typedef int (*sigsegv_handler_t) (void* fault_address, int serious);

/*
 * Installs a global SIGSEGV handler.
 * This should be called once only, and it ignores any previously installed
 * SIGSEGV handler.
 * Returns 0 on success, or -1 if the system doesn't support catching SIGSEGV.
 */
extern int sigsegv_install_handler (sigsegv_handler_t handler);

/*
 * Deinstalls the global SIGSEGV handler.
 * This goes back to the state where no SIGSEGV handler is installed.
 */
extern void sigsegv_deinstall_handler (void);

/*
 * Prepares leaving a SIGSEGV handler (through longjmp or similar means).
 * On Unix, this unblocks some signals.
 */
extern void sigsegv_leave_handler (void);

/*
 * The type of a context passed to a stack overflow handler.
 */
#if defined(__linux__) && (defined(i386) || defined(__i386) || defined(m68k) || defined(__m68k) || defined(mips) || defined(__mips) || defined(sparc) || defined(__sparc) || defined(alpha) || defined(__alpha) || defined(arm) || defined(__arm))
typedef struct sigcontext * stackoverflow_context_t;
#else
typedef void * stackoverflow_context_t;
#endif

/*
 * The type of a stack overflow handler.
 * Such a handler should perform a longjmp call in order to reduce the amount
 * of stack needed. It must not return.
 * The emergency argument is 0 when the stack could be repared, or 1 if the
 * application should better save its state and exit now.
 */
typedef void (*stackoverflow_handler_t) (int emergency, stackoverflow_context_t scp);

/*
 * Installs a stack overflow handler.
 * The extra_stack argument is a pointer to a pre-allocated area used as a
 * stack for executing the handler. It is typically allocated by use of
 * `alloca' during `main'. Its size should be sufficiently large (typically
 * 16 KB).
 * Returns 0 on success, or -1 if the system doesn't support catching stack
 * overflow.
 */
extern int stackoverflow_install_handler (stackoverflow_handler_t handler,
                                          void* extra_stack, unsigned long extra_stack_size);

/*
 * Deinstalls the stack overflow handler.
 */
extern void stackoverflow_deinstall_handler (void);

/* -------------------------------------------------------------------------- */

/*
 * The following structure and functions permit to define different SIGSEGV
 * policies on different address ranges.
 */

/*
 * The type of a local SIGSEGV handler.
 * The fault address is passed as argument.
 * The second argument is fixed arbitrary user data.
 * The return value should be nonzero if the handler has done its job
 * and no other handler should be called, or 0 if the handler declines
 * responsibility for the given address.
 */
typedef int (*sigsegv_area_handler_t) (void* fault_address, void* user_arg);

/*
 * This structure represents a table of memory areas (address range intervals),
 * with an local SIGSEGV handler for each.
 */
typedef
struct sigsegv_dispatcher {
  void* tree;
}
sigsegv_dispatcher;

/*
 * Initializes a sigsegv_dispatcher structure.
 */
extern void sigsegv_init (sigsegv_dispatcher* dispatcher);

/*
 * Adds a local SIGSEGV handler to a sigsegv_dispatcher structure.
 * It will cover the interval [address..address+len-1].
 * Returns a "ticket" that can be used to remove the handler later.
 */
extern void* sigsegv_register (sigsegv_dispatcher* dispatcher,
                               void* address, unsigned long len,
                               sigsegv_area_handler_t handler, void* handler_arg);

/*
 * Removes a local SIGSEGV handler.
 */
extern void sigsegv_unregister (sigsegv_dispatcher* dispatcher, void* ticket);

/*
 * Call the local SIGSEGV handler responsible for the given fault address.
 * Return the handler's return value. 0 means that no handler has been found,
 * or that a handler was found but declined responsibility.
 */
extern int sigsegv_dispatch (sigsegv_dispatcher* dispatcher, void* fault_address);

/* -------------------------------------------------------------------------- */

#ifdef __cplusplus
}
#endif

#endif /* _SIGSEGV_H */
