/* -*- C -*-
 * $Id: defs.h,v 1.2.2.2 2002/01/16 09:22:41 matz Exp $
 */

#ifndef DEFS_H
#define DEFS_H

#include "ruby.h"
#include <sys/types.h>

#if defined(HAVE_SYS_CDEFS_H)
# include <sys/cdefs.h>
#endif
#if !defined(__BEGIN_DECLS)
# define __BEGIN_DECLS
# define __END_DECLS
#endif

#if defined(HAVE_INTTYPES_H)
# include <inttypes.h>
#else
  typedef unsigned char uint8_t;
  typedef unsigned int  uint32_t;
# if SIZEOF_LONG == 8
  typedef unsigned long uint64_t;
# elif defined(__GNUC__)
  typedef unsigned long long uint64_t;
# elif defined(_MSC_VER)
  typedef unsigned _int64 uint64_t;
# else
#  define NO_UINT64_T
# endif
#endif

#endif /* DEFS_H */
