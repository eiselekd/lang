/* stdc.h -- macros to make source compile on both ANSI C and K&R C
   compilers. */

/* Copyright (C) 1993 Free Software Foundation, Inc.

   This file is part of GNU Bash, the Bourne Again SHell.

   Bash is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   Bash is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with Bash; see the file COPYING.  If not, write to the Free
   Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#if !defined (_RL_STDC_H_)
#define _RL_STDC_H_

/* Adapted from BSD /usr/include/sys/cdefs.h. */

/* A function can be defined using prototypes and compile on both ANSI C
   and traditional C compilers with something like this:
	extern char *func _PROTO((char *, char *, int)); */

#ifndef _PROTO

#if defined (__GNUC__) || defined (__STDC__) || defined (__cplusplus)
#define _PROTO(args) args
#else
#define _PROTO(args) ()
#endif

#endif

#endif /* !_RL_STDC_H_ */
