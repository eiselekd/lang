/* Trampoline accessor test */

/*
 * Copyright 1995-1999 Bruno Haible, <haible@clisp.cons.org>
 *
 * This is free software distributed under the GNU General Public Licence
 * described in the file COPYING. Contact the author if you don't have this
 * or can't live with it. There is ABSOLUTELY NO WARRANTY, explicit or implied,
 * on this software.
 */

#include <stdio.h>

#include "trampoline_r.h"

typedef int (*function)();

#if defined(__STDC__) || defined(__GNUC__) || defined(__cplusplus)
int f (int x)
#else
int f (x)
  int x;
#endif
{ return x; }

#define MAGIC1  0x9db9af42
#define MAGIC2  0x614a13c9

int main ()
{
  function cf = alloc_trampoline_r(&f, (void*)MAGIC1, (void*)MAGIC2);
  if (is_trampoline_r(&main))
    { printf("is_trampoline_r(&main) returns true!\n"); exit(1); }
  if (!is_trampoline_r(cf))
    { printf("is_trampoline_r() returns false!\n"); exit(1); }
  if (trampoline_r_address(cf) != &f)
    { printf("trampoline_r_address() doesn't work!\n"); exit(1); }
  if (trampoline_r_data0(cf) != (void*)MAGIC1)
    { printf("trampoline_r_data0() doesn't work!\n"); exit(1); }
  if (trampoline_r_data1(cf) != (void*)MAGIC2)
    { printf("trampoline_r_data1() doesn't work!\n"); exit(1); }
  printf("test2 passed.\n");
  exit(0);
}
