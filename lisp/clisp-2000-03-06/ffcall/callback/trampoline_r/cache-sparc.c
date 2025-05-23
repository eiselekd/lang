/* Instruction cache flushing for sparc */

/*
 * Copyright 1996-1999 Bruno Haible, <haible@clisp.cons.org>
 *
 * This is free software distributed under the GNU General Public Licence
 * described in the file COPYING. Contact the author if you don't have this
 * or can't live with it. There is ABSOLUTELY NO WARRANTY, explicit or implied,
 * on this software.
 */

/*
 * This assumes that the range [first_addr..last_addr] lies in at most two
 * cache lines.
 */
void __TR_clear_cache_2 (char* first_addr, char* last_addr)
{
  asm volatile ("iflush %0+0;" /* the +0 is needed by gas, says gforth-0.3.0 */
                "iflush %0+8"
                : : "r" (first_addr));
}
