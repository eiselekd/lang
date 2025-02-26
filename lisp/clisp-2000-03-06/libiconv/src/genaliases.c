/* Copyright (C) 1999 Free Software Foundation, Inc.
   This file is part of the GNU ICONV Library.

   The GNU ICONV Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU ICONV Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU ICONV Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* Creates the aliases.gperf table. */

#include <stdio.h>
#include <stdlib.h>

static void emit_encoding (const char* const* names, size_t n, const char* c_name)
{
  for (; n > 0; names++, n--) {
    /* Output *names in upper case. */
    const char* s = *names;
    for (; *s; s++) {
      unsigned char c = * (unsigned char *) s;
      if (c >= 0x80)
        exit(1);
      if (c >= 'a' && c <= 'z')
        c -= 'a'-'A';
      putc(c, stdout);
    }
    printf(", ei_%s\n", c_name);
  }
}

int main ()
{
  printf("struct alias { const char* name; unsigned int encoding_index; };\n");
  printf("%%%%\n");

#define DEFENCODING(xxx_names,xxx,xxx_ifuncs,xxx_ofuncs1,xxx_ofuncs2) \
  {                                                           \
    static const char* const names[] = BRACIFY xxx_names;     \
    emit_encoding(names,sizeof(names)/sizeof(names[0]),#xxx); \
  }
#define BRACIFY(...) { __VA_ARGS__ }
#include "encodings.def"
#undef BRACIFY
#undef DEFENCODING

  fflush(stdout);
  if (ferror(stdout))
    exit(1);
  exit(0);
}
