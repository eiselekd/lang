/* Copyright (C) 2000 Free Software Foundation, Inc.
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

/* Creates the flags.h include file. */

#include <stdio.h>
#include <stdlib.h>

#include "converters.h"

static void emit_encoding (struct wctomb_funcs * ofuncs, const char* c_name)
{
  /* Prepare a converter struct. */
  struct conv_struct conv;
  memset(&conv,'\0',sizeof(conv));
  conv.ofuncs = *ofuncs;

  {
    /* See whether the encoding can encode the accents and quotation marks. */
    wchar_t probe[6] = { 0x0060, 0x00b4, 0x2018, 0x2019, 0x3131, 0x3163, };
    int res[6];
    int i;
    for (i = 0; i < 6; i++) {
      unsigned char buf[10];
      memset(&conv.ostate,'\0',sizeof(state_t));
      res[i] = (conv.ofuncs.xxx_wctomb(&conv,buf,probe[i],sizeof(buf)) != 0);
    }
    printf("#define ei_%s_oflags (",c_name);
    {
      int first = 1;
      if (res[0] && res[1]) {
        printf("HAVE_ACCENTS");
        first = 0;
      }
      if (res[2] && res[3]) {
        if (!first) printf(" | ");
        printf("HAVE_QUOTATION_MARKS");
        first = 0;
      }
      if (res[4] && res[5]) {
        if (!first) printf(" | ");
        printf("HAVE_HANGUL_JAMO");
        first = 0;
      }
      if (first) printf("0");
    }
    printf(")\n");
  }
}

int main ()
{
  int bitmask = 1;
  printf("/* Generated automatically by genflags. */\n");
  printf("\n");
  printf("/* Set if the encoding can encode\n");
  printf("   the acute and grave accents U+00B4 and U+0060. */\n");
  printf("#define HAVE_ACCENTS %d\n",bitmask);
  printf("\n");
  bitmask = bitmask << 1;
  printf("/* Set if the encoding can encode\n");
  printf("   the single quotation marks U+2018 and U+2019. */\n");
  printf("#define HAVE_QUOTATION_MARKS %d\n",bitmask);
  printf("\n");
  bitmask = bitmask << 1;
  printf("/* Set if the encoding can encode\n");
  printf("   the double-width Hangul letters (Jamo) U+3131 to U+3163. */\n");
  printf("#define HAVE_HANGUL_JAMO %d\n",bitmask);
  printf("\n");

#define DEFENCODING(xxx_names,xxx,xxx_ifuncs,xxx_ofuncs1,xxx_ofuncs2) \
  {                                                       \
    struct wctomb_funcs ofuncs = xxx_ofuncs1,xxx_ofuncs2; \
    emit_encoding(&ofuncs,#xxx);                          \
  }
#include "encodings.def"
#undef DEFENCODING

  fflush(stdout);
  if (ferror(stdout))
    exit(1);
  exit(0);
}
