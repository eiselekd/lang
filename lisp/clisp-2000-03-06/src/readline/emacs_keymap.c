/* emacs_keymap.c -- the keymap for emacs_mode in readline (). */

/* Copyright (C) 1987, 1989, 1992 Free Software Foundation, Inc.

   This file is part of the GNU Readline Library, a library for
   reading lines of text with interactive input and history editing.

   The GNU Readline Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 1, or
   (at your option) any later version.

   The GNU Readline Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   The GNU General Public License is often shipped with GNU software, and
   is generally kept in a file called COPYING or LICENSE.  If you do not
   have a copy of the license, write to the Free Software Foundation,
   675 Mass Ave, Cambridge, MA 02139, USA. */

#if !defined (BUFSIZ)
#include <stdio.h>
#endif /* !BUFSIZ */

#include "readline.h"

/* An array of function pointers, one for each possible key.
   If the type byte is ISKMAP, then the pointer is the address of
   a keymap. */

KEYMAP_ENTRY_ARRAY emacs_standard_keymap = {

  /* Control keys. */
#if defined(__MSDOS__) || defined(__EMX__)
  { ISKMAP, (Function *)emacs_dos_keymap }, /* prefix of special keys */
#else
  { ISFUNC, rl_set_mark },		/* Control-@ */
#endif
  { ISFUNC, rl_beg_of_line },		/* Control-a */
  { ISFUNC, rl_backward },		/* Control-b */
  { ISFUNC, (Function *)0x0 },		/* Control-c */
  { ISFUNC, rl_delete },		/* Control-d */
  { ISFUNC, rl_end_of_line },		/* Control-e */
  { ISFUNC, rl_forward },		/* Control-f */
  { ISFUNC, rl_abort },			/* Control-g */
  { ISFUNC, rl_rubout },		/* Control-h */
  { ISFUNC, rl_complete },		/* Control-i */
  { ISFUNC, rl_newline },		/* Control-j */
  { ISFUNC, rl_kill_line },		/* Control-k */
  { ISFUNC, rl_clear_screen },		/* Control-l */
  { ISFUNC, rl_newline },		/* Control-m */
  { ISFUNC, rl_get_next_history },	/* Control-n */
  { ISFUNC, (Function *)0x0 },		/* Control-o */
  { ISFUNC, rl_get_previous_history },	/* Control-p */
  { ISFUNC, rl_quoted_insert },		/* Control-q */
  { ISFUNC, rl_reverse_search_history }, /* Control-r */
  { ISFUNC, rl_forward_search_history }, /* Control-s */
  { ISFUNC, rl_transpose_chars },	/* Control-t */
  { ISFUNC, rl_unix_line_discard },	/* Control-u */
  { ISFUNC, rl_quoted_insert },		/* Control-v */
  { ISFUNC, rl_unix_word_rubout },	/* Control-w */
  { ISKMAP, (Function *)emacs_ctlx_keymap },	/* Control-x */
  { ISFUNC, rl_yank },			/* Control-y */
  { ISFUNC, (Function *)0x0 },		/* Control-z */
  { ISKMAP, (Function *)emacs_meta_keymap }, /* Control-[ */
  { ISFUNC, (Function *)0x0 },		/* Control-\ */
  { ISFUNC, rl_char_search },		/* Control-] */
  { ISFUNC, (Function *)0x0 },		/* Control-^ */
  { ISFUNC, rl_undo_command },		/* Control-_ */

  /* The start of printing characters. */
  { ISFUNC, rl_insert },	/* SPACE */
  { ISFUNC, rl_insert },	/* ! */
  { ISFUNC, rl_insert },	/* " */
  { ISFUNC, rl_insert },	/* # */
  { ISFUNC, rl_insert },	/* $ */
  { ISFUNC, rl_insert },	/* % */
  { ISFUNC, rl_insert },	/* & */
  { ISFUNC, rl_insert },	/* ' */
  { ISFUNC, rl_insert },	/* ( */
#if defined (PAREN_MATCHING)
  { ISFUNC, rl_insert_close },	/* ) */
#else
  { ISFUNC, rl_insert },	/* ) */
#endif /* !PAREN_MATCHING */
  { ISFUNC, rl_insert },	/* * */
  { ISFUNC, rl_insert },	/* + */
  { ISFUNC, rl_insert },	/* , */
  { ISFUNC, rl_insert },	/* - */
  { ISFUNC, rl_insert },	/* . */
  { ISFUNC, rl_insert },	/* / */

  /* Regular digits. */
  { ISFUNC, rl_insert },	/* 0 */
  { ISFUNC, rl_insert },	/* 1 */
  { ISFUNC, rl_insert },	/* 2 */
  { ISFUNC, rl_insert },	/* 3 */
  { ISFUNC, rl_insert },	/* 4 */
  { ISFUNC, rl_insert },	/* 5 */
  { ISFUNC, rl_insert },	/* 6 */
  { ISFUNC, rl_insert },	/* 7 */
  { ISFUNC, rl_insert },	/* 8 */
  { ISFUNC, rl_insert },	/* 9 */

  /* A little more punctuation. */
  { ISFUNC, rl_insert },	/* : */
  { ISFUNC, rl_insert },	/* ; */
  { ISFUNC, rl_insert },	/* < */
  { ISFUNC, rl_insert },	/* = */
  { ISFUNC, rl_insert },	/* > */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* @ */

  /* Uppercase alphabet. */
  { ISFUNC, rl_insert },	/* A */
  { ISFUNC, rl_insert },	/* B */
  { ISFUNC, rl_insert },	/* C */
  { ISFUNC, rl_insert },	/* D */
  { ISFUNC, rl_insert },	/* E */
  { ISFUNC, rl_insert },	/* F */
  { ISFUNC, rl_insert },	/* G */
  { ISFUNC, rl_insert },	/* H */
  { ISFUNC, rl_insert },	/* I */
  { ISFUNC, rl_insert },	/* J */
  { ISFUNC, rl_insert },	/* K */
  { ISFUNC, rl_insert },	/* L */
  { ISFUNC, rl_insert },	/* M */
  { ISFUNC, rl_insert },	/* N */
  { ISFUNC, rl_insert },	/* O */
  { ISFUNC, rl_insert },	/* P */
  { ISFUNC, rl_insert },	/* Q */
  { ISFUNC, rl_insert },	/* R */
  { ISFUNC, rl_insert },	/* S */
  { ISFUNC, rl_insert },	/* T */
  { ISFUNC, rl_insert },	/* U */
  { ISFUNC, rl_insert },	/* V */
  { ISFUNC, rl_insert },	/* W */
  { ISFUNC, rl_insert },	/* X */
  { ISFUNC, rl_insert },	/* Y */
  { ISFUNC, rl_insert },	/* Z */

  /* Some more punctuation. */
  { ISFUNC, rl_insert },	/* [ */
  { ISFUNC, rl_insert },	/* \ */
#if defined (PAREN_MATCHING)
  { ISFUNC, rl_insert_close },	/* ] */
#else
  { ISFUNC, rl_insert },	/* ] */
#endif /* !PAREN_MATCHING */
  { ISFUNC, rl_insert },	/* ^ */
  { ISFUNC, rl_insert },	/* _ */
  { ISFUNC, rl_insert },	/* ` */

  /* Lowercase alphabet. */
  { ISFUNC, rl_insert },	/* a */
  { ISFUNC, rl_insert },	/* b */
  { ISFUNC, rl_insert },	/* c */
  { ISFUNC, rl_insert },	/* d */
  { ISFUNC, rl_insert },	/* e */
  { ISFUNC, rl_insert },	/* f */
  { ISFUNC, rl_insert },	/* g */
  { ISFUNC, rl_insert },	/* h */
  { ISFUNC, rl_insert },	/* i */
  { ISFUNC, rl_insert },	/* j */
  { ISFUNC, rl_insert },	/* k */
  { ISFUNC, rl_insert },	/* l */
  { ISFUNC, rl_insert },	/* m */
  { ISFUNC, rl_insert },	/* n */
  { ISFUNC, rl_insert },	/* o */
  { ISFUNC, rl_insert },	/* p */
  { ISFUNC, rl_insert },	/* q */
  { ISFUNC, rl_insert },	/* r */
  { ISFUNC, rl_insert },	/* s */
  { ISFUNC, rl_insert },	/* t */
  { ISFUNC, rl_insert },	/* u */
  { ISFUNC, rl_insert },	/* v */
  { ISFUNC, rl_insert },	/* w */
  { ISFUNC, rl_insert },	/* x */
  { ISFUNC, rl_insert },	/* y */
  { ISFUNC, rl_insert },	/* z */

  /* Final punctuation. */
  { ISFUNC, rl_insert },	/* { */
  { ISFUNC, rl_insert },	/* | */
#if defined (PAREN_MATCHING)
  { ISFUNC, rl_insert_close },	/* } */
#else
  { ISFUNC, rl_insert },	/* } */
#endif /* !PAREN_MATCHING */
  { ISFUNC, rl_insert },	/* ~ */
  { ISFUNC, rl_rubout },	/* RUBOUT */

#if KEYMAP_SIZE > 128
  /* Pure 8-bit characters (128 - 159).
     These might be used in some
     character sets. */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */
  { ISFUNC, rl_insert },	/* ? */

  /* ISO Latin-1 characters (160 - 255) */
  { ISFUNC, rl_insert },	/* No-break space */
  { ISFUNC, rl_insert },	/* Inverted exclamation mark */
  { ISFUNC, rl_insert },	/* Cent sign */
  { ISFUNC, rl_insert },	/* Pound sign */
  { ISFUNC, rl_insert },	/* Currency sign */
  { ISFUNC, rl_insert },	/* Yen sign */
  { ISFUNC, rl_insert },	/* Broken bar */
  { ISFUNC, rl_insert },	/* Section sign */
  { ISFUNC, rl_insert },	/* Diaeresis */
  { ISFUNC, rl_insert },	/* Copyright sign */
  { ISFUNC, rl_insert },	/* Feminine ordinal indicator */
  { ISFUNC, rl_insert },	/* Left pointing double angle quotation mark */
  { ISFUNC, rl_insert },	/* Not sign */
  { ISFUNC, rl_insert },	/* Soft hyphen */
  { ISFUNC, rl_insert },	/* Registered sign */
  { ISFUNC, rl_insert },	/* Macron */
  { ISFUNC, rl_insert },	/* Degree sign */
  { ISFUNC, rl_insert },	/* Plus-minus sign */
  { ISFUNC, rl_insert },	/* Superscript two */
  { ISFUNC, rl_insert },	/* Superscript three */
  { ISFUNC, rl_insert },	/* Acute accent */
  { ISFUNC, rl_insert },	/* Micro sign */
  { ISFUNC, rl_insert },	/* Pilcrow sign */
  { ISFUNC, rl_insert },	/* Middle dot */
  { ISFUNC, rl_insert },	/* Cedilla */
  { ISFUNC, rl_insert },	/* Superscript one */
  { ISFUNC, rl_insert },	/* Masculine ordinal indicator */
  { ISFUNC, rl_insert },	/* Right pointing double angle quotation mark */
  { ISFUNC, rl_insert },	/* Vulgar fraction one quarter */
  { ISFUNC, rl_insert },	/* Vulgar fraction one half */
  { ISFUNC, rl_insert },	/* Vulgar fraction three quarters */
  { ISFUNC, rl_insert },	/* Inverted questionk mark */
  { ISFUNC, rl_insert },	/* Latin capital letter a with grave */
  { ISFUNC, rl_insert },	/* Latin capital letter a with acute */
  { ISFUNC, rl_insert },	/* Latin capital letter a with circumflex */
  { ISFUNC, rl_insert },	/* Latin capital letter a with tilde */
  { ISFUNC, rl_insert },	/* Latin capital letter a with diaeresis */
  { ISFUNC, rl_insert },	/* Latin capital letter a with ring above */
  { ISFUNC, rl_insert },	/* Latin capital letter ae */
  { ISFUNC, rl_insert },	/* Latin capital letter c with cedilla */
  { ISFUNC, rl_insert },	/* Latin capital letter e with grave */
  { ISFUNC, rl_insert },	/* Latin capital letter e with acute */
  { ISFUNC, rl_insert },	/* Latin capital letter e with circumflex */
  { ISFUNC, rl_insert },	/* Latin capital letter e with diaeresis */
  { ISFUNC, rl_insert },	/* Latin capital letter i with grave */
  { ISFUNC, rl_insert },	/* Latin capital letter i with acute */
  { ISFUNC, rl_insert },	/* Latin capital letter i with circumflex */
  { ISFUNC, rl_insert },	/* Latin capital letter i with diaeresis */
  { ISFUNC, rl_insert },	/* Latin capital letter eth (Icelandic) */
  { ISFUNC, rl_insert },	/* Latin capital letter n with tilde */
  { ISFUNC, rl_insert },	/* Latin capital letter o with grave */
  { ISFUNC, rl_insert },	/* Latin capital letter o with acute */
  { ISFUNC, rl_insert },	/* Latin capital letter o with circumflex */
  { ISFUNC, rl_insert },	/* Latin capital letter o with tilde */
  { ISFUNC, rl_insert },	/* Latin capital letter o with diaeresis */
  { ISFUNC, rl_insert },	/* Multiplication sign */
  { ISFUNC, rl_insert },	/* Latin capital letter o with stroke */
  { ISFUNC, rl_insert },	/* Latin capital letter u with grave */
  { ISFUNC, rl_insert },	/* Latin capital letter u with acute */
  { ISFUNC, rl_insert },	/* Latin capital letter u with circumflex */
  { ISFUNC, rl_insert },	/* Latin capital letter u with diaeresis */
  { ISFUNC, rl_insert },	/* Latin capital letter Y with acute */
  { ISFUNC, rl_insert },	/* Latin capital letter thorn (Icelandic) */
  { ISFUNC, rl_insert },	/* Latin small letter sharp s (German) */
  { ISFUNC, rl_insert },	/* Latin small letter a with grave */
  { ISFUNC, rl_insert },	/* Latin small letter a with acute */
  { ISFUNC, rl_insert },	/* Latin small letter a with circumflex */
  { ISFUNC, rl_insert },	/* Latin small letter a with tilde */
  { ISFUNC, rl_insert },	/* Latin small letter a with diaeresis */
  { ISFUNC, rl_insert },	/* Latin small letter a with ring above */
  { ISFUNC, rl_insert },	/* Latin small letter ae */
  { ISFUNC, rl_insert },	/* Latin small letter c with cedilla */
  { ISFUNC, rl_insert },	/* Latin small letter e with grave */
  { ISFUNC, rl_insert },	/* Latin small letter e with acute */
  { ISFUNC, rl_insert },	/* Latin small letter e with circumflex */
  { ISFUNC, rl_insert },	/* Latin small letter e with diaeresis */
  { ISFUNC, rl_insert },	/* Latin small letter i with grave */
  { ISFUNC, rl_insert },	/* Latin small letter i with acute */
  { ISFUNC, rl_insert },	/* Latin small letter i with circumflex */
  { ISFUNC, rl_insert },	/* Latin small letter i with diaeresis */
  { ISFUNC, rl_insert },	/* Latin small letter eth (Icelandic) */
  { ISFUNC, rl_insert },	/* Latin small letter n with tilde */
  { ISFUNC, rl_insert },	/* Latin small letter o with grave */
  { ISFUNC, rl_insert },	/* Latin small letter o with acute */
  { ISFUNC, rl_insert },	/* Latin small letter o with circumflex */
  { ISFUNC, rl_insert },	/* Latin small letter o with tilde */
  { ISFUNC, rl_insert },	/* Latin small letter o with diaeresis */
  { ISFUNC, rl_insert },	/* Division sign */
  { ISFUNC, rl_insert },	/* Latin small letter o with stroke */
  { ISFUNC, rl_insert },	/* Latin small letter u with grave */
  { ISFUNC, rl_insert },	/* Latin small letter u with acute */
  { ISFUNC, rl_insert },	/* Latin small letter u with circumflex */
  { ISFUNC, rl_insert },	/* Latin small letter u with diaeresis */
  { ISFUNC, rl_insert },	/* Latin small letter y with acute */
  { ISFUNC, rl_insert },	/* Latin small letter thorn (Icelandic) */
  { ISFUNC, rl_insert }		/* Latin small letter y with diaeresis */
#endif /* KEYMAP_SIZE > 128 */
};

KEYMAP_ENTRY_ARRAY emacs_meta_keymap = {

  /* Meta keys.  Just like above, but the high bit is set. */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-@ */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-a */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-b */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-c */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-d */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-e */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-f */
  { ISFUNC, rl_abort },		/* Meta-Control-g */
  { ISFUNC, rl_backward_kill_word },	/* Meta-Control-h */
  { ISFUNC, rl_tab_insert },	/* Meta-Control-i */
  { ISFUNC, rl_vi_editing_mode }, /* Meta-Control-j */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-k */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-l */
  { ISFUNC, rl_vi_editing_mode }, /* Meta-Control-m */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-n */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-o */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-p */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-q */
  { ISFUNC, rl_revert_line },	/* Meta-Control-r */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-s */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-t */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-u */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-v */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-w */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-x */
  { ISFUNC, rl_yank_nth_arg },	/* Meta-Control-y */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-z */

  { ISFUNC, rl_complete },	/* Meta-Control-[ */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-\ */
  { ISFUNC, rl_backward_char_search },	/* Meta-Control-] */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-^ */
  { ISFUNC, (Function *)0x0 },	/* Meta-Control-_ */

  /* The start of printing characters. */
  { ISFUNC, rl_set_mark },	/* Meta-SPACE */
  { ISFUNC, (Function *)0x0 },	/* Meta-! */
  { ISFUNC, (Function *)0x0 },	/* Meta-" */
  { ISFUNC, rl_insert_comment },/* Meta-# */
  { ISFUNC, (Function *)0x0 },	/* Meta-$ */
  { ISFUNC, (Function *)0x0 },	/* Meta-% */
  { ISFUNC, rl_tilde_expand },	/* Meta-& */
  { ISFUNC, (Function *)0x0 },	/* Meta-' */
  { ISFUNC, (Function *)0x0 },	/* Meta-( */
  { ISFUNC, (Function *)0x0 },	/* Meta-) */
  { ISFUNC, rl_insert_completions },	/* Meta-* */
  { ISFUNC, (Function *)0x0 },	/* Meta-+ */
  { ISFUNC, (Function *)0x0 },	/* Meta-, */
  { ISFUNC, rl_digit_argument }, /* Meta-- */
  { ISFUNC, rl_yank_last_arg},	/* Meta-. */
  { ISFUNC, (Function *)0x0 },	/* Meta-/ */

  /* Regular digits. */
  { ISFUNC, rl_digit_argument }, /* Meta-0 */
  { ISFUNC, rl_digit_argument }, /* Meta-1 */
  { ISFUNC, rl_digit_argument }, /* Meta-2 */
  { ISFUNC, rl_digit_argument }, /* Meta-3 */
  { ISFUNC, rl_digit_argument }, /* Meta-4 */
  { ISFUNC, rl_digit_argument }, /* Meta-5 */
  { ISFUNC, rl_digit_argument }, /* Meta-6 */
  { ISFUNC, rl_digit_argument }, /* Meta-7 */
  { ISFUNC, rl_digit_argument }, /* Meta-8 */
  { ISFUNC, rl_digit_argument }, /* Meta-9 */

  /* A little more punctuation. */
  { ISFUNC, (Function *)0x0 },		/* Meta-: */
  { ISFUNC, (Function *)0x0 },		/* Meta-; */
  { ISFUNC, rl_beginning_of_history },	/* Meta-< */
  { ISFUNC, rl_possible_completions },	/* Meta-= */
  { ISFUNC, rl_end_of_history },	/* Meta-> */
  { ISFUNC, rl_possible_completions },	/* Meta-? */
  { ISFUNC, (Function *)0x0 },		/* Meta-@ */

  /* Uppercase alphabet. */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-A */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-B */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-C */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-D */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-E */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-F */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-G */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-H */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-I */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-J */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-K */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-L */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-M */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-N */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-O */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-P */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-Q */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-R */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-S */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-T */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-U */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-V */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-W */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-X */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-Y */
  { ISFUNC, rl_do_lowercase_version },	/* Meta-Z */

  /* Some more punctuation. */
  { ISFUNC, (Function *)0x0 },		/* Meta-[ */	/* was rl_arrow_keys */
  { ISFUNC, rl_delete_horizontal_space },	/* Meta-\ */
  { ISFUNC, (Function *)0x0 },		/* Meta-] */
  { ISFUNC, (Function *)0x0 },		/* Meta-^ */
  { ISFUNC, rl_yank_last_arg },		/* Meta-_ */
  { ISFUNC, (Function *)0x0 },		/* Meta-` */

  /* Lowercase alphabet. */
  { ISFUNC, (Function *)0x0 },	/* Meta-a */
  { ISFUNC, rl_backward_word },	/* Meta-b */
  { ISFUNC, rl_capitalize_word }, /* Meta-c */
  { ISFUNC, rl_kill_word },	/* Meta-d */
  { ISFUNC, (Function *)0x0 },	/* Meta-e */
  { ISFUNC, rl_forward_word },	/* Meta-f */
  { ISFUNC, (Function *)0x0 },	/* Meta-g */
  { ISFUNC, (Function *)0x0 },	/* Meta-h */
  { ISFUNC, (Function *)0x0 },	/* Meta-i */
  { ISFUNC, (Function *)0x0 },	/* Meta-j */
  { ISFUNC, (Function *)0x0 },	/* Meta-k */
  { ISFUNC, rl_downcase_word },	/* Meta-l */
  { ISFUNC, (Function *)0x0 },	/* Meta-m */
  { ISFUNC, rl_noninc_forward_search },	/* Meta-n */
  { ISFUNC, (Function *)0x0 },	/* Meta-o */	/* was rl_arrow_keys */
  { ISFUNC, rl_noninc_reverse_search },	/* Meta-p */
  { ISFUNC, (Function *)0x0 },	/* Meta-q */
  { ISFUNC, rl_revert_line },	/* Meta-r */
  { ISFUNC, (Function *)0x0 },	/* Meta-s */
  { ISFUNC, rl_transpose_words }, /* Meta-t */
  { ISFUNC, rl_upcase_word },	/* Meta-u */
  { ISFUNC, (Function *)0x0 },	/* Meta-v */
  { ISFUNC, (Function *)0x0 },	/* Meta-w */
  { ISFUNC, (Function *)0x0 },	/* Meta-x */
  { ISFUNC, rl_yank_pop },	/* Meta-y */
  { ISFUNC, (Function *)0x0 },	/* Meta-z */

  /* Final punctuation. */
  { ISFUNC, (Function *)0x0 },	/* Meta-{ */
  { ISFUNC, (Function *)0x0 },	/* Meta-| */
  { ISFUNC, (Function *)0x0 },	/* Meta-} */
  { ISFUNC, rl_tilde_expand },	/* Meta-~ */
  { ISFUNC, rl_backward_kill_word }, /* Meta-rubout */

#if KEYMAP_SIZE > 128
  /* Undefined keys. */
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 }
#endif /* KEYMAP_SIZE > 128 */
};

KEYMAP_ENTRY_ARRAY emacs_ctlx_keymap = {

  /* Control keys. */
  { ISFUNC, (Function *)0x0 },		/* Control-@ */
  { ISFUNC, (Function *)0x0 },		/* Control-a */
  { ISFUNC, (Function *)0x0 },		/* Control-b */
  { ISFUNC, (Function *)0x0 },		/* Control-c */
  { ISFUNC, (Function *)0x0 },		/* Control-d */
  { ISFUNC, (Function *)0x0 },		/* Control-e */
  { ISFUNC, (Function *)0x0 },		/* Control-f */
  { ISFUNC, rl_abort },			/* Control-g */
  { ISFUNC, (Function *)0x0 },		/* Control-h */
  { ISFUNC, (Function *)0x0 },		/* Control-i */
  { ISFUNC, (Function *)0x0 },		/* Control-j */
  { ISFUNC, (Function *)0x0 },		/* Control-k */
  { ISFUNC, (Function *)0x0 },		/* Control-l */
  { ISFUNC, (Function *)0x0 },		/* Control-m */
  { ISFUNC, (Function *)0x0 },		/* Control-n */
  { ISFUNC, (Function *)0x0 },		/* Control-o */
  { ISFUNC, (Function *)0x0 },		/* Control-p */
  { ISFUNC, (Function *)0x0 },		/* Control-q */
  { ISFUNC, rl_re_read_init_file },	/* Control-r */
  { ISFUNC, (Function *)0x0 },		/* Control-s */
  { ISFUNC, (Function *)0x0 },		/* Control-t */
  { ISFUNC, rl_undo_command },		/* Control-u */
  { ISFUNC, (Function *)0x0 },		/* Control-v */
  { ISFUNC, (Function *)0x0 },		/* Control-w */
  { ISFUNC, rl_exchange_point_and_mark },/* Control-x */
  { ISFUNC, (Function *)0x0 },		/* Control-y */
  { ISFUNC, (Function *)0x0 },		/* Control-z */
  { ISFUNC, (Function *)0x0 },		/* Control-[ */
  { ISFUNC, (Function *)0x0 },		/* Control-\ */
  { ISFUNC, (Function *)0x0 },		/* Control-] */
  { ISFUNC, (Function *)0x0 },		/* Control-^ */
  { ISFUNC, (Function *)0x0 },		/* Control-_ */

  /* The start of printing characters. */
  { ISFUNC, (Function *)0x0 },		/* SPACE */
  { ISFUNC, (Function *)0x0 },		/* ! */
  { ISFUNC, (Function *)0x0 },		/* " */
  { ISFUNC, (Function *)0x0 },		/* # */
  { ISFUNC, (Function *)0x0 },		/* $ */
  { ISFUNC, (Function *)0x0 },		/* % */
  { ISFUNC, (Function *)0x0 },		/* & */
  { ISFUNC, (Function *)0x0 },		/* ' */
  { ISFUNC, rl_start_kbd_macro },	/* ( */
  { ISFUNC, rl_end_kbd_macro  },	/* ) */
  { ISFUNC, (Function *)0x0 },		/* * */
  { ISFUNC, (Function *)0x0 },		/* + */
  { ISFUNC, (Function *)0x0 },		/* , */
  { ISFUNC, (Function *)0x0 },		/* - */
  { ISFUNC, (Function *)0x0 },		/* . */
  { ISFUNC, (Function *)0x0 },		/* / */

  /* Regular digits. */
  { ISFUNC, (Function *)0x0 },		/* 0 */
  { ISFUNC, (Function *)0x0 },		/* 1 */
  { ISFUNC, (Function *)0x0 },		/* 2 */
  { ISFUNC, (Function *)0x0 },		/* 3 */
  { ISFUNC, (Function *)0x0 },		/* 4 */
  { ISFUNC, (Function *)0x0 },		/* 5 */
  { ISFUNC, (Function *)0x0 },		/* 6 */
  { ISFUNC, (Function *)0x0 },		/* 7 */
  { ISFUNC, (Function *)0x0 },		/* 8 */
  { ISFUNC, (Function *)0x0 },		/* 9 */

  /* A little more punctuation. */
  { ISFUNC, (Function *)0x0 },	/* : */
  { ISFUNC, (Function *)0x0 },	/* ; */
  { ISFUNC, (Function *)0x0 },	/* < */
  { ISFUNC, (Function *)0x0 },	/* = */
  { ISFUNC, (Function *)0x0 },	/* > */
  { ISFUNC, (Function *)0x0 },	/* ? */
  { ISFUNC, (Function *)0x0 },	/* @ */

  /* Uppercase alphabet. */
  { ISFUNC, rl_do_lowercase_version },	/* A */
  { ISFUNC, rl_do_lowercase_version },	/* B */
  { ISFUNC, rl_do_lowercase_version },	/* C */
  { ISFUNC, rl_do_lowercase_version },	/* D */
  { ISFUNC, rl_do_lowercase_version },	/* E */
  { ISFUNC, rl_do_lowercase_version },	/* F */
  { ISFUNC, rl_do_lowercase_version },	/* G */
  { ISFUNC, rl_do_lowercase_version },	/* H */
  { ISFUNC, rl_do_lowercase_version },	/* I */
  { ISFUNC, rl_do_lowercase_version },	/* J */
  { ISFUNC, rl_do_lowercase_version },	/* K */
  { ISFUNC, rl_do_lowercase_version },	/* L */
  { ISFUNC, rl_do_lowercase_version },	/* M */
  { ISFUNC, rl_do_lowercase_version },	/* N */
  { ISFUNC, rl_do_lowercase_version },	/* O */
  { ISFUNC, rl_do_lowercase_version },	/* P */
  { ISFUNC, rl_do_lowercase_version },	/* Q */
  { ISFUNC, rl_do_lowercase_version },	/* R */
  { ISFUNC, rl_do_lowercase_version },	/* S */
  { ISFUNC, rl_do_lowercase_version },	/* T */
  { ISFUNC, rl_do_lowercase_version },	/* U */
  { ISFUNC, rl_do_lowercase_version },	/* V */
  { ISFUNC, rl_do_lowercase_version },	/* W */
  { ISFUNC, rl_do_lowercase_version },	/* X */
  { ISFUNC, rl_do_lowercase_version },	/* Y */
  { ISFUNC, rl_do_lowercase_version },	/* Z */

  /* Some more punctuation. */
  { ISFUNC, (Function *)0x0 },		/* [ */
  { ISFUNC, (Function *)0x0 },		/* \ */
  { ISFUNC, (Function *)0x0 },		/* ] */
  { ISFUNC, (Function *)0x0 },		/* ^ */
  { ISFUNC, (Function *)0x0 },		/* _ */
  { ISFUNC, (Function *)0x0 },		/* ` */

  /* Lowercase alphabet. */
  { ISFUNC, (Function *)0x0 },		/* a */
  { ISFUNC, (Function *)0x0 },		/* b */
  { ISFUNC, (Function *)0x0 },		/* c */
  { ISFUNC, (Function *)0x0 },		/* d */
  { ISFUNC, rl_call_last_kbd_macro },	/* e */
  { ISFUNC, (Function *)0x0 },		/* f */
  { ISFUNC, (Function *)0x0 },		/* g */
  { ISFUNC, (Function *)0x0 },		/* h */
  { ISFUNC, (Function *)0x0 },		/* i */
  { ISFUNC, (Function *)0x0 },		/* j */
  { ISFUNC, (Function *)0x0 },		/* k */
  { ISFUNC, (Function *)0x0 },		/* l */
  { ISFUNC, (Function *)0x0 },		/* m */
  { ISFUNC, (Function *)0x0 },		/* n */
  { ISFUNC, (Function *)0x0 },		/* o */
  { ISFUNC, (Function *)0x0 },		/* p */
  { ISFUNC, (Function *)0x0 },		/* q */
  { ISFUNC, (Function *)0x0 },		/* r */
  { ISFUNC, (Function *)0x0 },		/* s */
  { ISFUNC, (Function *)0x0 },		/* t */
  { ISFUNC, (Function *)0x0 },		/* u */
  { ISFUNC, (Function *)0x0 },		/* v */
  { ISFUNC, (Function *)0x0 },		/* w */
  { ISFUNC, (Function *)0x0 },		/* x */
  { ISFUNC, (Function *)0x0 },		/* y */
  { ISFUNC, (Function *)0x0 },		/* z */

  /* Final punctuation. */
  { ISFUNC, (Function *)0x0 },		/* { */
  { ISFUNC, (Function *)0x0 },		/* | */
  { ISFUNC, (Function *)0x0 },		/* } */
  { ISFUNC, (Function *)0x0 },		/* ~ */
  { ISFUNC, rl_backward_kill_line },	/* RUBOUT */

#if KEYMAP_SIZE > 128
  /* Undefined keys. */
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 },
  { ISFUNC, (Function *)0x0 }
#endif /* KEYMAP_SIZE > 128 */
};

#if defined(__MSDOS__) || defined(__EMX__)

KEYMAP_ENTRY_ARRAY emacs_dos_keymap = {
  { ISFUNC, (Function *)0x0 },      	/*   0 */
  { ISFUNC, (Function *)0x0 },      	/*   1: <Alt>+<Esc>    [DOS] */
  { ISFUNC, (Function *)0x0 },      	/*   2: <Ctrl>+<Space> [OS2] */
  { ISFUNC, (Function *)0x0 },      	/*   3: <Ctrl>+<@> */
  { ISFUNC, (Function *)0x0 },      	/*   4: <Shift>+<Ins>  [OS2] */
  { ISFUNC, (Function *)0x0 },      	/*   5: <Shift>+<Del>  [OS2] */
  { ISFUNC, (Function *)0x0 },      	/*   6 */
  { ISFUNC, (Function *)0x0 },      	/*   7 */
  { ISFUNC, (Function *)0x0 },      	/*   8 */
  { ISFUNC, (Function *)0x0 },      	/*   9 */
  { ISFUNC, (Function *)0x0 },      	/*  10 */
  { ISFUNC, (Function *)0x0 },      	/*  11 */
  { ISFUNC, (Function *)0x0 },      	/*  12 */
  { ISFUNC, (Function *)0x0 },      	/*  13 */
  { ISFUNC, (Function *)0x0 },      	/*  14: <Alt>+<Backspace> */
  { ISFUNC, (Function *)0x0 },      	/*  15: <Shift>+<Tab> */
  { ISFUNC, (Function *)0x0 },      	/*  16: <Alt>+<Q> */
  { ISFUNC, (Function *)0x0 },      	/*  17: <Alt>+<W> */
  { ISFUNC, (Function *)0x0 },      	/*  18: <Alt>+<E> */
  { ISFUNC, (Function *)0x0 },      	/*  19: <Alt>+<R> */
  { ISFUNC, (Function *)0x0 },      	/*  20: <Alt>+<T> */
  { ISFUNC, (Function *)0x0 },      	/*  21: <Alt>+<Y> */
  { ISFUNC, (Function *)0x0 },      	/*  22: <Alt>+<U> */
  { ISFUNC, (Function *)0x0 },      	/*  23: <Alt>+<I> */
  { ISFUNC, (Function *)0x0 },      	/*  24: <Alt>+<O> */
  { ISFUNC, (Function *)0x0 },      	/*  25: <Alt>+<P> */
  { ISFUNC, (Function *)0x0 },      	/*  26: <Alt>+<[> */
  { ISFUNC, (Function *)0x0 },      	/*  27: <Alt>+<]> */
  { ISFUNC, (Function *)0x0 },      	/*  28: <Alt>+<Return> */
  { ISFUNC, (Function *)0x0 },      	/*  29 */
  { ISFUNC, (Function *)0x0 },      	/*  30: <Alt>+<A> */
  { ISFUNC, (Function *)0x0 },      	/*  31: <Alt>+<S> */
  { ISFUNC, (Function *)0x0 },      	/*  32: <Alt>+<D> */
  { ISFUNC, (Function *)0x0 },      	/*  33: <Alt>+<F> */
  { ISFUNC, (Function *)0x0 },      	/*  34: <Alt>+<G> */
  { ISFUNC, (Function *)0x0 },      	/*  35: <Alt>+<H> */
  { ISFUNC, (Function *)0x0 },      	/*  36: <Alt>+<J> */
  { ISFUNC, (Function *)0x0 },      	/*  37: <Alt>+<K> */
  { ISFUNC, (Function *)0x0 },      	/*  38: <Alt>+<L> */
  { ISFUNC, (Function *)0x0 },      	/*  39: <Alt>+<;> */
  { ISFUNC, (Function *)0x0 },      	/*  40: <Alt>+<'> */
  { ISFUNC, (Function *)0x0 },      	/*  41: <Alt>+<`> */
  { ISFUNC, (Function *)0x0 },      	/*  42 */
  { ISFUNC, (Function *)0x0 },      	/*  43: <Alt>+<\> */
  { ISFUNC, (Function *)0x0 },      	/*  44: <Alt>+<Z> */
  { ISFUNC, (Function *)0x0 },      	/*  45: <Alt>+<X> */
  { ISFUNC, (Function *)0x0 },      	/*  46: <Alt>+<C> */
  { ISFUNC, (Function *)0x0 },      	/*  47: <Alt>+<V> */
  { ISFUNC, (Function *)0x0 },      	/*  48: <Alt>+<B> */
  { ISFUNC, (Function *)0x0 },      	/*  49: <Alt>+<N> */
  { ISFUNC, (Function *)0x0 },      	/*  50: <Alt>+<M> */
  { ISFUNC, (Function *)0x0 },      	/*  51: <Alt>+<,> */
  { ISFUNC, (Function *)0x0 },      	/*  52: <Alt>+<.> */
  { ISFUNC, (Function *)0x0 },      	/*  53: <Alt>+</> */
  { ISFUNC, (Function *)0x0 },      	/*  54 */
  { ISFUNC, (Function *)0x0 },      	/*  55: <Alt>+<*> (numeric keypad) */
  { ISFUNC, (Function *)0x0 },      	/*  56 */
  { ISFUNC, (Function *)0x0 },      	/*  57: <Alt>+<Space>  [OS2] */
  { ISFUNC, (Function *)0x0 },      	/*  58 */
  { ISFUNC, (Function *)0x0 },      	/*  59: <F1> */
  { ISFUNC, (Function *)0x0 },      	/*  60: <F2> */
  { ISFUNC, (Function *)0x0 },      	/*  61: <F3> */
  { ISFUNC, (Function *)0x0 },      	/*  62: <F4> */
  { ISFUNC, (Function *)0x0 },      	/*  63: <F5> */
  { ISFUNC, (Function *)0x0 },      	/*  64: <F6> */
  { ISFUNC, (Function *)0x0 },      	/*  65: <F7> */
  { ISFUNC, (Function *)0x0 },      	/*  66: <F8> */
  { ISFUNC, (Function *)0x0 },      	/*  67: <F9> */
  { ISFUNC, (Function *)0x0 },      	/*  68: <F10> */
  { ISFUNC, (Function *)0x0 },      	/*  69 */
  { ISFUNC, (Function *)0x0 },      	/*  70 */
  { ISFUNC, (Function *) rl_beg_of_line }, /*  71: <Home> */
  { ISFUNC, (Function *) rl_get_previous_history }, /*  72: <Up arrow> */
  { ISFUNC, (Function *) rl_beginning_of_history }, /*  73: <Page up> */
  { ISFUNC, (Function *)0x0 },      	/*  74: <Alt>+<-> (numeric keypad) */
  { ISFUNC, (Function *) rl_backward }, /*  75: <Left arrow> */
  { ISFUNC, (Function *)0x0 },      	/*  76: center cursor */
  { ISFUNC, (Function *) rl_forward },  /*  77: <Right arrow> */
  { ISFUNC, (Function *)0x0 },      	/*  78: <Alt>+<+> (numeric keypad) */
  { ISFUNC, (Function *) rl_end_of_line }, /*  79: <End> */
  { ISFUNC, (Function *) rl_get_next_history }, /*  80: <Down arrow> */
  { ISFUNC, (Function *) rl_end_of_history }, /*  81: <Page down> */
  { ISFUNC, (Function *)0x0 },      	/*  82: <Ins> */
  { ISFUNC, (Function *) rl_delete },   /*  83: <Del> */
  { ISFUNC, (Function *)0x0 },      	/*  84: <Shift>+<F1> */
  { ISFUNC, (Function *)0x0 },      	/*  85: <Shift>+<F2> */
  { ISFUNC, (Function *)0x0 },      	/*  86: <Shift>+<F3> */
  { ISFUNC, (Function *)0x0 },      	/*  87: <Shift>+<F4> */
  { ISFUNC, (Function *)0x0 },      	/*  88: <Shift>+<F5> */
  { ISFUNC, (Function *)0x0 },      	/*  89: <Shift>+<F6> */
  { ISFUNC, (Function *)0x0 },      	/*  90: <Shift>+<F7> */
  { ISFUNC, (Function *)0x0 },      	/*  91: <Shift>+<F8> */
  { ISFUNC, (Function *)0x0 },      	/*  92: <Shift>+<F9> */
  { ISFUNC, (Function *)0x0 },      	/*  93: <Shift>+<F10> */
  { ISFUNC, (Function *)0x0 },      	/*  94: <Ctrl>+<F1> */
  { ISFUNC, (Function *)0x0 },      	/*  95: <Ctrl>+<F2> */
  { ISFUNC, (Function *)0x0 },      	/*  96: <Ctrl>+<F3> */
  { ISFUNC, (Function *)0x0 },      	/*  97: <Ctrl>+<F4> */
  { ISFUNC, (Function *)0x0 },      	/*  98: <Ctrl>+<F5> */
  { ISFUNC, (Function *)0x0 },      	/*  99: <Ctrl>+<F6> */
  { ISFUNC, (Function *)0x0 },      	/* 100: <Ctrl>+<F7> */
  { ISFUNC, (Function *)0x0 },      	/* 101: <Ctrl>+<F8> */
  { ISFUNC, (Function *)0x0 },      	/* 102: <Ctrl>+<F9> */
  { ISFUNC, (Function *)0x0 },      	/* 103: <Ctrl>+<F10> */
  { ISFUNC, (Function *)0x0 },      	/* 104: <Alt>+<F1> */
  { ISFUNC, (Function *)0x0 },      	/* 105: <Alt>+<F2> */
  { ISFUNC, (Function *)0x0 },      	/* 106: <Alt>+<F3> */
  { ISFUNC, (Function *)0x0 },      	/* 107: <Alt>+<F4> */
  { ISFUNC, (Function *)0x0 },      	/* 108: <Alt>+<F5> */
  { ISFUNC, (Function *)0x0 },      	/* 109: <Alt>+<F6> */
  { ISFUNC, (Function *)0x0 },      	/* 110: <Alt>+<F7> */
  { ISFUNC, (Function *)0x0 },      	/* 111: <Alt>+<F8> */
  { ISFUNC, (Function *)0x0 },      	/* 112: <Alt>+<F9> */
  { ISFUNC, (Function *)0x0 },      	/* 113: <Alt>+<F10> */
  { ISFUNC, (Function *)0x0 },      	/* 114: <Ctrl>+<PrtSc> */
  { ISFUNC, (Function *) rl_backward_word }, /* 115: <Ctrl>+<Left arrow> */
  { ISFUNC, (Function *) rl_forward_word }, /* 116: <Ctrl>+<Right arrow> */
  { ISFUNC, (Function *) rl_kill_line },/* 117: <Ctrl>+<End> */
  { ISFUNC, (Function *)0x0 },      	/* 118: <Ctrl>+<Page down> */
  { ISFUNC, (Function *) rl_backward_kill_line },/* 119: <Ctrl>+<Home> */
  { ISFUNC, (Function *)0x0 },      	/* 120: <Alt>+<1> */
  { ISFUNC, (Function *)0x0 },      	/* 121: <Alt>+<2> */
  { ISFUNC, (Function *)0x0 },      	/* 122: <Alt>+<3> */
  { ISFUNC, (Function *)0x0 },      	/* 123: <Alt>+<4> */
  { ISFUNC, (Function *)0x0 },      	/* 124: <Alt>+<5> */
  { ISFUNC, (Function *)0x0 },      	/* 125: <Alt>+<6> */
  { ISFUNC, (Function *)0x0 },      	/* 126: <Alt>+<7> */
  { ISFUNC, (Function *)0x0 },      	/* 127: <Alt>+<8> */
  { ISFUNC, (Function *)0x0 },      	/* 128: <Alt>+<9> */
  { ISFUNC, (Function *)0x0 },      	/* 129: <Alt>+<0> */
  { ISFUNC, (Function *)0x0 },      	/* 130: <Alt>+<-> */
  { ISFUNC, (Function *)0x0 },      	/* 131: <Alt>+<=> */
  { ISFUNC, (Function *)0x0 },      	/* 132: <Ctrl>+<Page up> */
  { ISFUNC, (Function *)0x0 },      	/* 133: <F11> */
  { ISFUNC, (Function *)0x0 },      	/* 134: <F12> */
  { ISFUNC, (Function *)0x0 },      	/* 135: <Shift>+<F11> */
  { ISFUNC, (Function *)0x0 },      	/* 136: <Shift>+<F12> */
  { ISFUNC, (Function *)0x0 },      	/* 137: <Ctrl>+<F11> */
  { ISFUNC, (Function *)0x0 },      	/* 138: <Ctrl>+<F12> */
  { ISFUNC, (Function *)0x0 },      	/* 139: <Alt>+<F11> */
  { ISFUNC, (Function *)0x0 },      	/* 140: <Alt>+<F12> */
  { ISFUNC, (Function *)0x0 },      	/* 141: <Ctrl>+<Up arrow> */
  { ISFUNC, (Function *)0x0 },      	/* 142: <Ctrl>+<-> (numeric keypad) */
  { ISFUNC, (Function *)0x0 },      	/* 143: <Ctrl>+<Center> */
  { ISFUNC, (Function *)0x0 },      	/* 144: <Ctrl>+<+> (numeric keypad) */
  { ISFUNC, (Function *)0x0 },      	/* 145: <Ctrl>+<Down arrow> */
  { ISFUNC, (Function *)0x0 },      	/* 146: <Ctrl>+<Ins> */
  { ISFUNC, (Function *)0x0 },      	/* 147: <Ctrl>+<Del> */
  { ISFUNC, (Function *)0x0 },      	/* 148: <Ctrl>+<Tab> */
  { ISFUNC, (Function *)0x0 },      	/* 149: <Ctrl>+</> (numeric keypad) */
  { ISFUNC, (Function *)0x0 },      	/* 150: <Ctrl>+<*> (numeric keypad) */
  { ISFUNC, (Function *)0x0 },      	/* 151: <Alt>+<Home> */
  { ISFUNC, (Function *)0x0 },      	/* 152: <Alt>+<Up arrow> */
  { ISFUNC, (Function *)0x0 },      	/* 153: <Alt>+<Page up> */
  { ISFUNC, (Function *)0x0 },      	/* 154 */
  { ISFUNC, (Function *)0x0 },      	/* 155: <Alt>+<Left arrow> */
  { ISFUNC, (Function *)0x0 },      	/* 156 */
  { ISFUNC, (Function *)0x0 },      	/* 157: <Alt>+<Right arrow> */
  { ISFUNC, (Function *)0x0 },      	/* 158 */
  { ISFUNC, (Function *)0x0 },      	/* 159: <Alt>+<End> */
  { ISFUNC, (Function *)0x0 },      	/* 160: <Alt>+<Down arrow> */
  { ISFUNC, (Function *)0x0 },      	/* 161: <Alt>+<Page down> */
  { ISFUNC, (Function *)0x0 },      	/* 162: <Alt>+<Ins> */
  { ISFUNC, (Function *)0x0 },      	/* 163: <Alt>+<Del> */
  { ISFUNC, (Function *)0x0 },      	/* 164: <Alt>+</> (numeric keypad) */
  { ISFUNC, (Function *)0x0 },      	/* 165: <Alt>+<Tab>  [DOS] */
  { ISFUNC, (Function *)0x0 },      	/* 166: <Alt>+<Enter> (numeric keypad) */
  { ISFUNC, (Function *)0x0 },      	/* 167 */
};

#endif
