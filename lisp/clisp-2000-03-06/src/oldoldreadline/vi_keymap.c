/* vi_keymap.c -- changed by Bruno Haible, 17 August 1994 */

/* vi_keymap.c -- the keymap for vi_mode in readline (). */

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

extern KEYMAP_ENTRY_ARRAY vi_escape_keymap;

/* The keymap arrays for handling vi mode. */
KEYMAP_ENTRY_ARRAY vi_movement_keymap = {

  /* The regular control keys come first. */
  { ISFUNC, (VFunction *)0x0 },		/* Control-@ */
  { ISFUNC, (VFunction *)0x0 },		/* Control-a */
  { ISFUNC, (VFunction *)0x0 },		/* Control-b */
  { ISFUNC, (VFunction *)0x0 },		/* Control-c */
  { ISFUNC, rl_vi_eof_maybe },		/* Control-d */
  { ISFUNC, rl_emacs_editing_mode },	/* Control-e */
  { ISFUNC, (VFunction *)0x0 },		/* Control-f */
  { ISFUNC, rl_abort },			/* Control-g */
  { ISFUNC, rl_backward },		/* Control-h */
  { ISFUNC, (VFunction *)0x0 },		/* Control-i */
  { ISFUNC, rl_newline },		/* Control-j */
  { ISFUNC, rl_kill_line },		/* Control-k */
  { ISFUNC, rl_clear_screen },		/* Control-l */
  { ISFUNC, rl_newline },		/* Control-m */
  { ISFUNC, rl_get_next_history },	/* Control-n */
  { ISFUNC, (VFunction *)0x0 },		/* Control-o */
  { ISFUNC, rl_get_previous_history },	/* Control-p */
  { ISFUNC, rl_quoted_insert },		/* Control-q */
  { ISFUNC, rl_reverse_search_history }, /* Control-r */
  { ISFUNC, rl_forward_search_history }, /* Control-s */
  { ISFUNC, rl_transpose_chars },	/* Control-t */
  { ISFUNC, rl_unix_line_discard },	/* Control-u */
  { ISFUNC, rl_quoted_insert },		/* Control-v */
  { ISFUNC, rl_unix_word_rubout },	/* Control-w */
  { ISFUNC, (VFunction *)0x0 },		/* Control-x */
  { ISFUNC, rl_yank },			/* Control-y */
  { ISFUNC, (VFunction *)0x0 },		/* Control-z */

  { ISKMAP, (VFunction *)vi_escape_keymap }, /* Control-[ */
  { ISFUNC, (VFunction *)0x0 },		/* Control-\ */
  { ISFUNC, (VFunction *)0x0 },		/* Control-] */
  { ISFUNC, (VFunction *)0x0 },		/* Control-^ */
  { ISFUNC, rl_undo_command },		/* Control-_ */

  /* The start of printing characters. */
  { ISFUNC, rl_forward },		/* SPACE */
  { ISFUNC, (VFunction *)0x0 },		/* ! */
  { ISFUNC, (VFunction *)0x0 },		/* " */
  { ISFUNC, rl_vi_comment },		/* # */
  { ISFUNC, rl_end_of_line },		/* $ */
  { ISFUNC, rl_vi_match },		/* % */
  { ISFUNC, rl_vi_tilde_expand },	/* & */
  { ISFUNC, (VFunction *)0x0 },		/* ' */
  { ISFUNC, (VFunction *)0x0 },		/* ( */
  { ISFUNC, (VFunction *)0x0 },		/* ) */
  { ISFUNC, rl_vi_complete },		/* * */
  { ISFUNC, rl_get_next_history},	/* + */
  { ISFUNC, rl_vi_char_search },	/* , */
  { ISFUNC, rl_get_previous_history },	/* - */
  { ISFUNC, rl_vi_redo },		/* . */
  { ISFUNC, rl_vi_search },		/* / */

  /* Regular digits. */
  { ISFUNC, rl_vi_arg_digit },		/* 0 */
  { ISFUNC, rl_vi_arg_digit },		/* 1 */
  { ISFUNC, rl_vi_arg_digit },		/* 2 */
  { ISFUNC, rl_vi_arg_digit },		/* 3 */
  { ISFUNC, rl_vi_arg_digit },		/* 4 */
  { ISFUNC, rl_vi_arg_digit },		/* 5 */
  { ISFUNC, rl_vi_arg_digit },		/* 6 */
  { ISFUNC, rl_vi_arg_digit },		/* 7 */
  { ISFUNC, rl_vi_arg_digit },		/* 8 */
  { ISFUNC, rl_vi_arg_digit },		/* 9 */

  /* A little more punctuation. */
  { ISFUNC, (VFunction *)0x0 },		/* : */
  { ISFUNC, rl_vi_char_search },	/* ; */
  { ISFUNC, (VFunction *)0x0 },		/* < */
  { ISFUNC, rl_vi_complete },		/* = */
  { ISFUNC, (VFunction *)0x0 },		/* > */
  { ISFUNC, rl_vi_search },		/* ? */
  { ISFUNC, (VFunction *)0x0 },		/* @ */

  /* Uppercase alphabet. */
  { ISFUNC, rl_vi_append_eol },		/* A */
  { ISFUNC, rl_vi_prev_word},		/* B */
  { ISFUNC, rl_vi_change_to },		/* C */
  { ISFUNC, rl_vi_delete_to },		/* D */
  { ISFUNC, rl_vi_end_word },		/* E */
  { ISFUNC, rl_vi_char_search },	/* F */
  { ISFUNC, rl_vi_fetch_history },	/* G */
  { ISFUNC, (VFunction *)0x0 },		/* H */
  { ISFUNC, rl_vi_insert_beg },		/* I */
  { ISFUNC, (VFunction *)0x0 },		/* J */
  { ISFUNC, (VFunction *)0x0 },		/* K */
  { ISFUNC, (VFunction *)0x0 },		/* L */
  { ISFUNC, (VFunction *)0x0 },		/* M */
  { ISFUNC, rl_vi_search_again },	/* N */
  { ISFUNC, (VFunction *)0x0 },		/* O */
  { ISFUNC, rl_vi_put },		/* P */
  { ISFUNC, (VFunction *)0x0 },		/* Q */
  { ISFUNC, rl_vi_replace },		/* R */
  { ISFUNC, rl_vi_subst },		/* S */
  { ISFUNC, rl_vi_char_search },	/* T */
  { ISFUNC, rl_revert_line },		/* U */
  { ISFUNC, (VFunction *)0x0 },		/* V */
  { ISFUNC, rl_vi_next_word },		/* W */
  { ISFUNC, rl_rubout },		/* X */
  { ISFUNC, rl_vi_yank_to },		/* Y */
  { ISFUNC, (VFunction *)0x0 },		/* Z */

  /* Some more punctuation. */
  { ISFUNC, (VFunction *)0x0 },		/* [ */
  { ISFUNC, rl_vi_complete },		/* \ */
  { ISFUNC, (VFunction *)0x0 },		/* ] */
  { ISFUNC, rl_vi_first_print },	/* ^ */
  { ISFUNC, rl_vi_yank_arg },		/* _ */
  { ISFUNC, (VFunction *)0x0 },		/* ` */

  /* Lowercase alphabet. */
  { ISFUNC, rl_vi_append_mode },	/* a */
  { ISFUNC, rl_vi_prev_word },		/* b */
  { ISFUNC, rl_vi_change_to },		/* c */
  { ISFUNC, rl_vi_delete_to },		/* d */
  { ISFUNC, rl_vi_end_word },		/* e */
  { ISFUNC, rl_vi_char_search },	/* f */
  { ISFUNC, (VFunction *)0x0 },		/* g */
  { ISFUNC, rl_backward },		/* h */
  { ISFUNC, rl_vi_insertion_mode },	/* i */
  { ISFUNC, rl_get_next_history },	/* j */
  { ISFUNC, rl_get_previous_history },	/* k */
  { ISFUNC, rl_forward },		/* l */
  { ISFUNC, (VFunction *)0x0 },		/* m */
  { ISFUNC, rl_vi_search_again },	/* n */
  { ISFUNC, (VFunction *)0x0 },		/* o */
  { ISFUNC, rl_vi_put },		/* p */
  { ISFUNC, (VFunction *)0x0 },		/* q */
  { ISFUNC, rl_vi_change_char },	/* r */
  { ISFUNC, rl_vi_subst },		/* s */
  { ISFUNC, rl_vi_char_search },	/* t */
  { ISFUNC, rl_undo_command },		/* u */
  { ISFUNC, (VFunction *)0x0 },		/* v */
  { ISFUNC, rl_vi_next_word },		/* w */
  { ISFUNC, rl_vi_delete },		/* x */
  { ISFUNC, rl_vi_yank_to },		/* y */
  { ISFUNC, (VFunction *)0x0 },		/* z */

  /* Final punctuation. */
  { ISFUNC, (VFunction *)0x0 },		/* { */
  { ISFUNC, rl_vi_column },		/* | */
  { ISFUNC, (VFunction *)0x0 },		/* } */
  { ISFUNC, rl_vi_change_case },	/* ~ */
  { ISFUNC, (VFunction *)0x0 },		/* RUBOUT */

#if KEYMAP_SIZE==256
  /* Latin-1 or DOS characters */
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
#endif

};


KEYMAP_ENTRY_ARRAY vi_insertion_keymap = {

  /* The regular control keys come first. */
  { ISFUNC, (VFunction *)0x0 },		/* Control-@ */
  { ISFUNC, rl_insert },		/* Control-a */
  { ISFUNC, rl_insert },		/* Control-b */
  { ISFUNC, rl_insert },		/* Control-c */
  { ISFUNC, rl_vi_eof_maybe },		/* Control-d */
  { ISFUNC, rl_insert },		/* Control-e */
  { ISFUNC, rl_insert },		/* Control-f */
  { ISFUNC, rl_insert },		/* Control-g */
  { ISFUNC, rl_rubout },		/* Control-h */
  { ISFUNC, rl_complete },		/* Control-i */
  { ISFUNC, rl_newline },		/* Control-j */
  { ISFUNC, rl_insert },		/* Control-k */
  { ISFUNC, rl_insert },		/* Control-l */
  { ISFUNC, rl_newline },		/* Control-m */
  { ISFUNC, rl_insert },		/* Control-n */
  { ISFUNC, rl_insert },		/* Control-o */
  { ISFUNC, rl_insert },		/* Control-p */
  { ISFUNC, rl_insert },		/* Control-q */
  { ISFUNC, rl_reverse_search_history }, /* Control-r */
  { ISFUNC, rl_forward_search_history }, /* Control-s */
  { ISFUNC, rl_transpose_chars },	/* Control-t */
  { ISFUNC, rl_unix_line_discard },	/* Control-u */
  { ISFUNC, rl_quoted_insert },		/* Control-v */
  { ISFUNC, rl_unix_word_rubout },	/* Control-w */
  { ISFUNC, rl_insert },		/* Control-x */
  { ISFUNC, rl_yank },			/* Control-y */
  { ISFUNC, rl_insert },		/* Control-z */

  { ISFUNC, rl_vi_movement_mode },	/* Control-[ */
  { ISFUNC, rl_insert },		/* Control-\ */
  { ISFUNC, rl_insert },		/* Control-] */
  { ISFUNC, rl_insert },		/* Control-^ */
  { ISFUNC, rl_undo_command },		/* Control-_ */

  /* The start of printing characters. */
  { ISFUNC, rl_insert },		/* SPACE */
  { ISFUNC, rl_insert },		/* ! */
  { ISFUNC, rl_insert },		/* " */
  { ISFUNC, rl_insert },		/* # */
  { ISFUNC, rl_insert },		/* $ */
  { ISFUNC, rl_insert },		/* % */
  { ISFUNC, rl_insert },		/* & */
  { ISFUNC, rl_insert },		/* ' */
  { ISFUNC, rl_insert },		/* ( */
  { ISFUNC, rl_insert },		/* ) */
  { ISFUNC, rl_insert },		/* * */
  { ISFUNC, rl_insert },		/* + */
  { ISFUNC, rl_insert },		/* , */
  { ISFUNC, rl_insert },		/* - */
  { ISFUNC, rl_insert },		/* . */
  { ISFUNC, rl_insert },		/* / */

  /* Regular digits. */
  { ISFUNC, rl_insert },		/* 0 */
  { ISFUNC, rl_insert },		/* 1 */
  { ISFUNC, rl_insert },		/* 2 */
  { ISFUNC, rl_insert },		/* 3 */
  { ISFUNC, rl_insert },		/* 4 */
  { ISFUNC, rl_insert },		/* 5 */
  { ISFUNC, rl_insert },		/* 6 */
  { ISFUNC, rl_insert },		/* 7 */
  { ISFUNC, rl_insert },		/* 8 */
  { ISFUNC, rl_insert },		/* 9 */

  /* A little more punctuation. */
  { ISFUNC, rl_insert },		/* : */
  { ISFUNC, rl_insert },		/* ; */
  { ISFUNC, rl_insert },		/* < */
  { ISFUNC, rl_insert },		/* = */
  { ISFUNC, rl_insert },		/* > */
  { ISFUNC, rl_insert },		/* ? */
  { ISFUNC, rl_insert },		/* @ */

  /* Uppercase alphabet. */
  { ISFUNC, rl_insert },		/* A */
  { ISFUNC, rl_insert },		/* B */
  { ISFUNC, rl_insert },		/* C */
  { ISFUNC, rl_insert },		/* D */
  { ISFUNC, rl_insert },		/* E */
  { ISFUNC, rl_insert },		/* F */
  { ISFUNC, rl_insert },		/* G */
  { ISFUNC, rl_insert },		/* H */
  { ISFUNC, rl_insert },		/* I */
  { ISFUNC, rl_insert },		/* J */
  { ISFUNC, rl_insert },		/* K */
  { ISFUNC, rl_insert },		/* L */
  { ISFUNC, rl_insert },		/* M */
  { ISFUNC, rl_insert },		/* N */
  { ISFUNC, rl_insert },		/* O */
  { ISFUNC, rl_insert },		/* P */
  { ISFUNC, rl_insert },		/* Q */
  { ISFUNC, rl_insert },		/* R */
  { ISFUNC, rl_insert },		/* S */
  { ISFUNC, rl_insert },		/* T */
  { ISFUNC, rl_insert },		/* U */
  { ISFUNC, rl_insert },		/* V */
  { ISFUNC, rl_insert },		/* W */
  { ISFUNC, rl_insert },		/* X */
  { ISFUNC, rl_insert },		/* Y */
  { ISFUNC, rl_insert },		/* Z */

  /* Some more punctuation. */
  { ISFUNC, rl_insert },		/* [ */
  { ISFUNC, rl_insert },		/* \ */
  { ISFUNC, rl_insert },		/* ] */
  { ISFUNC, rl_insert },		/* ^ */
  { ISFUNC, rl_insert },		/* _ */
  { ISFUNC, rl_insert },		/* ` */

  /* Lowercase alphabet. */
  { ISFUNC, rl_insert },		/* a */
  { ISFUNC, rl_insert },		/* b */
  { ISFUNC, rl_insert },		/* c */
  { ISFUNC, rl_insert },		/* d */
  { ISFUNC, rl_insert },		/* e */
  { ISFUNC, rl_insert },		/* f */
  { ISFUNC, rl_insert },		/* g */
  { ISFUNC, rl_insert },		/* h */
  { ISFUNC, rl_insert },		/* i */
  { ISFUNC, rl_insert },		/* j */
  { ISFUNC, rl_insert },		/* k */
  { ISFUNC, rl_insert },		/* l */
  { ISFUNC, rl_insert },		/* m */
  { ISFUNC, rl_insert },		/* n */
  { ISFUNC, rl_insert },		/* o */
  { ISFUNC, rl_insert },		/* p */
  { ISFUNC, rl_insert },		/* q */
  { ISFUNC, rl_insert },		/* r */
  { ISFUNC, rl_insert },		/* s */
  { ISFUNC, rl_insert },		/* t */
  { ISFUNC, rl_insert },		/* u */
  { ISFUNC, rl_insert },		/* v */
  { ISFUNC, rl_insert },		/* w */
  { ISFUNC, rl_insert },		/* x */
  { ISFUNC, rl_insert },		/* y */
  { ISFUNC, rl_insert },		/* z */

  /* Final punctuation. */
  { ISFUNC, rl_insert },		/* { */
  { ISFUNC, rl_insert },		/* | */
  { ISFUNC, rl_insert },		/* } */
  { ISFUNC, rl_insert },		/* ~ */
  { ISFUNC, rl_rubout },		/* RUBOUT */

#if KEYMAP_SIZE==256
#ifdef ISOLATIN
  /* Latin-1 characters */
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, rl_insert },	/* nobreakspace */
  { ISFUNC, rl_insert },	/* inv. exclamation */
  { ISFUNC, rl_insert },	/* U.S. cent */
  { ISFUNC, rl_insert },	/* British pound */
  { ISFUNC, rl_insert },	/* general currency */
  { ISFUNC, rl_insert },	/* Japanese yen */
  { ISFUNC, rl_insert },	/* broken bar */
  { ISFUNC, rl_insert },	/* section */
  { ISFUNC, rl_insert },	/* umlaut accent */
  { ISFUNC, rl_insert },	/* copyright */
  { ISFUNC, rl_insert },	/* femin. ordinal */
  { ISFUNC, rl_insert },	/* open guillemets */
  { ISFUNC, rl_insert },	/* not sign */
  { ISFUNC, rl_insert },	/* hyphen */
  { ISFUNC, rl_insert },	/* registered trade */
  { ISFUNC, rl_insert },	/* macron */
  { ISFUNC, rl_insert },	/* degree */
  { ISFUNC, rl_insert },	/* plus/minus */
  { ISFUNC, rl_insert },	/* power 2 */
  { ISFUNC, rl_insert },	/* power 3 */
  { ISFUNC, rl_insert },	/* accent acute */
  { ISFUNC, rl_insert },	/* Greek mu */
  { ISFUNC, rl_insert },	/* paragraph */
  { ISFUNC, rl_insert },	/* middle dot */
  { ISFUNC, rl_insert },	/* cedilla */
  { ISFUNC, rl_insert },	/* power 1 */
  { ISFUNC, rl_insert },	/* masc. ordinal */
  { ISFUNC, rl_insert },	/* close guillemets */
  { ISFUNC, rl_insert },	/* one fourth */
  { ISFUNC, rl_insert },	/* one half */
  { ISFUNC, rl_insert },	/* three fourth */
  { ISFUNC, rl_insert },	/* inv. question */
  { ISFUNC, rl_insert },	/* A accent grave */
  { ISFUNC, rl_insert },	/* A accent acute */
  { ISFUNC, rl_insert },	/* A circumflex */
  { ISFUNC, rl_insert },	/* A tilde */
  { ISFUNC, rl_insert },	/* A umlaut */
  { ISFUNC, rl_insert },	/* A degree */
  { ISFUNC, rl_insert },	/* AE ligature */
  { ISFUNC, rl_insert },	/* C cedilla */
  { ISFUNC, rl_insert },	/* E accent grave */
  { ISFUNC, rl_insert },	/* E accent acute */
  { ISFUNC, rl_insert },	/* E circumflex */
  { ISFUNC, rl_insert },	/* E umlaut */
  { ISFUNC, rl_insert },	/* I accent grave */
  { ISFUNC, rl_insert },	/* I accent acute */
  { ISFUNC, rl_insert },	/* I circumflex */
  { ISFUNC, rl_insert },	/* I umlaut */
  { ISFUNC, rl_insert },	/* D stroke */
  { ISFUNC, rl_insert },	/* N tilde */
  { ISFUNC, rl_insert },	/* O accent grave */
  { ISFUNC, rl_insert },	/* O accent acute */
  { ISFUNC, rl_insert },	/* O circumflex */
  { ISFUNC, rl_insert },	/* O tilde */
  { ISFUNC, rl_insert },	/* O umlaut */
  { ISFUNC, rl_insert },	/* multiply */
  { ISFUNC, rl_insert },	/* O crossbar */
  { ISFUNC, rl_insert },	/* U accent grave */
  { ISFUNC, rl_insert },	/* U accent acute */
  { ISFUNC, rl_insert },	/* U circumflex */
  { ISFUNC, rl_insert },	/* U umlaut */
  { ISFUNC, rl_insert },	/* Y accent acute */
  { ISFUNC, rl_insert },	/* Thorn */
  { ISFUNC, rl_insert },	/* sharp s */
  { ISFUNC, rl_insert },	/* a accent grave */
  { ISFUNC, rl_insert },	/* a accent acute */
  { ISFUNC, rl_insert },	/* a circumflex */
  { ISFUNC, rl_insert },	/* a tilde */
  { ISFUNC, rl_insert },	/* a umlaut */
  { ISFUNC, rl_insert },	/* a degree */
  { ISFUNC, rl_insert },	/* ae ligature */
  { ISFUNC, rl_insert },	/* c cedilla */
  { ISFUNC, rl_insert },	/* e accent grave */
  { ISFUNC, rl_insert },	/* e accent acute */
  { ISFUNC, rl_insert },	/* e circumflex */
  { ISFUNC, rl_insert },	/* e umlaut */
  { ISFUNC, rl_insert },	/* i accent grave */
  { ISFUNC, rl_insert },	/* i accent acute */
  { ISFUNC, rl_insert },	/* i circumflex */
  { ISFUNC, rl_insert },	/* i umlaut */
  { ISFUNC, rl_insert },	/* d stroke */
  { ISFUNC, rl_insert },	/* n tilde */
  { ISFUNC, rl_insert },	/* o accent grave */
  { ISFUNC, rl_insert },	/* o accent acute */
  { ISFUNC, rl_insert },	/* o circumflex */
  { ISFUNC, rl_insert },	/* o tilde */
  { ISFUNC, rl_insert },	/* o umlaut */
  { ISFUNC, rl_insert },	/* divide */
  { ISFUNC, rl_insert },	/* o crossbar */
  { ISFUNC, rl_insert },	/* u accent grave */
  { ISFUNC, rl_insert },	/* u accent acute */
  { ISFUNC, rl_insert },	/* u circumflex */
  { ISFUNC, rl_insert },	/* u umlaut */
  { ISFUNC, rl_insert },	/* y accent acute */
  { ISFUNC, rl_insert },	/* thorn */
  { ISFUNC, rl_insert },	/* y umlaut */
#else /* defined(DOSCHARS) || defined(NEXTCHARS) */
  /* DOS characters */
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
  { ISFUNC, rl_insert },
#endif
#endif

};

KEYMAP_ENTRY_ARRAY vi_escape_keymap = {

  /* The regular control keys come first. */
  { ISFUNC, (VFunction *)0x0 },			/* Control-@ */
  { ISFUNC, (VFunction *)0x0 },			/* Control-a */
  { ISFUNC, (VFunction *)0x0 },			/* Control-b */
  { ISFUNC, (VFunction *)0x0 },			/* Control-c */
  { ISFUNC, (VFunction *)0x0 },			/* Control-d */
  { ISFUNC, (VFunction *)0x0 },			/* Control-e */
  { ISFUNC, (VFunction *)0x0 },			/* Control-f */
  { ISFUNC, (VFunction *)0x0 },			/* Control-g */
  { ISFUNC, (VFunction *)0x0 },			/* Control-h */
  { ISFUNC, rl_tab_insert},			/* Control-i */
  { ISFUNC, rl_emacs_editing_mode},		/* Control-j */
  { ISFUNC, rl_kill_line },			/* Control-k */
  { ISFUNC, (VFunction *)0x0 },			/* Control-l */
  { ISFUNC, rl_emacs_editing_mode},		/* Control-m */
  { ISFUNC, (VFunction *)0x0 },			/* Control-n */
  { ISFUNC, (VFunction *)0x0 },			/* Control-o */
  { ISFUNC, (VFunction *)0x0 },			/* Control-p */
  { ISFUNC, (VFunction *)0x0 },			/* Control-q */
  { ISFUNC, (VFunction *)0x0 },			/* Control-r */
  { ISFUNC, (VFunction *)0x0 },			/* Control-s */
  { ISFUNC, (VFunction *)0x0 },			/* Control-t */
  { ISFUNC, (VFunction *)0x0 },			/* Control-u */
  { ISFUNC, (VFunction *)0x0 },			/* Control-v */
  { ISFUNC, (VFunction *)0x0 },			/* Control-w */
  { ISFUNC, (VFunction *)0x0 },			/* Control-x */
  { ISFUNC, (VFunction *)0x0 },			/* Control-y */
  { ISFUNC, (VFunction *)0x0 },			/* Control-z */

  { ISFUNC, rl_vi_movement_mode },		/* Control-[ */
  { ISFUNC, (VFunction *)0x0 },			/* Control-\ */
  { ISFUNC, (VFunction *)0x0 },			/* Control-] */
  { ISFUNC, (VFunction *)0x0 },			/* Control-^ */
  { ISFUNC, rl_undo_command },			/* Control-_ */

  /* The start of printing characters. */
  { ISFUNC, (VFunction *)0x0 },		/* SPACE */
  { ISFUNC, (VFunction *)0x0 },		/* ! */
  { ISFUNC, (VFunction *)0x0 },		/* " */
  { ISFUNC, (VFunction *)0x0 },		/* # */
  { ISFUNC, (VFunction *)0x0 },		/* $ */
  { ISFUNC, (VFunction *)0x0 },		/* % */
  { ISFUNC, (VFunction *)0x0 },		/* & */
  { ISFUNC, (VFunction *)0x0 },		/* ' */
  { ISFUNC, (VFunction *)0x0 },		/* ( */
  { ISFUNC, (VFunction *)0x0 },		/* ) */
  { ISFUNC, (VFunction *)0x0 },		/* * */
  { ISFUNC, (VFunction *)0x0 },		/* + */
  { ISFUNC, (VFunction *)0x0 },		/* , */
  { ISFUNC, (VFunction *)0x0 },		/* - */
  { ISFUNC, (VFunction *)0x0 },		/* . */
  { ISFUNC, (VFunction *)0x0 },		/* / */

  /* Regular digits. */
  { ISFUNC, rl_vi_arg_digit },		/* 0 */
  { ISFUNC, rl_vi_arg_digit },		/* 1 */
  { ISFUNC, rl_vi_arg_digit },		/* 2 */
  { ISFUNC, rl_vi_arg_digit },		/* 3 */
  { ISFUNC, rl_vi_arg_digit },		/* 4 */
  { ISFUNC, rl_vi_arg_digit },		/* 5 */
  { ISFUNC, rl_vi_arg_digit },		/* 6 */
  { ISFUNC, rl_vi_arg_digit },		/* 7 */
  { ISFUNC, rl_vi_arg_digit },		/* 8 */
  { ISFUNC, rl_vi_arg_digit },		/* 9 */

  /* A little more punctuation. */
  { ISFUNC, (VFunction *)0x0 },		/* : */
  { ISFUNC, (VFunction *)0x0 },		/* ; */
  { ISFUNC, (VFunction *)0x0 },		/* < */
  { ISFUNC, (VFunction *)0x0 },		/* = */
  { ISFUNC, (VFunction *)0x0 },		/* > */
  { ISFUNC, (VFunction *)0x0 },		/* ? */
  { ISFUNC, (VFunction *)0x0 },		/* @ */

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
  { ISFUNC, rl_arrow_keys },		/* [ */
  { ISFUNC, (VFunction *)0x0 },		/* \ */
  { ISFUNC, (VFunction *)0x0 },		/* ] */
  { ISFUNC, (VFunction *)0x0 },		/* ^ */
  { ISFUNC, (VFunction *)0x0 },		/* _ */
  { ISFUNC, (VFunction *)0x0 },		/* ` */

  /* Lowercase alphabet. */
  { ISFUNC, (VFunction *)0x0 },		/* a */
  { ISFUNC, (VFunction *)0x0 },		/* b */
  { ISFUNC, (VFunction *)0x0 },		/* c */
  { ISFUNC, (VFunction *)0x0 },		/* d */
  { ISFUNC, (VFunction *)0x0 },		/* e */
  { ISFUNC, (VFunction *)0x0 },		/* f */
  { ISFUNC, (VFunction *)0x0 },		/* g */
  { ISFUNC, (VFunction *)0x0 },		/* h */
  { ISFUNC, (VFunction *)0x0 },		/* i */
  { ISFUNC, (VFunction *)0x0 },		/* j */
  { ISFUNC, (VFunction *)0x0 },		/* k */
  { ISFUNC, (VFunction *)0x0 },		/* l */
  { ISFUNC, (VFunction *)0x0 },		/* m */
  { ISFUNC, (VFunction *)0x0 },		/* n */
  { ISFUNC, rl_arrow_keys },		/* o */
  { ISFUNC, (VFunction *)0x0 },		/* p */
  { ISFUNC, (VFunction *)0x0 },		/* q */
  { ISFUNC, (VFunction *)0x0 },		/* r */
  { ISFUNC, (VFunction *)0x0 },		/* s */
  { ISFUNC, (VFunction *)0x0 },		/* t */
  { ISFUNC, (VFunction *)0x0 },		/* u */
  { ISFUNC, (VFunction *)0x0 },		/* v */
  { ISFUNC, (VFunction *)0x0 },		/* w */
  { ISFUNC, (VFunction *)0x0 },		/* x */
  { ISFUNC, (VFunction *)0x0 },		/* y */
  { ISFUNC, (VFunction *)0x0 },		/* z */

  /* Final punctuation. */
  { ISFUNC, (VFunction *)0x0 },		/* { */
  { ISFUNC, (VFunction *)0x0 },		/* | */
  { ISFUNC, (VFunction *)0x0 },		/* } */
  { ISFUNC, (VFunction *)0x0 },		/* ~ */
  { ISFUNC, rl_backward_kill_word }, 	/* RUBOUT */

#if KEYMAP_SIZE==256
  /* Latin-1 or DOS characters */
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
  { ISFUNC, (VFunction *)0x0 },
#endif

};
