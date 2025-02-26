/**********************************************************************

  ruby.c -

  $Author: nobu $
  $Date: 2002/02/10 07:54:43 $
  created at: Tue Aug 10 12:47:31 JST 1993

  Copyright (C) 1993-2001 Yukihiro Matsumoto
  Copyright (C) 2000  Network Applied Communication Laboratory, Inc.
  Copyright (C) 2000  Information-technology Promotion Agency, Japan

**********************************************************************/

#if defined _WIN32 || defined __CYGWIN__
#include <windows.h>
#endif
#include "ruby.h"
#include "dln.h"
#include "node.h"
#include <stdio.h>
#include <sys/types.h>
#include <ctype.h>

#ifdef __hpux
#include <sys/pstat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef HAVE_STRING_H
char *strchr _((const char*,const char));
char *strrchr _((const char*,const char));
char *strstr _((const char*,const char*));
#endif

#include "util.h"

#ifndef HAVE_STDLIB_H
char *getenv();
#endif

VALUE ruby_debug = Qfalse;
VALUE ruby_verbose = Qfalse;
static int sflag = 0;
static int xflag = 0;
extern int yydebug;

char *ruby_inplace_mode = Qfalse;

static void load_stdin _((void));
static void load_file _((char *, int));
static void forbid_setid _((const char *));

static VALUE do_loop = Qfalse, do_print = Qfalse;
static VALUE do_check = Qfalse, do_line = Qfalse;
static VALUE do_split = Qfalse;

static char *script;

static int origargc;
static char **origargv;

static void
usage(name)
    const char *name;
{
    /* This message really ought to be max 23 lines.
     * Removed -h because the user already knows that option. Others? */

    static char *usage_msg[] = {
"-0[octal]       specify record separator (\\0, if no argument)",
"-a              autosplit mode with -n or -p (splits $_ into $F)",
"-c              check syntax only",
"-Cdirectory     cd to directory, before executing your script",
"-d              set debugging flags (set $DEBUG to true)",
"-e 'command'    one line of script. Several -e's allowed. Omit [programfile]",
"-Fpattern       split() pattern for autosplit (-a)",
"-i[extension]   edit ARGV files in place (make backup if extension supplied)",
"-Idirectory     specify $LOAD_PATH directory (may be used more than once)",
"-Kkcode         specifies KANJI (Japanese) code-set",
"-l              enable line ending processing",
"-n              assume 'while gets(); ... end' loop around your script",
"-p              assume loop like -n but print line also like sed",
"-rlibrary       require the library, before executing your script",
"-s              enable some switch parsing for switches after script name",
"-S              look for the script using PATH environment variable",
"-T[level]       turn on tainting checks",
"-v              print version number, then turn on verbose mode",
"-w              turn warnings on for your script",
"-x[directory]   strip off text before #!ruby line and perhaps cd to directory",
"--copyright     print the copyright",
"--version       print the version",
NULL
};
    char **p = usage_msg;

    printf("Usage: %s [switches] [--] [programfile] [arguments]\n", name);
    while (*p)
	printf("  %s\n", *p++);
}

extern VALUE rb_load_path;

#define STATIC_FILE_LENGTH 255

#if defined _WIN32 || defined __CYGWIN__ || defined __DJGPP__
static char *
rubylib_mangle(s, l)
    char *s;
    unsigned int l;
{
    static char *newp, *oldp;
    static int newl, oldl, notfound;
    static char newsub[STATIC_FILE_LENGTH+1];

    if (!newp && !notfound) {
	newp = getenv("RUBYLIB_PREFIX");
	if (newp) {
	    char *s;

	    oldp = newp;
	    while (*newp && !ISSPACE(*newp) && *newp != ';') {
		newp++; oldl++;		/* Skip digits. */
	    }
	    while (*newp && (ISSPACE(*newp) || *newp == ';')) {
		newp++;			/* Skip whitespace. */
	    }
	    newl = strlen(newp);
	    if (newl == 0 || oldl == 0 || newl > STATIC_FILE_LENGTH) {
		rb_fatal("malformed RUBYLIB_PREFIX");
	    }
	    strcpy(newsub, newp);
	    s = newsub;
	    while (*s) {
		if (*s == '\\') *s = '/';
		s++;
	    }
	} else {
	    notfound = 1;
	}
    }
    if (l == 0) {
	l = strlen(s);
    }
    if (!newp || l < oldl || strncasecmp(oldp, s, oldl) != 0) {
	static char ret[STATIC_FILE_LENGTH+1];
	strncpy(ret, s, l);
	ret[l] = 0;
	return ret;
    }
    if (l + newl - oldl > STATIC_FILE_LENGTH || newl > STATIC_FILE_LENGTH) {
	rb_fatal("malformed RUBYLIB_PREFIX");
    }
    strcpy(newsub + newl, s + oldl);
    newsub[l + newl - oldl] = 0;
    return newsub;
}
#define rubylib_mangled_path(s, l) rb_str_new2(rubylib_mangle((s), (l)))
#define rubylib_mangled_path2(s) rb_str_new2(rubylib_mangle((s), 0))
#else
#define rubylib_mangled_path(s, l) rb_str_new((s), (l))
#define rubylib_mangled_path2(s) rb_str_new2(s)
#endif

void
ruby_incpush(path)
    const char *path;
{
    const char sep = PATH_SEP_CHAR;

    if (path == 0) return;
#if defined(__CYGWIN__)
    {
	char rubylib[FILENAME_MAX];
	conv_to_posix_path(path, rubylib, FILENAME_MAX);
	path = rubylib;
    }
#endif
    if (strchr(path, sep)) {
	const char *p, *s;
	VALUE ary = rb_ary_new();

	p = path;
	while (*p) {
	    while (*p == sep) p++;
	    if (s = strchr(p, sep)) {
		rb_ary_push(ary, rubylib_mangled_path(p, (int)(s-p)));
		p = s + 1;
	    }
	    else {
		rb_ary_push(ary, rubylib_mangled_path2(p));
		break;
	    }
	}
	rb_ary_concat(rb_load_path, ary);
    }
    else {
	rb_ary_push(rb_load_path, rubylib_mangled_path2(path));
    }
}

#if defined _WIN32 || defined __CYGWIN__ || defined __DJGPP__ || defined __EMX__
#define LOAD_RELATIVE 1
#endif

void
ruby_init_loadpath()
{
#if defined LOAD_RELATIVE
    char libpath[FILENAME_MAX+1];
    char *p;
    int rest;
#if defined _WIN32 || defined __CYGWIN__
# if defined LIBRUBY_SO
    HMODULE libruby = GetModuleHandle(LIBRUBY_SO);
# else
    HMODULE libruby = NULL;
# endif
    GetModuleFileName(libruby, libpath, sizeof libpath);
#elif defined(DJGPP)
    extern char *__dos_argv0;
    strncpy(libpath, __dos_argv0, FILENAME_MAX);
#define CharNext(p) ((p) + mblen(p, MB_CUR_MAX))
#elif defined(__EMX__)
    _execname(libpath, FILENAME_MAX);
#endif

#ifndef CharNext		/* defined as CharNext[AW] on Windows. */
#define CharNext(p) ((p) + 1)
#endif

    for (p = libpath; *p; p = CharNext(p))
	if (*p == '\\')
	    *p = '/';

    p = strrchr(libpath, '/');
    if (p) {
	*p = 0;
	if (p-libpath > 3 && !strcasecmp(p-4, "/bin")) {
	    p -= 4;
	    *p = 0;
	}
    } else {
	strcpy(libpath, ".");
	p = libpath + 1;
    }

    rest = FILENAME_MAX - (p - libpath);

#define RUBY_RELATIVE(path) (strncpy(p, (path), rest), libpath)
#else
#define RUBY_RELATIVE(path) (path)
#endif

    if (rb_safe_level() == 0) {
	ruby_incpush(getenv("RUBYLIB"));
    }

#ifdef RUBY_SEARCH_PATH
    ruby_incpush(RUBY_RELATIVE(RUBY_SEARCH_PATH));
#endif

    ruby_incpush(RUBY_RELATIVE(RUBY_SITE_LIB2));
#ifdef RUBY_SITE_THIN_ARCHLIB
    ruby_incpush(RUBY_RELATIVE(RUBY_SITE_THIN_ARCHLIB));
#endif
    ruby_incpush(RUBY_RELATIVE(RUBY_SITE_ARCHLIB));
    ruby_incpush(RUBY_RELATIVE(RUBY_SITE_LIB));

    ruby_incpush(RUBY_RELATIVE(RUBY_LIB));
#ifdef RUBY_THIN_ARCHLIB
    ruby_incpush(RUBY_RELATIVE(RUBY_THIN_ARCHLIB));
#endif
    ruby_incpush(RUBY_RELATIVE(RUBY_ARCHLIB));

    if (rb_safe_level() == 0) {
	ruby_incpush(".");
    }
}

struct req_list {
    char *name;
    struct req_list *next;
};
static struct req_list req_list_head, *req_list_last = &req_list_head;

static void
add_modules(mod)
    const char *mod;
{
    struct req_list *list;

    list = ALLOC(struct req_list);
    list->name = ALLOC_N(char, strlen(mod)+1);
    strcpy(list->name, mod);
    list->next = 0;
    req_list_last->next = list;
    req_list_last = list;
}

extern void Init_ext _((void));

static void
require_libraries()
{
    extern NODE *ruby_eval_tree;
    extern NODE *ruby_eval_tree_begin;
    char *orig_sourcefile = ruby_sourcefile;
    NODE *save[2];
    struct req_list *list = req_list_head.next;
    struct req_list *tmp;

    Init_ext();		/* should be called here for some reason :-( */
    ruby_sourcefile = 0;
    save[0] = ruby_eval_tree;
    save[1] = ruby_eval_tree_begin;
    ruby_eval_tree = ruby_eval_tree_begin = 0;
    req_list_last = 0;
    while (list) {
	rb_require(list->name);
	tmp = list->next;
	free(list->name);
	free(list);
	list = tmp;
    }
    req_list_head.next = 0;
    ruby_eval_tree = save[0];
    ruby_eval_tree_begin = save[1];
    ruby_sourcefile = orig_sourcefile;
}

static void
process_sflag()
{
    if (sflag) {
	int n;
	VALUE *args;

	n = RARRAY(rb_argv)->len;
	args = RARRAY(rb_argv)->ptr;
	while (n > 0) {
	    char *s = STR2CSTR(*args++);
	    char *p;

	    if (s[0] != '-') break;
	    n--;
	    if (s[1] == '-' && s[2] == '\0') break;

	    s[0] = '$';
	    if (p = strchr(s, '=')) {
		*p++ = '\0';
		rb_gv_set(s, rb_str_new2(p));
	    }
	    else {
		rb_gv_set(s, Qtrue);
	    }
	    s[0] = '-';
	}
	n = RARRAY(rb_argv)->len - n;
	while (n--) {
	    rb_ary_shift(rb_argv);
	}
    }
    sflag = 0;
}

static void proc_options _((int argc, char **argv));

static char*
moreswitches(s)
    char *s;
{
    int argc; char *argv[3];
    char *p = s;

    argc = 2; argv[0] = argv[2] = 0;
    while (*s && !ISSPACE(*s))
	s++;
    argv[1] = ALLOCA_N(char, s-p+2);
    argv[1][0] = '-';
    strncpy(argv[1]+1, p, s-p);
    argv[1][s-p+1] = '\0';
    proc_options(argc, argv);
    while (*s && ISSPACE(*s))
	s++;
    return s;
}

static void
proc_options(argc, argv)
    int argc;
    char **argv;
{
    char *argv0 = argv[0];
    int do_search;
    char *s;

    int version = 0;
    int copyright = 0;
    int verbose = 0;
    VALUE e_script = Qfalse;

    if (argc == 0) return;

    do_search = Qfalse;

    for (argc--,argv++; argc > 0; argc--,argv++) {
	if (argv[0][0] != '-' || !argv[0][1]) break;

	s = argv[0]+1;
      reswitch:
	switch (*s) {
	  case 'a':
	    do_split = Qtrue;
	    s++;
	    goto reswitch;

	  case 'p':
	    do_print = Qtrue;
	    /* through */
	  case 'n':
	    do_loop = Qtrue;
	    s++;
	    goto reswitch;

	  case 'd':
	    ruby_debug = Qtrue;
	    ruby_verbose = Qtrue;
	    s++;
	    goto reswitch;

	  case 'y':
	    yydebug = 1;
	    s++;
	    goto reswitch;

	  case 'v':
	    if (verbose) {
		s++;
		goto reswitch;
	    }
	    ruby_show_version();
	    verbose = 1;
	  case 'w':
	    ruby_verbose = Qtrue;
	    s++;
	    goto reswitch;

	  case 'c':
	    do_check = Qtrue;
	    s++;
	    goto reswitch;

	  case 's':
	    forbid_setid("-s");
	    sflag = 1;
	    s++;
	    goto reswitch;

	  case 'h':
	    usage(origargv[0]);
	    exit(0);

	  case 'l':
	    do_line = Qtrue;
	    rb_output_rs = rb_rs;
	    s++;
	    goto reswitch;

	  case 'S':
	    forbid_setid("-S");
	    do_search = Qtrue;
	    s++;
	    goto reswitch;

	  case 'e':
	    forbid_setid("-e");
	    if (!*++s) {
		s = argv[1];
		argc--,argv++;
	    }
	    if (!s) {
		fprintf(stderr, "%s: no code specified for -e\n", origargv[0]);
		exit(2);
	    }
	    if (!e_script) {
		e_script = rb_str_new(0,0);
		if (script == 0) script = "-e";
	    }
	    rb_str_cat2(e_script, s);
	    rb_str_cat2(e_script, "\n");
	    break;

	  case 'r':
	    forbid_setid("-r");
	    if (*++s) {
		add_modules(s);
	    }
	    else if (argv[1]) {
		add_modules(argv[1]);
		argc--,argv++;
	    }
	    break;

	  case 'i':
	    forbid_setid("-i");
	    if (ruby_inplace_mode) free(ruby_inplace_mode);
	    ruby_inplace_mode = strdup(s+1);
	    break;

	  case 'x':
	    xflag = Qtrue;
	    s++;
	    if (*s && chdir(s) < 0) {
		rb_fatal("Can't chdir to %s", s);
	    }
	    break;

	  case 'C':
	  case 'X':
	    s++;
	    if (!*s) {
		s = argv[1];
		argc--,argv++;
	    }
	    if (!s || !*s) {
		rb_fatal("Can't chdir");
	    }
	    if (chdir(s) < 0) {
		rb_fatal("Can't chdir to %s", s);
	    }
	    break;

	  case 'F':
	    if (*++s) {
		rb_fs = rb_str_new2(s);
	    }
	    break;

	  case 'K':
	    if (*++s) {
		rb_set_kcode(s);
		s++;
	    }
	    goto reswitch;

	  case 'T':
	    {
		int numlen;
		int v = 1;

		if (*++s) {
		    v = scan_oct(s, 2, &numlen);
		    if (numlen == 0) v = 1;
		}
		rb_set_safe_level(v);
	    }
	    break;

	  case 'I':
	    forbid_setid("-I");
	    if (*++s)
		ruby_incpush(s);
	    else if (argv[1]) {
		ruby_incpush(argv[1]);
		argc--,argv++;
	    }
	    break;

	  case '0':
	    {
		int numlen;
		int v;
		char c;

		v = scan_oct(s, 4, &numlen);
		s += numlen;
		if (v > 0377) rb_rs = Qnil;
		else if (v == 0 && numlen >= 2) {
		    rb_rs = rb_str_new2("\n\n");
		}
		else {
		    c = v & 0xff;
		    rb_rs = rb_str_new(&c, 1);
		}
	    }
	    goto reswitch;

	  case '-':
	    if (!s[1] || s[1] == '\r' && !s[2]) {
		argc--,argv++;
		goto switch_end;
	    }
	    s++;
	    if (strcmp("copyright", s) == 0)
		copyright = 1;
	    else if (strcmp("debug", s) == 0) {
		ruby_debug = Qtrue;
                ruby_verbose = Qtrue;
            }
	    else if (strcmp("version", s) == 0)
		version = 1;
	    else if (strcmp("verbose", s) == 0) {
		verbose = 1;
		ruby_verbose = Qtrue;
	    }
	    else if (strcmp("yydebug", s) == 0)
		yydebug = 1;
	    else if (strcmp("help", s) == 0) {
		usage(origargv[0]);
		exit(0);
	    }
	    else {
		fprintf(stderr, "%s: invalid option --%s  (-h will show valid options)\n",
			origargv[0], s);
		exit(2);
	    }
	    break;

	  case '*':
	  case ' ':
	    if (s[1] == '-') s+=2;
	    break;

	  default:
	    fprintf(stderr, "%s: invalid option -%c  (-h will show valid options)\n",
		    origargv[0], *s);
	    exit(2);

	  case 0:
	    break;
	}
    }

  switch_end:
    if (argv0 == 0) return;

    if (rb_safe_level() == 0 && (s = getenv("RUBYOPT"))) {
	while (ISSPACE(*s)) s++;
	if (*s == '-' && *(s+1) == 'T') {
	    int numlen;
	    int v = 1;

	    s += 2;
	    if (*++s) {
		v = scan_oct(s, 2, &numlen);
		if (numlen == 0) v = 1;
	    }
	    rb_set_safe_level(v);
	}
	else {
	    while (s && *s) {
		while (ISSPACE(*s)) s++;
		if (*s == '-') {
		    s++;
		    if (ISSPACE(*s)) continue;
		}
		if (!*s) break;
		if (!strchr("IdvwrK", *s))
		    rb_raise(rb_eRuntimeError, "Illegal switch in RUBYOPT: -%c", *s);
		s = moreswitches(s);
	    }
	}
    }

    if (version) {
	ruby_show_version();
	exit(0);
    }
    if (copyright) {
	ruby_show_copyright();
    }

    if (rb_safe_level() >= 4) {
      OBJ_TAINT(rb_argv);
      OBJ_TAINT(rb_load_path);
    }

    if (!e_script && argc == 0) { /* no more args */
	if (verbose) exit(0);
	script = "-";
    }
    else {
	if (!e_script) {
	    script = argv[0];
	}
	if (script[0] == '\0') {
	    script = "-";
	}
	else {
	    if (do_search) {
		char *path = getenv("RUBYPATH");

		script = 0;
		if (path) {
		    script = dln_find_file(argv[0], path);
		}
		if (!script) {
		    script = dln_find_file(argv[0], getenv("PATH"));
		}
		if (!script) script = argv[0];
	    }
	}
	if (!e_script) {
	    argc--; argv++;
	}
    }

    ruby_script(script);
    ruby_set_argv(argc, argv);
    process_sflag();

    ruby_init_loadpath();
    ruby_sourcefile = argv0;
    if (e_script) {
	require_libraries();
	rb_compile_string(script, e_script, 1);
    }
    else if (strlen(script) == 1 && script[0] == '-') {
	load_stdin();
    }
    else {
	load_file(script, 1);
    }

    process_sflag();
    xflag = 0;

    if (rb_safe_level() >= 4) {
      FL_UNSET(rb_argv, FL_TAINT);
      FL_UNSET(rb_load_path, FL_TAINT);
    }
}

extern int ruby__end__seen;

static void
load_file(fname, script)
    char *fname;
    int script;
{
    extern VALUE rb_stdin;
    VALUE f;
    int line_start = 1;

    if (strcmp(fname, "-") == 0) {
	f = rb_stdin;
    }
    else {
	FILE *fp = fopen(fname, "r");

	if (fp == NULL) {
	    rb_load_fail(fname);
	}
	fclose(fp);

	f = rb_file_open(fname, "r");
#if defined DOSISH || defined __CYGWIN__
	{
	    char *ext = strrchr(fname, '.');
	    if (ext && strcasecmp(ext, ".exe") == 0)
		rb_io_binmode(f);
	}
#endif
    }

    if (script) {
	VALUE c = 1;		/* something not nil */
	VALUE line;
	char *p;

	if (xflag) {
	    forbid_setid("-x");
	    xflag = Qfalse;
	    while (!NIL_P(line = rb_io_gets(f))) {
		line_start++;
		if (RSTRING(line)->len > 2
		    && RSTRING(line)->ptr[0] == '#'
		    && RSTRING(line)->ptr[1] == '!') {
		    if (p = strstr(RSTRING(line)->ptr, "ruby")) {
			goto start_read;
		    }
		}
	    }
	    rb_raise(rb_eLoadError, "No Ruby script found in input");
	}

	c = rb_io_getc(f);
	if (c == INT2FIX('#')) {
	    line = rb_io_gets(f);
	    if (NIL_P(line)) return;
	    line_start++;

	    if (RSTRING(line)->len > 2 && RSTRING(line)->ptr[0] == '!') {
		if ((p = strstr(RSTRING(line)->ptr, "ruby")) == 0) {
		    /* not ruby script, kick the program */
		    char **argv;
		    char *path;
		    char *pend = RSTRING(line)->ptr + RSTRING(line)->len;

		    p = RSTRING(line)->ptr + 1;	/* skip `#!' */
		    if (pend[-1] == '\n') pend--; /* chomp line */
		    if (pend[-1] == '\r') pend--;
		    *pend = '\0';
		    while (p < pend && ISSPACE(*p))
			p++;
		    path = p;	/* interpreter path */
		    while (p < pend && !ISSPACE(*p))
			p++;
		    *p++ = '\0';
		    if (p < pend) {
			argv = ALLOCA_N(char*, origargc+3);
			argv[1] = p;
			MEMCPY(argv+2, origargv+1, char*, origargc);
		    }
		    else {
			argv = origargv;
		    }
		    argv[0] = path;
		    execv(path, argv);

		    ruby_sourcefile = fname;
		    ruby_sourceline = 1;
		    rb_fatal("Can't exec %s", path);
		}

	      start_read:
		p += 4;
		RSTRING(line)->ptr[RSTRING(line)->len-1] = '\0';
		if (RSTRING(line)->ptr[RSTRING(line)->len-2] == '\r')
		    RSTRING(line)->ptr[RSTRING(line)->len-2] = '\0';
		if (p = strstr(p, " -")) {
		    p++;	/* skip space before `-' */
		    while (*p == '-') {
			p = moreswitches(p+1);
		    }
		}
	    }
	}
	else if (!NIL_P(c)) {
	    rb_io_ungetc(f, c);
	}
	require_libraries();	/* Why here? unnatural */
	if (NIL_P(c)) return;
    }
    rb_compile_file(fname, f, line_start);
    if (script && ruby__end__seen) {
	rb_define_global_const("DATA", f);
    }
    else if (f != rb_stdin) {
	rb_io_close(f);
    }
    rb_gc();
}

void
rb_load_file(fname)
    char *fname;
{
    load_file(fname, 0);
}

static void
load_stdin()
{
    forbid_setid("program input from stdin");
    load_file("-", 1);
}

VALUE rb_progname;
VALUE rb_argv;
VALUE rb_argv0;

static void
set_arg0(val, id)
    VALUE val;
    ID id;
{
    char *s;
    int i;
    static int len;

    if (origargv == 0) rb_raise(rb_eRuntimeError, "$0 not initialized");
    s = rb_str2cstr(val, &i);
#ifdef __hpux
    if (i >= PST_CLEN) {
      union pstun j;
      j.pst_command = s;
      i = PST_CLEN;
      RSTRING(val)->len = i;
      *(s + i) = '\0';
      pstat(PSTAT_SETCMD, j, PST_CLEN, 0, 0);
    } else {
      union pstun j;
      j.pst_command = s;
      pstat(PSTAT_SETCMD, j, i, 0, 0);
    }
    rb_progname = rb_tainted_str_new(s, i);
#elif defined(HAVE_SETPROCTITLE)
    setproctitle("%.*s", i, s);
    rb_progname = rb_tainted_str_new(s, i);
#else
    if (len == 0) {
	char *s = origargv[0];
	int i;

	s += strlen(s);
	/* See if all the arguments are contiguous in memory */
	for (i = 1; i < origargc; i++) {
	    if (origargv[i] == s + 1)
		s += strlen(++s);	/* this one is ok too */
	}
	len = s - origargv[0];
    }
    if (i >= len) {
	i = len;
	memcpy(origargv[0], s, i);
	origargv[0][i] = '\0';
    }
    else {
	memcpy(origargv[0], s, i);
	s = origargv[0]+i;
	*s++ = '\0';
	while (++i < len)
	    *s++ = ' ';
	for (i = 1; i < origargc; i++)
	    origargv[i] = 0;
    }
    rb_progname = rb_tainted_str_new2(origargv[0]);
#endif
}

void
ruby_script(name)
    char *name;
{
    if (name) {
	rb_progname = rb_tainted_str_new2(name);
	ruby_sourcefile = name;
    }
}

static int uid, euid, gid, egid;

static void
init_ids()
{
    uid = (int)getuid();
    euid = (int)geteuid();
    gid = (int)getgid();
    egid = (int)getegid();
#ifdef VMS
    uid |= gid << 16;
    euid |= egid << 16;
#endif
    if (uid && (euid != uid || egid != gid)) {
	rb_set_safe_level(1);
    }
}

static void
forbid_setid(s)
    const char *s;
{
    if (euid != uid)
        rb_raise(rb_eSecurityError, "No %s allowed while running setuid", s);
    if (egid != gid)
        rb_raise(rb_eSecurityError, "No %s allowed while running setgid", s);
    if (rb_safe_level() > 0)
        rb_raise(rb_eSecurityError, "No %s allowed in tainted mode", s);
}

void
ruby_prog_init()
{
    init_ids();

    ruby_sourcefile = "ruby";
    rb_define_variable("$VERBOSE", &ruby_verbose);
    rb_define_variable("$-v", &ruby_verbose);
    rb_define_variable("$-w", &ruby_verbose);
    rb_define_variable("$DEBUG", &ruby_debug);
    rb_define_variable("$-d", &ruby_debug);
    rb_define_readonly_variable("$-p", &do_print);
    rb_define_readonly_variable("$-l", &do_line);

    rb_define_hooked_variable("$0", &rb_progname, 0, set_arg0);

    rb_argv = rb_ary_new();
    rb_define_readonly_variable("$*", &rb_argv);
    rb_define_global_const("ARGV", rb_argv);
    rb_define_readonly_variable("$-a", &do_split);
    rb_global_variable(&rb_argv0);

#ifdef MSDOS
    /*
     * There is no way we can refer to them from ruby, so close them to save
     * space.
     */
    (void)fclose(stdaux);
    (void)fclose(stdprn);
#endif
}

void
ruby_set_argv(argc, argv)
    int argc;
    char **argv;
{
    int i;

#if defined(USE_DLN_A_OUT)
    if (origargv) dln_argv0 = origargv[0];
    else          dln_argv0 = argv[0];
#endif
    rb_ary_clear(rb_argv);
    for (i=0; i < argc; i++) {
	rb_ary_push(rb_argv, rb_tainted_str_new2(argv[i]));
    }
}

void
ruby_process_options(argc, argv)
    int argc;
    char **argv;
{
    origargc = argc; origargv = argv;
    ruby_script(argv[0]);	/* for the time being */
    rb_argv0 = rb_progname;
#if defined(USE_DLN_A_OUT)
    dln_argv0 = argv[0];
#endif
    proc_options(argc, argv);

    if (do_check && ruby_nerrs == 0) {
	printf("Syntax OK\n");
	exit(0);
    }
    if (do_print) {
	rb_parser_append_print();
    }
    if (do_loop) {
	rb_parser_while_loop(do_line, do_split);
    }
}
