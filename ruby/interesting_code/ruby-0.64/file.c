
/************************************************

  file.c -

  $Author: matz $
  $Date: 1995/01/10 10:42:36 $
  created at: Mon Nov 15 12:24:34 JST 1993

  Copyright (C) 1994 Yukihiro Matsumoto

************************************************/

#include <sys/param.h>
#include <sys/time.h>
#include "ruby.h"
#include "io.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

char *strdup();

extern VALUE C_IO;
VALUE C_File;
VALUE M_FileTest;

VALUE time_new();

VALUE
file_open(fname, mode)
    char *fname, *mode;
{
    VALUE port;
    OpenFile *fptr;

    port = obj_alloc(C_File);

    MakeOpenFile(port, fptr);
    fptr->mode = io_mode_flags(mode);

    fptr->f = fopen(fname, mode);
    if (fptr->f == NULL) {
	if (errno == EMFILE) {
	    gc();
	    fptr->f = fopen(fname, mode);
	}
	if (fptr->f == NULL) {
	    rb_sys_fail(fname);
	}
    }

    fptr->path = strdup(fname);

    return port;
}

static int
apply2files0(func, args, arg, gl)
    void (*func)();
    struct RArray *args;
    void *arg;
    int gl;
{
    int i, n;
    VALUE path;

    for (i=n=0; i<args->len; i++) {
	path = args->ptr[i];
	if (TYPE(path) == T_STRING) {
	    if (gl) {
		char buf[MAXPATHLEN];
		char *p, *s;

		s = buf;
		p = RSTRING(path)->ptr;
		while (*p) {
		    switch (*s = *p++) {
		      case '*': case '?':
		      case '[': case '{':
			path = glob_new(path);
			goto glob;
		      case '\\':
			if (*p == '\0') break;
			*s = *p++;
		    }
		    s++;
		}
		*s = '\0';
		(*func)(buf, arg);
	    }
	    else {
		(*func)(RSTRING(path)->ptr, arg);
	    }
	    n++;
	}
	else {
	    extern VALUE C_Glob;

	    if (!obj_is_kind_of(path, C_Glob)) {
		WrongType(path, T_STRING);
	    }
	  glob:
	    n += apply2files0(func, rb_to_a(path), arg, 0);
	}
    }

    return n;
}

#define apply2files(func,args,arg) apply2files0(func,args,arg,1)

static VALUE
Ffile_tell(obj)
    VALUE obj;
{
    OpenFile *fptr;
    long pos;

    GetOpenFile(obj, fptr);

    pos = ftell(fptr->f);
    if (ferror(fptr->f) != 0) rb_sys_fail(Qnil);

    return int2inum(pos);
}

static VALUE
Ffile_seek(obj, offset, ptrname)
    VALUE obj, offset, ptrname;
{
    OpenFile *fptr;
    long pos;

    GetOpenFile(obj, fptr);

    pos = fseek(fptr->f, NUM2INT(offset), NUM2INT(ptrname));
    if (pos != 0) rb_sys_fail(Qnil);
    clearerr(fptr->f);

    return obj;
}

static VALUE
Ffile_set_pos(obj, offset)
    VALUE obj, offset;
{
    OpenFile *fptr;
    long pos;

    GetOpenFile(obj, fptr);
    pos = fseek(fptr->f, NUM2INT(offset), 0);
    if (pos != 0) rb_sys_fail(Qnil);
    clearerr(fptr->f);

    return obj;
}

static VALUE
Ffile_rewind(obj)
    VALUE obj;
{
    OpenFile *fptr;

    GetOpenFile(obj, fptr);
    if (fseek(fptr->f, 0L, 0) != 0) rb_sys_fail(Qnil);
    clearerr(fptr->f);

    return obj;
}

static VALUE
Ffile_eof(obj)
    VALUE obj;
{
    OpenFile *fptr;

    GetOpenFile(obj, fptr);
    if (feof(fptr->f) == 0) return FALSE;
    return TRUE;
}

static VALUE
Ffile_path(obj)
    VALUE obj;
{
    OpenFile *fptr;

    GetOpenFile(obj, fptr);
    return str_new2(fptr->path);
}

static VALUE
Ffile_isatty(obj)
    VALUE obj;
{
    return FALSE;
}

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>

static VALUE
stat_new(st)
    struct stat *st;
{
    if (st == Qnil) Bug("stat_new() called with nil");
    return struct_new("stat",
		      "dev", INT2FIX((int)st->st_dev),
		      "ino", INT2FIX((int)st->st_ino),
		      "mode", INT2FIX((int)st->st_mode),
		      "nlink", INT2FIX((int)st->st_nlink),
		      "uid", INT2FIX((int)st->st_uid),
		      "gid", INT2FIX((int)st->st_gid),
#ifdef HAVE_ST_RDEV
		      "rdev", INT2FIX((int)st->st_rdev),
#else
		      "rdev", INT2FIX(0),
#endif
		      "size", INT2FIX((int)st->st_size),
#ifdef HAVE_ST_BLKSIZE
		      "blksize", INT2FIX((int)st->st_blksize),
#else
		      "blksize", INT2FIX(0),
#endif
#ifdef HAVE_ST_BLOCKS
		      "blocks", INT2FIX((int)st->st_blocks), 
#else
		      "blocks", INT2FIX(0),
#endif
		      "atime", time_new(st->st_atime, 0),
		      "mtime", time_new(st->st_mtime, 0),
		      "ctime", time_new(st->st_ctime, 0),
		      Qnil);
}

static char lastpath[MAXPATHLEN];
static struct stat laststat;

cache_stat(path, st)
    char *path;
    struct stat *st;
{
    if (strcmp(lastpath, path) == 0) {
	*st = laststat;
	return 0;
    }
    if (stat(path, st) == -1)
	return -1;
    strcpy(lastpath, path);
    laststat = *st;

    return 0;
}

static VALUE
Sfile_stat(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) == -1) {
	rb_sys_fail(fname->ptr);
    }
    return stat_new(&st);
}

static VALUE
Ffile_stat(obj)
    VALUE obj;
{
    OpenFile *fptr;
    struct stat st;

    GetOpenFile(obj, fptr);
    if (fstat(fileno(fptr->f), &st) == -1) {
	rb_sys_fail(fptr->path);
    }
    return stat_new(&st);
}

static VALUE
Sfile_lstat(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    struct stat st;

    Check_Type(fname, T_STRING);
    if (lstat(fname->ptr, &st) == -1) {
	rb_sys_fail(fname->ptr);
    }
    return stat_new(&st);
}

static VALUE
Ffile_lstat(obj)
    VALUE obj;
{
    OpenFile *fptr;
    struct stat st;

    GetOpenFile(obj, fptr);
    if (lstat(fptr->path, &st) == -1) {
	rb_sys_fail(fptr->path);
    }
    return stat_new(&st);
}

#define HAS_GETGROUPS

static int
group_member(gid)
    GETGROUPS_T gid;
{
    GETGROUPS_T egid;

    if (getgid() ==  gid || getegid() == gid)
	return TRUE;

#ifdef HAS_GETGROUPS
#ifndef NGROUPS
#define NGROUPS 32
#endif
    {
	GETGROUPS_T gary[NGROUPS];
	int anum;

	anum = getgroups(NGROUPS, gary);
	while (--anum >= 0)
	    if (gary[anum] == gid)
		return TRUE;
    }
#endif
    return FALSE;
}

#ifndef S_IXUGO
#  define S_IXUGO		(S_IXUSR | S_IXGRP | S_IXOTH)
#endif

int
eaccess(path, mode)
     char *path;
     int mode;
{
  extern int group_member ();
  struct stat st;
  static int euid = -1;

  if (cache_stat(path, &st) < 0) return (-1);

  if (euid == -1)
    euid = geteuid ();

  if (euid == 0)
    {
      /* Root can read or write any file. */
      if (mode != X_OK)
	return 0;

      /* Root can execute any file that has any one of the execute
	 bits set. */
      if (st.st_mode & S_IXUGO)
	return 0;
    }

  if (st.st_uid == euid)        /* owner */
    mode <<= 6;
  else if (group_member (st.st_gid))
    mode <<= 3;

  if (st.st_mode & mode) return 0;

  return -1;
}

static VALUE
Ftest_d(obj, fname)
    VALUE obj;
    struct RString *fname;
{
#ifndef S_ISDIR
#   define S_ISDIR(m) ((m & S_IFMT) == S_IFDIR)
#endif

    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) return FALSE;
    if (S_ISDIR(st.st_mode)) return TRUE;
    return FALSE;
}

static VALUE
Ftest_p(obj, fname)
    VALUE obj;
    struct RString *fname;
{
#ifdef S_IFIFO
#  ifndef S_ISFIFO
#    define S_ISFIFO(m) ((m & S_IFMT) == S_IFIFO)
#  endif

    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) return FALSE;
    if (S_ISFIFO(st.st_mode)) return TRUE;

#endif
    return FALSE;
}

static VALUE
Ftest_l(obj, fname)
    VALUE obj;
    struct RString *fname;
{
#ifndef S_ISLNK
#  ifdef _S_ISLNK
#    define S_ISLNK(m) _S_ISLNK(m)
#  else
#    ifdef _S_IFLNK
#      define S_ISLNK(m) ((m & S_IFMT) == _S_IFLNK)
#    else
#      ifdef S_IFLNK
#	 define S_ISLNK(m) ((m & S_IFMT) == S_IFLNK)
#      endif
#    endif
#  endif
#endif

#ifdef S_ISLNK
    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) return FALSE;
    if (S_ISLNK(st.st_mode)) return TRUE;

#endif
    return FALSE;
}

Ftest_S(obj, fname)
    VALUE obj;
    struct RString *fname;
{
#ifndef S_ISSOCK
#  ifdef _S_ISSOCK
#    define S_ISSOCK(m) _S_ISSOCK(m)
#  else
#    ifdef _S_IFSOCK
#      define S_ISSOCK(m) ((m & S_IFMT) == _S_IFSOCK)
#    else
#      ifdef S_IFSOCK
#	 define S_ISSOCK(m) ((m & S_IFMT) == S_IFSOCK)
#      endif
#    endif
#  endif
#endif

#ifdef S_ISSOCK
    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) return FALSE;
    if (S_ISSOCK(st.st_mode)) return TRUE;

#endif
    return FALSE;
}

static VALUE
Ftest_b(obj, fname)
    VALUE obj;
    struct RString *fname;
{
#ifndef S_ISBLK
#   ifdef S_IFBLK
#	define S_ISBLK(m) ((m & S_IFMT) == S_IFBLK)
#   endif
#endif

#ifdef S_ISBLK
    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) return FALSE;
    if (S_ISBLK(st.st_mode)) return TRUE;

#endif
    return FALSE;
}

static VALUE
Ftest_c(obj, fname)
    VALUE obj;
    struct RString *fname;
{
#ifndef S_ISCHR
#   define S_ISCHR(m) ((m & S_IFMT) == S_IFCHR)
#endif

    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) return FALSE;
    if (S_ISBLK(st.st_mode)) return TRUE;

    return FALSE;
}

static VALUE
Ftest_e(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) return FALSE;
    return TRUE;
}

static VALUE
Ftest_r(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    Check_Type(fname, T_STRING);
    if (eaccess(fname->ptr, R_OK) < 0) return FALSE;
    return TRUE;
}

static VALUE
Ftest_R(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    Check_Type(fname, T_STRING);
    if (access(fname->ptr, R_OK) < 0) return FALSE;
    return TRUE;
}

static VALUE
Ftest_w(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    Check_Type(fname, T_STRING);
    if (eaccess(fname->ptr, W_OK) < 0) return FALSE;
    return TRUE;
}

static VALUE
Ftest_W(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    Check_Type(fname, T_STRING);
    if (access(fname->ptr, W_OK) < 0) return FALSE;
    return TRUE;
}

static VALUE
Ftest_x(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    Check_Type(fname, T_STRING);
    if (eaccess(fname->ptr, X_OK) < 0) return FALSE;
    return TRUE;
}

static VALUE
Ftest_X(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    Check_Type(fname, T_STRING);
    if (access(fname->ptr, X_OK) < 0) return FALSE;
    return TRUE;
}

static VALUE
Ftest_f(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) return FALSE;
    if (S_ISREG(st.st_mode)) return TRUE;
    return FALSE;
}

static VALUE
Ftest_z(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) return FALSE;
    if (st.st_size == 0) return TRUE;
    return FALSE;
}

static VALUE
Ftest_s(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) return FALSE;
    if (st.st_size == 0) return FALSE;
    return int2inum(st.st_size);
}

static VALUE
Ftest_owned(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) return FALSE;
    if (st.st_uid == geteuid()) return TRUE;
    return FALSE;
}

static VALUE
Ftest_grpowned(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) return FALSE;
    if (st.st_gid == getegid()) return TRUE;
    return FALSE;
}

#if defined(S_ISUID) || defined(S_ISGID) || defined(S_ISVTX)
static VALUE
check3rdbyte(file, mode)
    char *file;
    int mode;
{
    struct stat st;

    if (cache_stat(file, &st) < 0) return FALSE;
    if (st.st_mode & mode) return TRUE;
    return FALSE;
}
#endif

static VALUE
Ftest_suid(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    Check_Type(fname, T_STRING);
#ifdef S_ISUID
    return check3rdbyte(fname->ptr, S_ISUID);
#else
    return FALSE;
#endif
}

static VALUE
Ftest_sgid(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    Check_Type(fname, T_STRING);
#ifdef S_ISGID
    return check3rdbyte(fname->ptr, S_ISGID);
#else
    return FALSE;
#endif
}

static VALUE
Ftest_sticky(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    Check_Type(fname, T_STRING);
#ifdef S_ISVTX
    return check3rdbyte(fname->ptr, S_ISVTX);
#else
    return FALSE;
#endif
}

static VALUE
Sfile_type(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    struct stat st;
    char *t;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) rb_sys_fail(fname->ptr);

    if (S_ISREG(st.st_mode)) {
	t = "file";
    } else if (S_ISDIR(st.st_mode)) {
	t = "directory";
    } else if (S_ISCHR(st.st_mode)) {
	t = "characterSpecial";
    } 
#ifdef S_ISBLK
    else if (S_ISBLK(st.st_mode)) {
	t = "blockSpecial";
    }
#endif
#ifndef S_ISFIFO
    else if (S_ISFIFO(st.st_mode)) {
	t = "fifo";
    } 
#endif
#ifdef S_ISLNK
    else if (S_ISLNK(st.st_mode)) {
	t = "link";
    }
#endif
#ifdef S_ISSOCK
    else if (S_ISSOCK(st.st_mode)) {
	t = "socket";
    }
#endif
    else {
	t = "unknown";
    }

    return str_new2(t);
}

static VALUE
Sfile_atime(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) rb_sys_fail(fname->ptr);
    return time_new(st.st_atime, 0);
}

static VALUE
Ffile_atime(obj)
    VALUE obj;
{
    OpenFile *fptr;
    struct stat st;

    GetOpenFile(obj, fptr);
    if (fstat(fileno(fptr->f), &st) == -1) {
	rb_sys_fail(fptr->path);
    }
    return time_new(st.st_atime, 0);
}

static VALUE
Sfile_mtime(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) rb_sys_fail(fname->ptr);
    return time_new(st.st_mtime, 0);
}

static VALUE
Ffile_mtime(obj)
    VALUE obj;
{
    OpenFile *fptr;
    struct stat st;

    GetOpenFile(obj, fptr);
    if (fstat(fileno(fptr->f), &st) == -1) {
	rb_sys_fail(fptr->path);
    }
    return time_new(st.st_mtime, 0);
}

static VALUE
Sfile_ctime(obj, fname)
    VALUE obj;
    struct RString *fname;
{
    struct stat st;

    Check_Type(fname, T_STRING);
    if (cache_stat(fname->ptr, &st) < 0) rb_sys_fail(fname->ptr);
    return time_new(st.st_ctime, 0);
}

static VALUE
Ffile_ctime(obj)
    VALUE obj;
{
    OpenFile *fptr;
    struct stat st;

    GetOpenFile(obj, fptr);
    if (fstat(fileno(fptr->f), &st) == -1) {
	rb_sys_fail(fptr->path);
    }
    return time_new(st.st_ctime, 0);
}

static void
chmod_internal(path, mode)
    char *path;
    int mode;
{
    if (chmod(path, mode) == -1)
	rb_sys_fail(path);
}

static VALUE
Sfile_chmod(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    VALUE vmode;
    VALUE rest;
    int mode, n;
    VALUE path;

    rb_scan_args(argc, argv, "1*", &vmode, &rest);
    mode = NUM2INT(vmode);

    n = apply2files(chmod_internal, rest, mode);
    return INT2FIX(n);
}

static VALUE
Ffile_chmod(obj, vmode)
    VALUE obj, vmode;
{
    OpenFile *fptr;
    int mode;

    mode = NUM2INT(vmode);

    GetOpenFile(obj, fptr);
    if (fchmod(fileno(fptr->f), mode) == -1)
	rb_sys_fail(fptr->path);

    return INT2FIX(0);
}

struct chown_args {
    int owner, group;
};

static void
chown_internal(path, args)
    char *path;
    struct chown_args *args;
{
    if (chown(path, args->owner, args->group) < 0)
	rb_sys_fail(path);
}

static VALUE
Sfile_chown(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    VALUE o, g, rest;
    struct chown_args arg;
    int n;

    rb_scan_args(argc, argv, "2*", &o, &g, &rest);
    if (o == Qnil) {
	arg.owner = -1;
    }
    else {
	arg.owner = NUM2INT(o);
    }
    if (g == Qnil) {
	arg.group = -1;
    }
    else {
	arg.group = NUM2INT(g);
    }

    n = apply2files(chown_internal, rest, &arg);
    return INT2FIX(n);
}

Ffile_chown(obj, owner, group)
    VALUE obj, owner, group;
{
    OpenFile *fptr;
    int mode;

    GetOpenFile(obj, fptr);
    if (fchown(fileno(fptr->f), NUM2INT(owner), NUM2INT(group)) == -1)
	rb_sys_fail(fptr->path);

    return INT2FIX(0);
}

struct timeval *time_timeval();

static void
utime_internal(path, tvp)
    char *path;
    struct timeval tvp[];
{
    if (utimes(path, tvp) < 0)
	rb_sys_fail(path);
}

static VALUE
Sfile_utime(argc, argv, obj)
    int argc;
    VALUE *argv;
    VALUE obj;
{
    VALUE atime, mtime, rest;
    struct timeval tvp[2];
    int n;

    rb_scan_args(argc, argv, "2*", &atime, &mtime, &rest);

    tvp[0] = *time_timeval(atime);
    tvp[1] = *time_timeval(mtime);

    n = apply2files(utime_internal, rest, tvp);
    return INT2FIX(n);
}

static VALUE
Sfile_link(obj, from, to)
    VALUE obj;
    struct RString *from, *to;
{
    Check_Type(from, T_STRING);
    Check_Type(to, T_STRING);

    if (link(from->ptr, to->ptr) < 0)
	rb_sys_fail(from->ptr);
    return INT2FIX(0);
}

static VALUE
Sfile_symlink(obj, from, to)
    VALUE obj;
    struct RString *from, *to;
{
    Check_Type(from, T_STRING);
    Check_Type(to, T_STRING);

    if (symlink(from->ptr, to->ptr) < 0)
	rb_sys_fail(from->ptr);
    return TRUE;
}

static VALUE
Sfile_readlink(obj, path)
    VALUE obj;
    struct RString *path;
{
    char buf[MAXPATHLEN];
    int cc;

    Check_Type(path, T_STRING);

    if ((cc = readlink(path->ptr, buf, MAXPATHLEN)) < 0)
	rb_sys_fail(path->ptr);

    return str_new(buf, cc);
}

static void
unlink_internal(path)
    char *path;
{
    if (unlink(path) < 0)
	rb_sys_fail(path);
}

static VALUE
Sfile_unlink(obj, args)
    VALUE obj;
    struct RArray *args;
{
    int n;

    n = apply2files(unlink_internal, args, Qnil);
    return INT2FIX(n);
}

static VALUE
Sfile_rename(obj, from, to)
    VALUE obj;
    struct RString *from, *to;
{
    Check_Type(from, T_STRING);
    Check_Type(to, T_STRING);

    if (rename(from->ptr, to->ptr) == -1)
	rb_sys_fail(from->ptr);

    return INT2FIX(0);
}

static VALUE
Sfile_umask(argc, argv)
    int argc;
    VALUE *argv;
{
    VALUE mask;
    int omask;

    if (argc == 0) {
	int omask = umask(0);
	umask(omask);
    }
    else if (argc == 1) {
	omask = umask(NUM2INT(argv[1]));
    }
    else {
	Fail("wrong # of argument");
    }
    return INT2FIX(omask);
}

static VALUE
Sfile_truncate(obj, path, len)
    VALUE obj, len;
    struct RString *path;
{
    Check_Type(path, T_STRING);

    if (truncate(path->ptr, NUM2INT(len)) < 0)
	rb_sys_fail(path->ptr);
    return TRUE;
}

static VALUE
Ffile_truncate(obj, len)
    VALUE obj, len;
{
    OpenFile *fptr;

    GetOpenFile(obj, fptr);

    if (!(fptr->mode & FMODE_WRITABLE)) {
	Fail("not opened for writing");
    }
    if (ftruncate(fileno(fptr->f), NUM2INT(len)) < 0)
	rb_sys_fail(fptr->path);
    return TRUE;
}

static VALUE
Ffile_fcntl(obj, req, arg)
    VALUE obj, req;
    struct RString *arg;
{
    io_ctl(obj, req, arg, 0);
    return obj;
}

Init_File()
{
    M_FileTest = rb_define_module("FileTest");

    rb_define_method(M_FileTest, "d",  Ftest_d, 1);
    rb_define_method(M_FileTest, "isdirectory",  Ftest_d, 1);
    rb_define_method(M_FileTest, "a",  Ftest_e, 1);
    rb_define_method(M_FileTest, "e",  Ftest_e, 1);
    rb_define_method(M_FileTest, "exists",  Ftest_e, 1);
    rb_define_method(M_FileTest, "r",  Ftest_r, 1);
    rb_define_method(M_FileTest, "readable",  Ftest_r, 1);
    rb_define_method(M_FileTest, "R",  Ftest_R, 1);
    rb_define_method(M_FileTest, "w",  Ftest_w, 1);
    rb_define_method(M_FileTest, "writable",  Ftest_w, 1);
    rb_define_method(M_FileTest, "W",  Ftest_W, 1);
    rb_define_method(M_FileTest, "x",  Ftest_x, 1);
    rb_define_method(M_FileTest, "executable",  Ftest_x, 1);
    rb_define_method(M_FileTest, "X",  Ftest_X, 1);
    rb_define_method(M_FileTest, "f",  Ftest_f, 1);
    rb_define_method(M_FileTest, "isfile",  Ftest_f, 1);
    rb_define_method(M_FileTest, "z",  Ftest_z, 1);
    rb_define_method(M_FileTest, "s",  Ftest_s, 1);
    rb_define_method(M_FileTest, "size",  Ftest_s, 1);
    rb_define_method(M_FileTest, "O",  Ftest_owned, 1);
    rb_define_method(M_FileTest, "owned",  Ftest_owned, 1);
    rb_define_method(M_FileTest, "G",  Ftest_grpowned, 1);

    rb_define_method(M_FileTest, "p",  Ftest_p, 1);
    rb_define_method(M_FileTest, "ispipe",  Ftest_p, 1);
    rb_define_method(M_FileTest, "l",  Ftest_l, 1);
    rb_define_method(M_FileTest, "issymlink",  Ftest_l, 1);
    rb_define_method(M_FileTest, "S",  Ftest_S, 1);
    rb_define_method(M_FileTest, "issocket",  Ftest_S, 1);

    rb_define_method(M_FileTest, "b",  Ftest_b, 1);
    rb_define_method(M_FileTest, "c",  Ftest_c, 1);

    rb_define_method(M_FileTest, "u",  Ftest_suid, 1);
    rb_define_method(M_FileTest, "setuid",  Ftest_suid, 1);
    rb_define_method(M_FileTest, "g",  Ftest_sgid, 1);
    rb_define_method(M_FileTest, "setgid",  Ftest_sgid, 1);
    rb_define_method(M_FileTest, "k",  Ftest_sticky, 1);

    C_File = rb_define_class("File", C_IO);

    rb_define_single_method(C_File, "stat",  Sfile_stat, 1);
    rb_define_single_method(C_File, "lstat", Sfile_lstat, 1);
    rb_define_single_method(C_File, "type",  Sfile_type, 1);

    rb_define_single_method(C_File, "atime", Sfile_atime, 1);
    rb_define_single_method(C_File, "mtime", Sfile_mtime, 1);
    rb_define_single_method(C_File, "ctime", Sfile_ctime, 1);

    rb_define_single_method(C_File, "utime", Sfile_utime, -1);
    rb_define_single_method(C_File, "chmod", Sfile_chmod, -1);
    rb_define_single_method(C_File, "chown", Sfile_chown, -1);

    rb_define_single_method(C_File, "link", Sfile_link, 2);
    rb_define_single_method(C_File, "symlink", Sfile_symlink, 2);
    rb_define_single_method(C_File, "readlink", Sfile_readlink, 1);

    rb_define_single_method(C_File, "unlink", Sfile_unlink, -2);
    rb_define_single_method(C_File, "delete", Sfile_unlink, -2);
    rb_define_single_method(C_File, "rename", Sfile_rename, 2);
    rb_define_single_method(C_File, "umask", Sfile_umask, -1);
    rb_define_single_method(C_File, "truncate", Sfile_truncate, 2);

    rb_include_module(CLASS_OF(C_File), M_FileTest);

    rb_define_method(C_File, "stat",  Ffile_stat, 0);
    rb_define_method(C_File, "lstat",  Ffile_lstat, 0);

    rb_define_method(C_File, "atime", Ffile_atime, 0);
    rb_define_method(C_File, "mtime", Ffile_mtime, 0);
    rb_define_method(C_File, "ctime", Ffile_ctime, 0);

    rb_define_method(C_File, "chmod", Ffile_chmod, 1);
    rb_define_method(C_File, "chown", Ffile_chown, 2);
    rb_define_method(C_File, "truncate", Ffile_truncate, 1);

    rb_define_method(C_File, "tell",  Ffile_tell, 0);
    rb_define_method(C_File, "seek",  Ffile_seek, 2);


    rb_define_method(C_File, "pos",  Ffile_tell, 0);
    rb_define_method(C_File, "pos=", Ffile_set_pos, 1);

    rb_define_method(C_File, "rewind",  Ffile_rewind, 0);
    rb_define_method(C_File, "isatty",  Ffile_isatty, 0);
    rb_define_method(C_File, "eof",  Ffile_eof, 0);

    rb_define_method(C_IO, "fcntl", Ffile_fcntl, 2);

    rb_define_method(C_File, "path",  Ffile_path, 0);
}
