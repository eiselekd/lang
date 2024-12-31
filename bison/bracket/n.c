#include <stdlib.h>
#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "parse.h"

#define fail(fmt, ...) do { printf(fmt, ##__VA_ARGS__); exit(1); } while(0)

static void
readin(char *fname)
{
  struct stat st;
  int fd;
  char *ptr, *p, *pend;

  fd = open(fname, O_RDONLY, 0);
  if (fd < 0) fail("fail open %s", fname);

  if (fstat(fd, &st) < 0) fail("fail sta %s", fname);
  if (!S_ISREG(st.st_mode)) {
    fail("fail %s is not a regular file", fname);
  }
  p = ptr = (char *)malloc(st.st_size+1);
  if (read(fd, ptr, st.st_size) != st.st_size) {

  }
  ptr[st.st_size] = 0;

  extern void * yy_scan_string (const char * yystr );
  yy_scan_string( ptr );

  yydebug=1;
  yyparse();

}

/* 8< --- main --- */

int main(int argc, char **argv)
{
  if (argc >= 1) {
    readin(argv[1]);
  }
  return 0;
}
