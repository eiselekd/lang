/* utf2.c
 * 14apr02abu
 * Convert process or file (ISO-8859-Latin15) to stdout (UTF-8, 2-Byte)
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>

// utf2 [-<cmd> [<arg> ..]]
// utf2 [<Infile/ISO-8859-Latin15>]
int main(int ac, char *av[]) {
	int c;
   pid_t pid = 0;
   FILE *fp = stdin;

   if (ac > 1) {
      if (*av[1] == '-') {
         int pfd[2];

         if (pipe(pfd) < 0) {
            fprintf(stderr, "utf2: Pipe error\n");
            return 1;
         }
         if ((pid = fork()) == 0) {
            close(pfd[0]);
            if (pfd[1] != STDOUT_FILENO)
               dup2(pfd[1], STDOUT_FILENO),  close(pfd[1]);
            execvp(av[1]+1, av+1);
         }
         if (pid < 0) {
            fprintf(stderr, "utf2: Fork error\n");
            return 1;
         }
         close(pfd[1]);
         if (!(fp = fdopen(pfd[0], "r"))) {
            fprintf(stderr, "utf2: Pipe open error\n");
            return 1;
         }
      }
      else if (!(fp = fopen(av[1], "r"))) {
         fprintf(stderr, "utf2: '%s' open error\n", av[1]);
         return 1;
      }
   }
	while ((c = getc(fp)) != EOF) {
      if (c == 0xA4)
         putchar(0xE2), putchar(0x82), putchar(0xAC);
      else if (c >= 0x80) {
         putchar(0xC0 | c>>6 & 0x1F);
         putchar(0x80 | c & 0x3F);
      }
      else
         putchar(c);
	}
   if (pid) {
      fclose(fp);
      while (waitpid(pid, NULL, 0) < 0)
         if (errno != EINTR) {
            fprintf(stderr, "utf2: Pipe close error\n");
            return 1;
         }
   }
   return 0;
}
