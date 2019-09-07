#include <fcntl.h>
#include <cstdio>
#include <errno.h>
#include <pty.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <iostream>
#include <fstream>
#include <pty.h>

int
ttySetRaw(int fd, struct termios *prevTermios)
{
    struct termios t;

    if (tcgetattr(fd, &t) == -1)
        return -1;

    if (prevTermios != NULL)
        *prevTermios = t;

    t.c_lflag &= ~(ICANON | ISIG | IEXTEN | ECHO);
                        /* Noncanonical mode, disable signals, extended
                           input processing, and echoing */

    t.c_iflag &= ~(BRKINT | ICRNL | IGNBRK | IGNCR | INLCR |
                      INPCK | ISTRIP | IXON | PARMRK);
                        /* Disable special handling of CR, NL, and BREAK.
                           No 8th-bit stripping or parity error handling.
                           Disable START/STOP output flow control. */

    t.c_oflag &= ~OPOST;                /* Disable all output processing */

    t.c_cc[VMIN] = 1;                   /* Character-at-a-time input */
    t.c_cc[VTIME] = 0;                  /* with blocking */

    if (tcsetattr(fd, TCSAFLUSH, &t) == -1)
        return -1;

    return 0;
}

float func(float i) {
    int j;
    for (j = 0; j < 200; j++) {
	i = cos(i);
    }
    return i;
}


int main(int, char const *[])
{
    int master, slave;
    char name[256];

    auto e = openpty(&master, &slave, &name[0], nullptr, nullptr);
    if(0 > e) {
	std::printf("Error: %s\n", strerror(errno));
	return -1;
    }

    std::ofstream c;

    c.open ("/tmp/pty.txt");
    c << name;
    c.close();

    ttySetRaw(master, 0);

    float v = 0.3;
    while (1) {
	char b[256];
	v = func(v);
	sprintf(b, "%08f\n", v);
	if (write(master, b, strlen(b)) <= 0)
	    break;
	printf("%s",b);
    }

    close(slave);
    close(master);
    return 0;
}
