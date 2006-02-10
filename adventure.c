#include <stdio.h>

struct symbols {
        char *dummy;
        char *here;
        char *lamp;
        char *direction;
        char *another;
} symbol;

char *test;

int main(void)
{
        test = 0x400;
        test = 0xABCD1234;
}
