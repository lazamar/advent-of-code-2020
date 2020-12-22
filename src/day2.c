#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAX_LINE_LENGTH     1000
#define MAX_LINES           (1<<10)
#define MAX_PATTERN_LENGTH  100

char line[MAX_LINE_LENGTH];
char* lines[MAX_LINES];

enum Boolean { False, True };

int onEachLine(enum Boolean isValid(int, int, char, char *)) {
    FILE* file = fopen("puzzle-inputs/2.txt", "r");

    int res = 0;

    while(fgets(line, MAX_LINE_LENGTH, file))
    {
        int min, max;
        char c, pattern[MAX_PATTERN_LENGTH];

        sscanf(line, "%d-%d %c: %s", &min, &max, &c, pattern);

        res += isValid(min, max, c, pattern);
    }

    return res;
}

enum Boolean isValidOne(int min, int max, char c, char * pattern) {
    int count = 0;
    for(; *pattern; pattern++) if (*pattern == c) count++;

    if (min <= count && count <= max) {
        return True;
    }
    return False;
}

enum Boolean isValidTwo(int min, int max, char c, char * pattern) {
    return 1 == (pattern[min - 1] == c) + (pattern[max - 1] == c);
}

int main ()
{
    printf("Number of valid passwords: %d\n", onEachLine(isValidOne));
    printf("Number of valid passwords: %d\n", onEachLine(isValidTwo));
}
