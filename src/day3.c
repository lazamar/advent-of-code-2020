#include <stdio.h>
#include <string.h>

#define MAX_LINE_LENGTH     1000

struct Strategy {
    int right, down;
};

long int treeCost(struct Strategy* s, int n, char* line) {

    if (n % (*s).down != 0) return 0;
    int len = strlen(line)- 1;
    int downSteps = n / (*s).down;
    int index = (downSteps * (*s).right) % len;
    if (line[index] == '.') return 0;
    return 1;
}

long int eachLine (struct Strategy* s)
{
    FILE* file = fopen("puzzle-inputs/3.txt", "r");
    char line[MAX_LINE_LENGTH];

    long int total = 0;
    int index = 0;
    while(fgets(line, FILENAME_MAX, file))
    {
        total += treeCost(s, index, line);
        index++;
    }
    return total;
}

int main () {
    struct Strategy strategies[] =
        { { 3, 1 }
        , { 1, 1 }
        , { 5, 1 }
        , { 7, 1 }
        , { 1, 2 }
        };

    long int total = eachLine(&strategies[0]);
    printf("Total trees found in path %ld\n", total);

    long int all = 1;
    for (int n = 5; n--;) {
        printf("For strategy %d: %ld\n", n, eachLine(&strategies[n]));
        all *= eachLine(&strategies[n]);
    }
    printf("Total trees found multiplied %ld\n", all);

}
