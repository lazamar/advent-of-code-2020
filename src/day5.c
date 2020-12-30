#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

#define MAX_LINE_LEN 12
#define MAX_IDS      2000

typedef struct {
    unsigned int id;
    char code[15];
} SeatId ;

SeatId seatId (char * input) {
    unsigned int row = 0;
    unsigned int col = 0;
    SeatId res = {};
    strncpy(res.code, input, 14);
    for (char c = *input; *input; c = *++input) {
        switch(c) {
            case 'B': row = (row << 1) | 1; break;
            case 'F': row = (row << 1)    ; break;
            case 'R': col = (col << 1) | 1; break;
            case 'L': col = (col << 1)    ; break;
            default: break;
        }
    }
    res.id = row * 8 + col;
    return res;
}

int main () {
    FILE * file = fopen("puzzle-inputs/5.txt", "r");
    char line[MAX_LINE_LEN];
    SeatId maxSeatId = { 0 };
    SeatId current;
    bool ids[MAX_IDS] = {};
    for (int i = MAX_IDS; i--;) ids[i] = false;


    while (fgets(line, MAX_LINE_LEN, file)) {
        current = seatId(line);
        ids[current.id] = true;
        maxSeatId = current.id > maxSeatId.id ? current : maxSeatId;
    }
    printf("Highest seat id: %d \t %s \n", maxSeatId.id, maxSeatId.code);

    unsigned int mySeat = 0;
    for (unsigned int i = MAX_IDS; i--;) {
        if (!ids[i] && ids[i + 1] && ids[i - 1]) {
            mySeat = i;
            break;
        }
    }

    printf("My seat number is: %d\n", mySeat);
    return 0;
}
