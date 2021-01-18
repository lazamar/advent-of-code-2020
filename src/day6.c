#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LEN 2000
#define MAX_ANSWERS  26

bool isBlankLine(char * line) {
    return strlen(line) == 1;
}

int anyYesAnswerCount(int answers[]) {
    int sum = 0;
    for (int i = MAX_ANSWERS; i--;) sum += answers[i] ? 1 : 0;
    return sum;
}

int allYesAnswerCount(int answers[], int respondents) {
    int sum = 0;
    for (int i = MAX_ANSWERS; i--;) sum += answers[i] == respondents ? 1 : 0;
    return sum;
}

void resetAnswers(int answers[]) {
    for (int i = MAX_ANSWERS; i--;) answers[i] = 0;
}

// Marks all answers from line as true in answers
void addAnswers(char * line, int answers[]) {
    for (char * pos = line; isalpha(*pos); pos++) {
        answers[*pos - 'a'] += 1;
    }
}

int main () {
    FILE * file = fopen("puzzle-inputs/6.txt", "r");
    char line[MAX_LINE_LEN];

    int answers[MAX_ANSWERS] = {};
    resetAnswers(answers);

    int anyTotal = 0;
    int allTotal = 0;
    int respondents = 0;
    while (fgets(line, MAX_LINE_LEN, file)) {
        if (isBlankLine(line)) {
            anyTotal += anyYesAnswerCount(answers);
            allTotal += allYesAnswerCount(answers, respondents);
            respondents = 0;
            resetAnswers(answers);
        } else {
            respondents += 1;
            addAnswers(line, answers);
        }
    }
    anyTotal += anyYesAnswerCount(answers);
    allTotal += allYesAnswerCount(answers, respondents);

    printf("The total of questions to which anyone answered yes is %d\n", anyTotal);
    printf("The total of questions to which everyone answered yes is %d\n", allTotal);
    fclose(file);
}
