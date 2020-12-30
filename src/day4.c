#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LEN 1024

typedef struct {
    unsigned int byr : 1;
    unsigned int iyr : 1;
    unsigned int eyr : 1;
    unsigned int hgt : 1;
    unsigned int hcl : 1;
    unsigned int ecl : 1;
    unsigned int pid : 1;
    unsigned int cid : 1;
} ValidCredentials;

ValidCredentials emptyCredentials = { 0,0,0,0,0,0,0,0 };

ValidCredentials merge (const ValidCredentials a, const ValidCredentials b) {
    ValidCredentials result = {
        a.byr | b.byr,
        a.iyr | b.iyr,
        a.eyr | b.eyr,
        a.hgt | b.hgt,
        a.hcl | b.hcl,
        a.ecl | b.ecl,
        a.pid | b.pid,
        a.cid | b.cid
    };
    return result;
}

bool isValid(const ValidCredentials a) {
    return a.byr
        && a.iyr
        && a.eyr
        && a.hgt
        && a.hcl
        && a.ecl
        && a.pid;
        // we don't are about cid
}

// Checks only if the field is present.
ValidCredentials noValidationParse (char* line) {
    ValidCredentials fields = emptyCredentials;
    while(*line) {
        char * field = strndup(line, 3);
        if      (!strcmp(field, "byr")) { fields.byr = 1; }
        else if (!strcmp(field, "iyr")) { fields.iyr = 1; }
        else if (!strcmp(field, "eyr")) { fields.eyr = 1; }
        else if (!strcmp(field, "hgt")) { fields.hgt = 1; }
        else if (!strcmp(field, "hcl")) { fields.hcl = 1; }
        else if (!strcmp(field, "ecl")) { fields.ecl = 1; }
        else if (!strcmp(field, "pid")) { fields.pid = 1; }
        else if (!strcmp(field, "cid")) { fields.cid = 1; }
        while (*line && *line++ != ' ');
    }
    return fields;
}

ValidCredentials validatedParse (char* line) {
    ValidCredentials fields = {};
    while(*line) {
        char * field = strndup(line, 3);
        char * content = line + 4;
        int success = 0;
        if (!strcmp(field, "byr")) {
            unsigned int byr;
            if (sscanf(content, "%4d%n", &byr, &success) && success && 1920 <= byr && byr <= 2002) {
                fields.byr = 1;
            }
        }
        else if (!strcmp(field, "iyr")) {
            unsigned int iyr;
            if (sscanf(content, "%4d%n", &iyr, &success) && success && 2010 <= iyr && iyr <= 2020) {
                fields.iyr = 1;
            }
        }
        else if (!strcmp(field, "eyr")) {
            unsigned int eyr;
            if (sscanf(content, "%4d%n", &eyr, &success) && success && 2020 <= eyr && eyr <= 2030) {
                fields.eyr = 1;
            }
        }
        else if (!strcmp(field, "hgt")) {
            unsigned int hgt;
            if ((sscanf(content, "%dcm%n", &hgt, &success) && success && 150 <= hgt && hgt <= 193)
                ||
                (sscanf(content, "%din%n", &hgt, (success = 0, &success)) && success && 59 <= hgt && hgt <= 76)
            ) {
                fields.hgt = 1;
            }
        }
        else if (!strcmp(field, "hcl")) {
            char hcl[7];
            if (sscanf(content, "#%6s%n", hcl, &success) && success && strspn(hcl, "0123456789abcdef") == 6) {
                fields.hcl = 1;
            }
        }
        else if (!strcmp(field, "ecl")) {
            char ecl[4];
            if (sscanf(content, "%3s%n", ecl, &success) && success &&
                (  0 == strcmp(ecl, "amb")
                || 0 == strcmp(ecl, "blu")
                || 0 == strcmp(ecl, "brn")
                || 0 == strcmp(ecl, "gry")
                || 0 == strcmp(ecl, "grn")
                || 0 == strcmp(ecl, "hzl")
                || 0 == strcmp(ecl, "oth")
                )
            ) {
                fields.ecl = 1;
            }
        }
        else if (!strcmp(field, "pid")) {
            if (strspn(content, "0123456789") == 9) {
                fields.pid = 1;
            }
        }
        while (*line && *line++ != ' ');
    }
    return fields;
}

bool isEmptyLine(char *line) {
    if (strcmp(line, "\n") == 0) return true;
    return false;
};

void runWithParser(ValidCredentials parse(char *)) {
    FILE* file = fopen("puzzle-inputs/4.txt", "r");
    char line[MAX_LINE_LEN];

    int validCount = 0;

    ValidCredentials currentCreds = emptyCredentials;

    while (fgets(line, MAX_LINE_LEN, file)) {
        currentCreds = merge(currentCreds, parse(line));
        if (isEmptyLine(line)) {
            validCount += isValid(currentCreds);
            currentCreds = emptyCredentials;
        }
    }
    validCount += isValid(currentCreds);

    fclose(file);
    printf("Valid passports one: %d\n", validCount);
}

int main() {
    printf("Question one\n");
    runWithParser(noValidationParse);

    printf("Question two\n");
    runWithParser(validatedParse);
    return 0;
}

