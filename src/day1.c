#include <stdio.h>
#include <string.h>
#include <stdlib.h>

# define MAX_LINE_LENGTH 100

struct ListNode {
    int value;
    struct ListNode* prev;
    struct ListNode* next;
};

struct ListNode* cons(int value, struct ListNode* head) {
    struct ListNode* newNode = (struct ListNode*) malloc(sizeof(struct ListNode));
    newNode->value = value;
    newNode->prev = NULL;
    newNode->next = head;
    return newNode;
}

void part1(struct ListNode* numbers)
{
    for(struct ListNode* l = numbers; l; l = l->next)
    {
        for(struct ListNode* ll = numbers; ll; ll = ll->next)
        {
            if (l->value + ll->value == 2020) {
                printf("The numbers that add to 2020 are: %d and %d \n", l->value, ll->value);
                printf("Which multiplied are equal to %d", l->value * ll->value);
                return;
            }
        }
    }
}

void part2(struct ListNode* numbers)
{
    for(struct ListNode* l = numbers; l; l = l->next)
    {
        for(struct ListNode* ll = numbers; ll; ll = ll->next)
        {
            for(struct ListNode* lll = numbers; lll; lll = lll->next)
            {
                if (l->value + ll->value + lll->value == 2020) {
                    printf("The three numbers that add to 2020 are: %d, %d, and %d \n", l->value, ll->value, lll->value);
                    printf("Which multiplied are equal to %d", l->value * ll->value * lll->value);
                    return;
                }
            }
        }
    }
}

int main () {
    char line[MAX_LINE_LENGTH];

    FILE* file = fopen("puzzle-inputs/1.txt", "r");

    struct ListNode* numbers = NULL;

    int value;
    while(fgets(line, MAX_LINE_LENGTH, file))
    {
        value = atoi(line);
        numbers = cons(value, numbers);
    }

    part1(numbers);
    part2(numbers);

    fclose(file);
}

