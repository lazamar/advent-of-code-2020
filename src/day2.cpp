#include <iostream>
#include <fstream>
#include "maybe.cpp"
#include <regex>
#include <set>

using namespace std;

typedef struct {
    int low, high;
    char letter;
    string pass;
} Policy;

Maybe<Policy> to_policy(string * s) {
    regex policy {R"(^(\d+)-(\d+) (\w): (.+)$)"};
    smatch matches;
    if (!regex_match(*s, matches, policy)) return Nothing<Policy>();
    int low = stoi(matches[1]);
    int high = stoi(matches[2]);
    char letter = matches[3].str().at(0);
    string pass = matches[4];
    Policy p = {low, high, letter, pass };
    return Just<Policy>(p);
}

bool is_valid_1(Policy p) {
    int n = count(p.pass.begin(), p.pass.end(), p.letter);
    return p.low <= n && n <= p.high;
}

bool is_valid_2(Policy p) {
    bool fst = p.letter == p.pass.at(p.low - 1);
    bool snd = p.letter == p.pass.at(p.high - 1);
    return (fst || snd) && !(fst && snd);
}

int valid_count(istream * s, bool test(Policy)) {
    string line;
    int total = 0;
    while (getline(*s, line)) {
        Maybe<Policy> mp = to_policy(&line);
        bool valid = mp.run<bool>(test, [](){ return false; });
        if (valid) total+= 1;
    }
    return total;
}

int main() {
    ifstream file_1 ("./puzzle-inputs/2.txt");
    int c_1 = valid_count(&file_1, is_valid_1);
    printf("Passes for password criterion 1: %d\n", c_1);

    ifstream file_2 ("./puzzle-inputs/2.txt");
    int c_2 = valid_count(&file_2, is_valid_2);
    printf("Passes for password criterion 2: %d\n", c_2);
    return 0;
}

