#include <iostream>
#include <fstream>
#include <set>
#include <tuple>
#include "maybe.cpp"

using namespace std;

Maybe<tuple<int, int>> two_add_to(int n, set<int> &xs) {
    for (int i : xs) {
        int match = n - i;
        if (xs.count(match)) {
            cout << i << " : " << match << "\n";
            return Maybe<tuple<int, int>>({i, match});
        }
    }
    return Nothing<tuple<int, int>>();
}

Maybe<tuple<int, int, int>> three_add_to(int n, set<int> &xs) {
    for (int i : xs) {
        auto two = two_add_to(n - i, xs);
        auto nums = two.withDefault({i, i});
        if (get<0>(nums) != i)
            return Just<tuple<int, int, int>>({i, get<0>(nums), get<1>(nums)});
    }
    return Nothing<tuple<int, int, int>>();
}

int main () {
    ifstream file ("./puzzle-inputs/1.txt");
    set<int> numbers;

    {   // Add all numbers to numbers;
        string line;
        while (getline(file, line)) numbers.insert(stoi(line));
    }

    {
        auto t = two_add_to(2020, numbers);
        t.run<void>(
            [](tuple<int, int> v){
                cout
                    << "Result: "
                    << get<0>(v)
                    << " and "
                    << get<1>(v)
                    << " which add to "
                    << get<0>(v) * get<1>(v)
                    << "\n";
            },
            [](void) {
                cout << "No result found";
            }
        );
    }

    {
        auto t = three_add_to(2020, numbers);
        t.run<void>(
            [](tuple<int, int, int> v){
                cout
                    << "Result: "
                    << get<0>(v)
                    << ", "
                    << get<1>(v)
                    << " and "
                    << get<1>(v)
                    << " which add to "
                    << get<0>(v) * get<1>(v) * get<2>(v)
                    << "\n";
            },
            [](void) {
                cout << "No result found";
            }
        );
    }

    return 0;
}
