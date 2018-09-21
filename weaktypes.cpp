// g++ -Wall -Wextra -Werror -pedantic

#include <cstdlib>
#include <iostream>
using namespace std;

int main() {
    uint16_t x = 65535;
    cout << x << endl;  // 65535

    uint64_t y = x * x;
    cout << y << endl;  // ?

    return 0;
}
