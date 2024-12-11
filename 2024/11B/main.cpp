#include <bits/stdc++.h>

typedef int64_t i64;

using namespace std;

i64 num_digits(i64 x) {
    i64 result = 0;
    while (x) {
        x /= 10;
        result += 1;
    }
    return result;
}

i64 dpt[76][1000];
i64 go(i64 n, i64 x) {
    if (n == 0) return 1;
    if (x == 0) return go(n - 1, 1);
    bool in_bounds = n <= 75 && x < 1000;
    if (in_bounds && dpt[n][x] != -1) return dpt[n][x];
    i64 result = -1;
    i64 p = num_digits(x);
    if (p % 2 == 0) {
        i64 d = pow(10, p / 2);
        i64 l = x / d, r = x % d;
        result = go(n - 1, l) + go(n - 1, r);
    } else {
        result = go(n - 1, 2024 * x);
    }
    if (in_bounds) dpt[n][x] = result;
    return result;
}

int main() {
    for (i64 n = 0; n <= 75; ++n) {
        for (i64 x = 0; x < 1000; ++x) {
            dpt[n][x] = -1;
        }
    }

    i64 result = 0;

    i64 x;
    while (cin >> x) {
        result += go(75, x);
    }

    cout << result << '\n';
}
