// The Computer Language Shootout
// http://shootout.alioth.debian.org/
// Precedent C entry modified by bearophile for speed and size, 31 Jan 2006
// Converted to C++ by Paul Kitchin

#include <iomanip>
#include <iostream>
#include <sstream>
#include <vector>

#include <iostream>
#include <cmath>
#include <vector>
#include <algorithm>

inline void nsieve(unsigned int limit) {
    std::vector<bool> primes(limit + 1, true);
    primes[1] = false;
    unsigned int p = 2;
    while (p <= static_cast<unsigned int>(std::sqrt(limit))) {
        for (unsigned int i = p * p; i <= limit; i += p) {
            primes[i] = false;
        }
        p = std::find(primes.begin() + p + 1, primes.end(), true) - primes.begin();
    }
    unsigned int primeCount = std::count(primes.begin(), primes.end(), true);
    std::cout << "Primes up to " << std::setw(8) << limit << std::setw(9) << primeCount << '\n';
}

int main(int argc, char **argv) {
    unsigned int n = (argc <= 1) ? 4 : std::atoi(argv[1]);
    for (unsigned int i = n; i >= n - 2; --i) {
        nsieve(10000 << i);
    }
    return 0;
}
