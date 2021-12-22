// The Computer Language Shootout
// http://shootout.alioth.debian.org/
// A faster C++ nsieve, by yonillasky

#include <iomanip>
#include <iostream>
#include <sstream>
#include <numeric>
#include <vector>
#include <algorithm>
#include <math.h>

void nsieve(std::size_t max) {
  // is_prime[x] specifies whether 2x + 3 was found to be prime
  static std::vector<uint8_t> is_prime;
  std::size_t num_entries = 1 + (max - 3) / 2;
  is_prime.assign(num_entries, 1u);
  // cutoff for sieve, min. divisor <= sqrt(max)
  std::size_t cutoff = std::min((std::size_t(sqrt(max)) - 1) / 2, num_entries);
  std::size_t i = 0;
  for (; i < cutoff; ++i) {
    if (is_prime[i] == 0u) continue;

    std::size_t offset = 3 * i + 3;
    std::size_t delta = 2 * i + 3;
    for (; offset < num_entries; offset += delta) {
      is_prime[offset] = 0u;
    }
  }
  std::size_t count = 1 + std::accumulate(is_prime.begin(), is_prime.end(), std::size_t(0), [](std::size_t acc, uint8_t x) {return acc + x;});
  std::cout << "Primes up to " << std::setw(8) << max << ' ' << std::setw(8)
            << count << '\n';
}

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "usage: " << argv[0] << " <n>\n";
    return 1;
  }
  unsigned int count;
  {
    std::istringstream convertor(argv[1]);
    if (!(convertor >> count) || !convertor.eof()) {
      std::cerr << "usage: " << argv[0] << " <n>\n";
      std::cerr << "\tn must be an integer\n";
      return 1;
    }
  }
  for (std::size_t i = 0; i < 3; ++i) {
    nsieve(10000 << (count - i));
  }
}
