// The Computer Language Shootout
// http://shootout.alioth.debian.org/
// Precedent C entry modified by bearophile for speed and size, 31 Jan 2006
// Converted to C++ by Paul Kitchin

#include <iomanip>
#include <iostream>
#include <sstream>
#include <vector>

void nsieve(std::size_t max)
{
   static std::vector< unsigned char > flags;
   flags.assign(max, false);
   std::size_t count = 0;
   for (std::size_t value = 2; value < max; ++value)
   {
      if (!flags[value])
      {
         ++count;
         for (std::size_t multiple = value * 2; multiple < max; multiple += value)
         {
            flags[multiple] = true;
         }
      }
   }
   std::cout << "Primes up to " << std::setw(8) << max << ' ' << std::setw(8) << count << '\n';
}

int main(int argc, char * * argv)
{
   if (argc != 2)
   {
      std::cerr << "usage: " << argv[0] << " <n>\n";
      return 1;
   }
   unsigned int count;
   {
      std::istringstream convertor(argv[1]);
      if (!(convertor >> count) || !convertor.eof())
      {
         std::cerr << "usage: " << argv[0] << " <n>\n";
         std::cerr << "\tn must be an integer\n";
         return 1;
      }
   }
   for (std::size_t i = 0; i < 3; ++i)
   {
      nsieve(10000 << (count - i));
   }
}
