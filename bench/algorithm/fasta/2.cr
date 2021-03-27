# From https://github.com/kostya/crystal-benchmarks-game/

IM = 139968
IA =   3877
IC =  29573

class Last
  def self.last
    @@last ||= 42
  end

  def self.last=(x)
    @@last = x
  end
end

def gen_random(max)
  Last.last = (Last.last * IA + IC) % IM
  max * Last.last / IM.to_f64
end

def make_cumulative(genelist)
  cp = 0.0_f64
  genelist.size.times do |i|
    c, p = genelist[i]
    cp += p
    genelist[i] = {c, cp}
  end
end

def select_random(genelist)
  r = gen_random(1)
  return genelist[0][0] if r < genelist[0][1]

  lo = 0
  hi = genelist.size - 1

  while hi > lo + 1
    i = (hi + lo) // 2
    if r < genelist[i][1]
      hi = i
    else
      lo = i
    end
  end
  genelist[hi][0]
end

LINE_LENGTH = 60

def make_random_fasta(id, desc, genelist, n)
  todo = n
  puts ">#{id} #{desc}"

  while todo > 0
    m = (todo < LINE_LENGTH) ? todo : LINE_LENGTH
    pick = String.new(m) do |buffer|
      m.times { |i| buffer[i] = select_random(genelist).ord.to_u8 }
      {m, m}
    end
    puts(pick)
    todo -= LINE_LENGTH
  end
end

def make_repeat_fasta(id, desc, s, n)
  todo = n
  k = 0
  kn = s.size

  puts ">#{id} #{desc}"
  while todo > 0
    m = (todo < LINE_LENGTH) ? todo : LINE_LENGTH

    while m >= kn - k
      print(s[k..-1])
      m -= kn - k
      k = 0
    end

    puts(s[k...k + m])
    k += m

    todo -= LINE_LENGTH
  end
end

iub = [
  {'a', 0.27},
  {'c', 0.12},
  {'g', 0.12},
  {'t', 0.27},

  {'B', 0.02},
  {'D', 0.02},
  {'H', 0.02},
  {'K', 0.02},
  {'M', 0.02},
  {'N', 0.02},
  {'R', 0.02},
  {'S', 0.02},
  {'V', 0.02},
  {'W', 0.02},
  {'Y', 0.02},
]

homosapiens = [{'a', 0.3029549426680}, {'c', 0.1979883004921}, {'g', 0.1975473066391}, {'t', 0.3015094502008}]
alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

n = (ARGV[0]? || 1000).to_i
make_cumulative(iub)
make_cumulative(homosapiens)

make_repeat_fasta("ONE", "Homo sapiens alu", alu, n * 2)
make_random_fasta("TWO", "IUB ambiguity codes", iub, n * 3)
make_random_fasta("THREE", "Homo sapiens frequency", homosapiens, n * 5)
