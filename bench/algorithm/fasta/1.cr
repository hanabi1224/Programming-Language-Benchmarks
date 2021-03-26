# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
# Contributed by Sokolov Yura
# Modified by Joseph LaFata
# ported from ruby to crystal by hanabi1224

IM = 139968
IA =   3877
IC =  29573

ALU =
  "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

IUB = {
  'a' => 0.27,
  'c' => 0.12,
  'g' => 0.12,
  't' => 0.27,
  'B' => 0.02,
  'D' => 0.02,
  'H' => 0.02,
  'K' => 0.02,
  'M' => 0.02,
  'N' => 0.02,
  'R' => 0.02,
  'S' => 0.02,
  'V' => 0.02,
  'W' => 0.02,
  'Y' => 0.02,
}

HOMOSAPIENS = {
  'a' => 0.3029549426680,
  'c' => 0.1979883004921,
  'g' => 0.1975473066391,
  't' => 0.3015094502008,
}

class Fasta
  @last = 42.0

  def gen_random(max)
    (max * (@last = (@last * IA + IC) % IM)) / IM
  end

  def make_repeat_fasta(id, desc, src, n)
    puts ">#{id} #{desc}"
    l = src.size
    s = src * ((n // l) + 1)
    s = s[0,n]
    0.step(to: s.size - 1, by: 60) { |x| print s[x, 60], "\n" }
  end

  def make_random_fasta(id, desc, table, n)
    puts ">#{id} #{desc}"
    rand, v = nil, nil
    prob = 0.0
    table.each { |(k, p)| table[k] = prob += p }
    output = String::Builder.new(capacity:60)
    n.times do
      rand = gen_random(1.0)
      table.each do |(k, p)|
        if p > rand
          output << k
          if output.bytesize == 60
            puts output.to_s
            output = String::Builder.new(capacity:60)
          end
          break
        end
      end
    end
    if output.bytesize > 0
      puts output.to_s
    end
  end
end

n = ARGV.size > 0 ? ARGV[0].to_i : 27

f = Fasta.new
f.make_repeat_fasta("ONE", "Homo sapiens alu", ALU, n*2)
f.make_random_fasta("TWO", "IUB ambiguity codes", IUB, n*3)
f.make_random_fasta("THREE", "Homo sapiens frequency", HOMOSAPIENS, n*5)
