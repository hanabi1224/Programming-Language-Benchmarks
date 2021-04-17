def generate(ch : Channel(Int32))
  spawn do
    i = 2
    loop do
      ch.send i
      i += 1
    end
  end
end

def filter(ch_in : Channel(Int32), ch_out : Channel(Int32), prime : Int32)
  spawn do
    loop do
      i = ch_in.receive
      if i % prime != 0
        ch_out.send(i)
      end
    end
  end
end

n = ARGV.size > 0 ? ARGV[0].to_i : 100
ch = Channel(Int32).new 2
f = generate(ch)
(0...n).each do
  prime = ch.receive
  puts prime
  ch_next = Channel(Int32).new 2
  filter ch, ch_next, prime
  ch = ch_next
end
