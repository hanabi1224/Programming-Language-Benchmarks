def get_n()
    n = 100
    if ARGV.size > 0
        n= Integer(ARGV[0])
    end
    n
end

def generate(y)
    i = 2
    while true do
        y << i
        i += 1
    end
end

def filter(y, stream, prime)
    loop do
        i = stream.next
        if i % prime != 0
            y << i
        end
    end
end

n = get_n()
stream = Enumerator.new {|y| generate(y)}
(0...n).each do
    prime = stream.next
    puts prime
    moved_stream = stream
    stream = Enumerator.new {|y| filter(y, moved_stream, prime)}
end
