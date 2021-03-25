# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# contributed by Gabriele Renzi
# ported from ruby to crystal by hanabi1224

require "big"

ZERO = BigInt.new 0
ONE = BigInt.new 1
THREE = BigInt.new 3
FOUR = BigInt.new 4
TEN = BigInt.new 10

class PiDigitSpigot
    def initialize()
        @y = ZERO
        @z = Transformation.new ONE,ZERO,ZERO,ONE
        @x = Transformation.new ZERO,ZERO,ZERO,ZERO
    end

    def next!
        @y = @z.extract(THREE)
        if safe? @y
            @z = produce(@y)
            @y
        else
            @z = consume @x.next!()
            next!()
        end
    end

    def safe?(digit)
        digit == @z.extract(FOUR)
    end

    def produce(i)
        Transformation.new(TEN,-i*TEN,ZERO,ONE).compose(@z)
    end

    def consume(a)
        @z.compose(a)
    end
end


class Transformation
    property q
    property r
    property s
    property t
    def initialize (q : BigInt, r : BigInt, s : BigInt, t : BigInt)
        @q,@r,@s,@t,@k = q,r,s,t,ZERO
    end

    def next!()
        @q = @k = @k + 1
        @r = 4 * @k + 2
        @s = 0
        @t = 2 * @k + 1
        self
    end

    def extract(j : BigInt) : BigInt
        (@q * j + @r) // (@s * j + @t)
    end

    def compose(a)
        self.class.new( @q * a.q,
                        @q * a.r + r * a.t,
                        @s * a.q + t * a.s,
                        @s * a.r + t * a.t
                    )
    end
end


WIDTH = 10
n = ARGV.size > 0 ? ARGV[0].to_i : 27
j = 0

digits = PiDigitSpigot.new

while n > 0
    if n >= WIDTH
        WIDTH.times {print digits.next!}
        j += WIDTH
    else
        n.times {print digits.next!}
        (WIDTH-n).times {print " "}
        j += n
    end
    puts "\t:"+j.to_s
    n -= WIDTH
end
