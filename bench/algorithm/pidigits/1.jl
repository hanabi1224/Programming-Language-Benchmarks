# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

# contributed by Jarret Revels
# based on Mario Pernici Python's program
# modified by hanabi1224

using Printf

function pidigits(N::Int)
    i = k = 0
    k1 = 1
    n,a,d,t,u = map(BigInt,(1,0,1,0,0))

    while true
        k += 1
        t = n << 1
        n *= k
        a += t
        k1 += 2
        a *= k1
        d *= k1

        if a >= n
            t,u = divrem(n*3 +a, d)
            u += n
            if d > u
                i += 1
                print(t)
                mod_i = mod(i,10)
                if mod_i == 0
                    @printf("\t:%d\n", i)
                end
                if i >= N
                    if mod_i > 0
                        for n in 1:(10 - mod_i)
                            print(' ')
                        end
                        @printf("\t:%d\n", i)
                    end
                    return
                end
                a -= d * t
                a *= 10
                n *= 10
            end
        end
    end
end

n = parse(Int,ARGS[1])
pidigits(n)
