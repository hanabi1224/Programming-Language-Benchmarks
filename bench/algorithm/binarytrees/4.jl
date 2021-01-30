# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Contributed by Adam Beckmeyer. Based on code by Jarret Revels, Alex
# Arslan, Michal Stransky, Jens Adam.

using Distributed

@everywhere struct Node
    l::Union{Node,Nothing}
    r::Union{Node,Nothing}
end

@everywhere make(n) =
    n === 0 ? Node(nothing, nothing) : Node(make(n-1), make(n-1))

@everywhere check(node) =
    node.l === nothing ? 1 : 1 + check(node.l) + check(node.r)

function binary_trees(io, n)
    write(io, "stretch tree of depth $(n+1)\t check: $(check(make(n+1)))\n")

    long_tree = make(n)

    d = 4
    while d <= n
        niter = 1 << (n - d + 4)
        c = @distributed (+) for _ in 1:niter
            check(make(d))
        end#for
        write(io, "$niter\t trees of depth $d\t check: $c\n")
        d += 2
    end

    write(io, "long lived tree of depth $n\t check: $(check(long_tree))\n")
end#function

isinteractive() || binary_trees(stdout, parse(Int, ARGS[1]))