# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

# based on Oleg Mazurov's Java Implementation and Jeremy Zerfas' C implementation
# transliterated and modified by Hamza Yusuf Çakır

global const preferred_num_blocks = 24

struct Fannkuch
    n::Int64
    blocksz::Int64
    maxflips::Vector{Int32}
    chksums::Vector{Int32}

    function Fannkuch(n, nthreads)
        nfact = factorial(n)

        blocksz = nfact ÷ (nfact < preferred_num_blocks ? 1 : preferred_num_blocks)
        maxflips = zeros(Int32, nthreads)
        chksums = zeros(Int32, nthreads)

        new(n, blocksz, maxflips, chksums)
    end
end

struct Perm
    p::Vector{Int8}
    pp::Vector{Int8}
    count::Vector{Int32}

    function Perm(n)
        p = zeros(Int8, n)
        pp = zeros(Int8, n)
        count = zeros(Int32, n)

        new(p, pp, count)
    end
end

Base.@propagate_inbounds @inline function first_permutation(perm::Perm, idx)
    p = perm.p
    pp = perm.pp

    for i = 2:length(p)
        p[i] = (i - 1) % Int8
    end

    for i = length(p):-1:2
        ifact = factorial(i-1)
        d = idx ÷ ifact
        perm.count[i] = d
        idx = idx % ifact

        for j = 1:i
            pp[j] = p[j]
        end

        for j = 1:i
            p[j] = j+d <= i ? pp[j+d] : pp[j+d-i]
        end
    end
end

Base.@propagate_inbounds @inline function next_permutation(perm::Perm)
    p = perm.p
    count = perm.count

    first = p[2]
    p[2]  = p[1]
    p[1]  = first

    i = 2
    while count[i] >= i - 1
        count[i] = 0

        next = p[1] = p[2]

        for j = 1:i
            p[j] = p[j+1]
        end

        i += 1
        p[i] = first
        first = next
    end
    count[i] += 1
    nothing
end

Base.@propagate_inbounds @inline function count_flips(perm::Perm)
    p = perm.p
    pp = perm.pp

    flips = Int32(1)

    first = p[1] + 1

    if p[first] != 0

        unsafe_copyto!(pp, 2, p, 2, length(p) - 1)

        while true
            flips += one(flips)
            new_first = pp[first]
            pp[first] = (first - 1) % Int8

            if first > 3
                lo = 2; hi = first - 1
                # see the note in Jeremy Zerfas' C implementation for
                # this loop
                for k = 0:13
                    t = pp[lo]
                    pp[lo] = pp[hi]
                    pp[hi] = t
                    (hi < lo + 3) && break
                    lo += 1
                    hi -= 1
                end
            end

            first = new_first + 1
            pp[first] == 0 && break
        end
    end

    return flips
end

Base.@propagate_inbounds function run_task(f::Fannkuch, perm::Perm, idxmin, idxmax)
    maxflips = Int32(0)
    chksum = Int32(0)

    i = idxmin
    while true
        if perm.p[1] != 0
            flips = count_flips(perm)
            (flips > maxflips) && (maxflips = flips)
            chksum += iseven(i) ? flips : -flips
        end
        i != idxmax || break
        i += 1
        next_permutation(perm)
    end

    id = Threads.threadid()
    (maxflips > f.maxflips[id]) && (f.maxflips[id] = maxflips)
    f.chksums[id] += chksum
    nothing
end

function runf(f::Fannkuch)
    factn = factorial(f.n)

    Threads.@threads for idxmin = 0:f.blocksz:factn-1
        perm = Perm(f.n)
        @inbounds first_permutation(perm, idxmin)
        idxmax = idxmin + f.blocksz - 1
        @inbounds run_task(f, perm, idxmin, idxmax)
    end
end

function fannkuchredux(n)
    f = Fannkuch(n, Threads.nthreads())

    runf(f)

    # reduce results
    chk = sum(f.chksums)
    res = maximum(f.maxflips)

    println(chk, "\nPfannkuchen(", n, ") = ", res)
end

n = parse(Int, ARGS[1])
fannkuchredux(n)