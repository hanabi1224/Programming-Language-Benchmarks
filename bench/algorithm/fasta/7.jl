# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# Contributed by Kristoffer Carlsson
# Updated by Adam Beckmeyer

import .Threads: @spawn

const LINESIZE = 60
const CHUNKSIZE = 512LINESIZE

const IM = 139968 % Int32
const IA = 3877 % Int32
const IC = 29573 % Int32
const SEED = Ref(42 % Int32)

gen_random() = (SEED[] = ((SEED[] * IA) + IC) % IM)

# Vector{UInt8} faster indexing than Base.CodeUnits{UInt8,String} (julia 1.2)
const alu = Vector{UInt8}(string(
   "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG",
   "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA",
   "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT",
   "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA",
   "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG",
   "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC",
   "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"))

const iub1 = Vector{UInt8}("acgtBDHKMNRSVWY")
# Instead of the probability, iub2 is the integer upper-bounding the random
# integer to the equivalent index in iub1. cumsum is not used here as
# it adds almost 10% of total time to inference time.
const iub2 = [floor(Int32, IM * x) for x in
              (0.27, 0.39, 0.51, 0.78, 0.8, 0.82, 0.84, 0.86, 0.88, 0.9, 0.92,
               0.94, 0.96, 0.98, 1.0)]
const homosapiens1 = Vector{UInt8}("acgt")
const homosapiens2 = [floor(Int32, IM * x) for x in
                      (0.3029549426680, 0.5009432431601, 0.6984905497992, 1.0)]

# Defining a function with successive comparisons is faster than getindex lookup
for (op, symb, pr) in ((:iub, iub1, iub2),
                       (:homosapiens, homosapiens1, homosapiens2))
    # successively compares x with all values in pr and returns value from symb
    @eval @inline function $op(x)
        $([:(x <= $(pr[i]) && return $(symb[i])) for i=1:length(symb)]...)
        'a' % UInt8
    end
end

function repeat_fasta(io, src, n)
    len = length(src)
    pos = 1
    GC.@preserve src for i=1:LINESIZE:n
        # l is the number of characters we need to write this iteration.
        # m is less than l if not enough characters are left to write from src.
        l = min(LINESIZE, n - i + 1)
        m = min(l, len - pos + 1)

        unsafe_write(io, pointer(src, pos), m)
        if l != m # reached end of src
            # Loop back around to the start of src and write remaining chars
            pos = 1
            m = l - m
            unsafe_write(io, pointer(src, pos), m)
        end
        pos += m

        write(io, '\n')
    end
end

# Writes chunks of random characters from c to io with newlines added
# every LINESIZE. c is filled with Task which must be ordered. The
# chunks yielded from each Task must be a multiple in length of
# LINESIZE unless they are the last chunk in the channel.
function print_chunks(io, c)
    for vt in c
        v = fetch(vt)::Vector{UInt8}
        n = length(v)
        for i=1:LINESIZE:n
            GC.@preserve v unsafe_write(io, pointer(v, i),
                                        min(LINESIZE, n - i + 1))
            write(io, '\n')
        end
    end
end

# Put tasks that yield chunks of random characters into c. The total
# number of characters before c is closed will be n.
function generate_chunks(flookup, c, n)
    for p=1:CHUNKSIZE:n
        # Create a chunk of random Int32 of length l
        l = min(CHUNKSIZE, n - p + 1)
        v = Vector{Int32}(undef, l)
        # Fill chunk with random numbers
        for i=1:l
            @inbounds v[i] = gen_random()
        end

        # Spawn task to convert random Int32 to characters. We must
        # put this task in the channel synchronously to make sure we
        # preserve the order of writing.
        t = @spawn @inbounds map!(flookup, Vector{UInt8}(undef, l), v)
        put!(c, t)
    end
    close(c)
end

function random_fasta(io, flookup, n, firstline, iotask, rtask=nothing)
    c = Channel{Task}(24)

    # We cannot proceed with generating random numbers until rtask is
    # complete otherwise the random numbers will be interleaved.
    rtask === nothing || wait(rtask)
    rtask = @spawn generate_chunks(flookup, c, n)
    # Similarily, we can't start writing to io until the previous
    # iotask has completed.
    iotask2 = @spawn begin
        wait(iotask)
        write(io, firstline)
        print_chunks(io, c)
    end

    iotask2, rtask
end

function fasta(n=25000000, io=stdout)
    write(io, ">ONE Homo sapiens alu\n")
    io1 = @spawn repeat_fasta(io, alu, 2n)

    io2, r1 = random_fasta(io, iub, 3n, ">TWO IUB ambiguity codes\n", io1)
    io3, _ = random_fasta(io, homosapiens, 5n, ">THREE Homo sapiens frequency\n",
                          io2, r1)
    wait(io3)
end

isinteractive() || fasta(parse(Int,ARGS[1]))