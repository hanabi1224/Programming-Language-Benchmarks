#=
The Computer Language Benchmarks Game
 https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

 direct transliteration of the swift#3 program by Ralph Ganszky and Daniel Muellenborn

 modified for Julia 1.0 by Simon Danisch
 tweaked for performance by maltezfaria and Adam Beckmeyer
=#

push!(LOAD_PATH, "MD5");

using Base.Cartesian
using Base64.Base64
using MD5
# include("MD5/MD5.jl")

# Calculate the byte to print for a given vector of 8 real numbers cr
# and a given imaginary component ci. This function should give the
# same result whether prune is true or false but may be faster or
# slower depending on the input.
function mand8(cr, ci, prune)
    zr = zi = tr = ti = @ntuple 8 _-> 0.0

    # In cases where the last call to mand8 resulted in 0x00, the next
    # call is much more likely to result in 0x00, so it's worth it to
    # check several times if the calculation can be aborted
    # early. Otherwise, the relatively costly check can be eliminated.
    if prune
        for _=1:10
            for _=1:5
                zr, zi, tr, ti = calc_sum(cr, ci, zr, zi , tr, ti)
            end
            # If all values are already > 4.0, we already know no
            # values will be <= 4.0 at end of iterations. Reduction
            # with binary & is used here instead of all function to
            # avoid branches caused by short-circuiting behavior.
            foldl(&, @ntuple(8, k-> tr[k] + ti[k] > 4.0)) && return 0x00
        end
    else
        for _=1:50
            zr, zi, tr, ti = calc_sum(cr, ci, zr, zi , tr, ti)
        end
    end

    # For each value <= 4.0, the k-th bit (counting from left) is set to 1
    foldl(|, @ntuple(8, k-> ifelse(tr[k] + ti[k] <= 4.0, 0x80 >> (k - 1), 0x00)))
end

# Single iteration of mandelbrot calculation for vector r of real
# components and vector i or imaginary components.
@inline function calc_sum(cr, ci, zr, zi, tr, ti)
    # Using broadcasting (r2 = r .* r) generates operations on llvm
    # <8 x double> vectors even with --cpu-target=core2 (widest simd
    # register on core2 is <2 x double>). @ntuple results in better
    # codegen (uses <2 x double>).
    zi = @ntuple 8 k-> muladd(zr[k] + zr[k], zi[k], ci)
    zr = @ntuple 8 k-> tr[k] - ti[k] + cr[k]
    tr = @ntuple 8 k-> zr[k] * zr[k]
    ti = @ntuple 8 k-> zi[k] * zi[k]
    zr, zi, tr, ti
end

# Write n by n portable bitmap image of mandelbrot set to io
function mandelbrot(io, n_raw)
    n = (n_raw + 7) ÷ 8 * 8

    # Precalculate real coordinates to check
    xvals = Float64[2i/n - 1.5 for i=0:n-1]
    # Precalculate imaginary coordinates to check
    yvals = Float64[2i/n - 1.0 for i=0:n-1]

    # Create a vector of bytes to output
    out = Vector{UInt8}(undef, n * n ÷ 8)
    # For each row (each imaginary coordinate), spawn a thread to fill
    # out values. At small values of n, this is too fine-grained of
    # parallelism to really be efficient, but it works well for large n.
    @sync for y=1:n
        # Threads.@spawn allows dynamic scheduling instead of static scheduling
        # of Threads.@threads macro.
        # See <https://github.com/JuliaLang/julia/issues/21017>.
        Threads.@spawn @inbounds begin
            ci = yvals[y]
            startofrow = (y - 1) * n ÷ 8
            # The first iteration within a row will generally return 0x00
            prune = true
            for x=1:8:n
                # Calculate whether the (x:x+7)-th real coordinates with
                # the y-th imaginary coordinate belong to the
                # mandelbrot set.
                byte = mand8(@ntuple(8, k-> xvals[x+k-1]), ci, prune)
                out[startofrow + x÷8 + 1] = byte
                prune = byte == 0x00
            end
        end
    end

    println("P4\n$n $n")
    println(bytes2hex(MD5.md5("test")))
end

isinteractive() || mandelbrot(stdout, parse(Int, ARGS[1]))
