# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
# contributed by Adam Beckmeyer

using Printf

const V = VecElement
const m128d = NTuple{2,V{Float64}}

# Use llvmcall to force SIMD vectorization of 
# packed double addition and division
Base.:+(a::m128d, b::m128d) =
    Base.llvmcall("""
        %res = fadd <2 x double> %0, %1
        ret <2 x double> %res
    """, m128d, Tuple{m128d, m128d}, a, b)
Base.:/(a::m128d, b::m128d) =
    Base.llvmcall("""
        %res = fdiv <2 x double> %0, %1
        ret <2 x double> %res
    """, m128d, Tuple{m128d, m128d}, a, b)

A(i, j) = (i + j) * (i + j + 1) / 2 + i + 1
At(i, j) = A(j, i)

function Av!(f, v, out)
    n = length(v)
    Threads.@threads for i=1:n
        x = (V(0.0), V(0.0))
        @inbounds for j=1:2:n
            x += @inbounds (V(v[j]), V(v[j+1])) / (V(f(i-1, j-1)), V(f(i-1, j)))
        end
        @inbounds out[i] = x[1].value + x[2].value
    end
end

function main(n)
    # This program is not compatible with odd values of n
    isodd(n) && (n += 1)

    u = ones(Float64, n)
    v = Vector{Float64}(undef, n)
    # temporary working vector w
    w = Vector{Float64}(undef, n)

    for _=1:10
        Av!(A, u, w)
        Av!(At, w, v)
        Av!(A, v, w)
        Av!(At, w, u)
    end

    uv = vv = 0.0
    @inbounds for i=1:n
        uv += u[i] * v[i]
        vv += v[i] * v[i]
    end
    sqrt(uv / vv)
end

isinteractive() || @printf("%.9f\n", main(parse(Int, ARGS[1])))
