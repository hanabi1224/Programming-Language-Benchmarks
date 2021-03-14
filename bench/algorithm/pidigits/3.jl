#=
 The Computer Language Benchmarks Game
 https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

 based on the C++ program of Alessandro Power
 with ideas borrowed from the C program of Mr Ledrug
 modified for Julia 1.0 by Luiz M. Faria
=#

# import a few functions from GMP wrapper to do in-place operations
using Base.GMP.MPZ: add_ui!, mul_ui!, add!, tdiv_q!
# and add a few more wrappers for GMP functions not currently in Base
const mpz_t = Ref{BigInt}
addmul_ui!(x::BigInt,a::BigInt,b) = ccall((:__gmpz_addmul_ui,"libgmp"), Cvoid, (mpz_t,mpz_t,Culong), x, a, b)
submul_ui!(x::BigInt,a::BigInt,b) = ccall((:__gmpz_submul_ui,"libgmp"), Cvoid, (mpz_t,mpz_t,Culong), x, a, b)
get_ui(x::BigInt) = ccall((:__gmpz_get_ui,"libgmp"), Culong, (mpz_t,), x)

function next!(q,r,t,k)
    mult  = 2*k + 1
    addmul_ui!(r,q,2)
    mul_ui!(r,mult)
    mul_ui!(t,mult)
    mul_ui!(q,k)
    return nothing
end

function extract!(q,r,t,tmp1,tmp2,n)
    mul_ui!(tmp1,q,n)
    add!(tmp1,r)
    tdiv_q!(tmp2,tmp1,t)
    return get_ui(tmp2)
end

function produce!(q,r,t,n)
    mul_ui!(q,10)
    submul_ui!(r,t,n)
    mul_ui!(r,10)
    return nothing
end

function pidigits(n::Int)
    q,r,t,tmp1,tmp2 = map(BigInt,(1,0,1,0,0))
    k = UInt(0)
    n_digits=0

    while n_digits<n
        i=0
        while (i<10 && n_digits<n)
            k +=1
            next!(q,r,t,k)
            q>r && continue

            digit = extract!(q,r,t,tmp1,tmp2,3)
            if digit == extract!(q,r,t,tmp1,tmp2,4)
                print(digit)
                produce!(q,r,t,digit)
                i +=1
                n_digits+=1
            end
        end
        while i < 10
            i+=1
            print(' ')
        end
        print("\t:$n_digits\n")
    end
end

n = parse(Int,ARGS[1])
pidigits(n)
