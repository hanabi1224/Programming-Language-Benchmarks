function nsieve(limit::Int)
    primes = trues(limit)
    primes[1] = false
    p = 2
    while !isnothing(p) && p <= isqrt(limit)
        primes[p*p:p:limit] .= false
        p = findnext(primes, p + 1)
    end
    println("Primes up to ", lpad(limit, 8), lpad(count(primes), 9))
end

function main()
    n = isempty(ARGS) ? 4 : parse(Int, ARGS[1])
    for i in n:-1:n-2
        nsieve(10000 << i)
    end
end

isinteractive() || main()
