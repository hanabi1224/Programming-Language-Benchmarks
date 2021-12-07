import os, strutils, bigints, strformat

const 
    ONE = initBigInt(1)
    TWO = initBigInt(2)
    TEN = initBigInt(10)

var 
    digits_printed = 0
    k = initBigInt(1)
    n1 = initBigInt(4)
    n2 = initBigInt(3)
    d = initBigInt(1)
    u, v, w = initBigInt(0)

let n = parseInt(paramStr(1))

while true:
    u = n1 div d
    v = n2 div d
    if u == v:
        stdout.write(u)
        digits_printed += 1
        let digits_printed_mod_ten = digits_printed mod 10
        if digits_printed_mod_ten == 0:
            stdout.writeLine(&"\t:{digits_printed}")
        if digits_printed >= n:
            if digits_printed_mod_ten > 0:
                var j = 0
                while j < 10 - digits_printed_mod_ten:
                    stdout.write(" ")
                    j += 1
                stdout.writeLine(&"\t:{digits_printed}")
            break
        
        let to_minus = u * TEN * d
        n1 = n1 * TEN - to_minus
        n2 = n2 * TEN - to_minus
    else:
        let k2 = k * TWO
        u = n1 * (k2 - ONE)
        v = n2 * TWO
        w = n1 * (k - ONE)
        n1 = u + v
        u = n2 * (k + TWO)
        n2 = w + u
        d = d * (k2 + ONE)
        k = k + ONE
