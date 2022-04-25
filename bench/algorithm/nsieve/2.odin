package main

import "bit_array"
// import "core:container/bit_array"
import "core:fmt"
import "core:runtime"
import "core:strconv"

main :: proc() {
    args := runtime.args__
    n := strconv.parse_int(runtime.cstring_to_string(args[1])) or_else 4
    for i in 0..<3 {
        shift : uint = auto_cast (n - i)
        nsieve(10000 << shift)
    }
}

nsieve :: proc(n : int) {
    flags, _ := bit_array.create(n)
    defer bit_array.destroy(flags)
    count := 0
    for i in 2..<n {
        if res, _ := bit_array.get(flags, i); !res {
            count += 1
            for j := i << 1; j < n; j += i {
                bit_array.set(flags, j)
            }
        }
    }
    fmt.printf("Primes up to %8s %8s\n", fmt.tprint(n), fmt.tprint(count))
}
