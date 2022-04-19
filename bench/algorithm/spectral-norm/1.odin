package main

import "core:fmt"
import "core:math"
import "core:runtime"
import "core:strconv"

main :: proc() {
    args := runtime.args__
    n := strconv.parse_int(auto_cast args[1]) or_else 10
    u := make([]f64, n)
    v := make([]f64, n)
    for i in 0..<n {
        u[i] = 1.0
        v[i] = 1.0
    }
    for _ in 0..<10 {
        a_times_transp(&v, &u, n);
        a_times_transp(&u, &v, n);
    }
    vbv := 0.0
    vv := 0.0
    for i in 0..<n {
        vbv += u[i] * v[i]
        vi := v[i]
        vv += vi * vi
    }
    ans := math.sqrt(vbv / vv)
    fmt.printf("%.9f\n", ans)
}

a_times_transp :: proc(v: ^[]f64, u: ^[]f64, n: int) {
    x := make([]f64, n)
    times(&x, u, n, false)
    times(v, &x, n, true)
}

times :: proc(v: ^[]f64, u: ^[]f64, n: int, reverse: bool) {
    for i in 0..<n {
        sum := 0.0
        for j in 0..<n {
            if reverse {
                sum += u[j] / auto_cast evala(j, i)
            }
            else {
                sum += u[j] / auto_cast evala(i, j)
            }
        }
        v[i] = sum
    }
}

evala :: proc(i: int, j: int) -> int {
    sum := i + j
    return sum * (sum + 1) / 2 + i + 1
}
