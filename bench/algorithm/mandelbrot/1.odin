package main

import "core:crypto/md5"
import "core:fmt"
import "core:runtime"
import "core:strconv"
import "core:strings"

Vec8 :: [8]f64

main :: proc() {
    args := runtime.args__
    size := strconv.parse_int(auto_cast args[1]) or_else 200
    size = (size + 7) / 8 * 8
    chunk_size := size / 8
    inv := 2.0 / f64(size)
    xloc := make([]Vec8, chunk_size)
    for i in 0..<size {
        xloc[i/8][i%8] = f64(i) * inv - 1.5
    }
    fmt.printf("P4\n%d %d\n", size, size)

    pixels := make([]u8, size * chunk_size)
    for chunk_id in 0..<size {
        ci := f64(chunk_id)*inv - 1.0
        offset := chunk_id * chunk_size
        for i in 0..<chunk_size {
            r := mbrot8(xloc[i], ci)
            if r > 0 {
                pixels[offset + i] = r
            }
        }
    }

    hash := md5.hash_bytes(pixels)
    sb := strings.make_builder()
    strings.init_builder_len_cap(&sb, 0, 32)
    defer strings.destroy_builder(&sb)
    for b, _ in hash {
        strings.write_string_builder(&sb, fmt.tprintf("%2x", b))
    }
	fmt.printf("%s\n", strings.to_string(sb))
}

mbrot8 :: proc(cr: Vec8, civ: f64) -> u8 {
    ci := Vec8 { civ, civ, civ, civ, civ, civ, civ, civ }
    zr : Vec8
    zi : Vec8
    tr : Vec8
    ti : Vec8
    absz : Vec8 = ---
    for _ in 0..<10 {
        for _ in 0..<5 {
            zi = (zr + zr) * zi + ci
            zr = tr - ti + cr
            tr = zr * zr
            ti = zi * zi
        }
        absz = tr + ti
        terminate := true
        for i in 0..<8 {
            if absz[i] <= 4.0 {
                terminate = false
                break
            }
        }
        if terminate {
            return 0
        }
    }
    accu : u8 = 0
    for i in 0..<8 {
        if absz[i] <= 4.0 {
            accu |= 0x80 >> u8(i)
        }
    }
    return accu
}
