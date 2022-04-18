package main

import (
	"crypto/md5"
	"fmt"
	"os"
	"strconv"
)

func main() {
	size := 200
	if len(os.Args) > 1 {
		if _size, err := strconv.Atoi(os.Args[1]); err == nil {
			size = _size
		}
	}
	size = (size + 7) / 8 * 8
	chunk_size := size / 8
	inv := 2.0 / float64(size)
	xloc := make([][8]float64, chunk_size)
	for i := 0; i < size; i++ {
		xloc[i/8][i%8] = float64(i)*inv - 1.5
	}
	fmt.Printf("P4\n%d %d\n", size, size)

	pixels := make([]byte, size*chunk_size)
	for chunk_id := 0; chunk_id < size; chunk_id++ {
		ci := float64(chunk_id)*inv - 1.0
		offset := chunk_id * chunk_size
		for i := 0; i < chunk_size; i++ {
			r := mbrot8(&xloc[i], ci)
			if r > 0 {
				pixels[offset+i] = r
			}
		}
	}

	hasher := md5.New()
	hasher.Write(pixels)
	fmt.Printf("%x\n", hasher.Sum(nil))
}

func mbrot8(cr *[8]float64, civ float64) byte {
	ci := [8]float64{civ, civ, civ, civ, civ, civ, civ, civ}
	zr := [8]float64{}
	zi := [8]float64{}
	tr := [8]float64{}
	ti := [8]float64{}
	absz := [8]float64{}
	tmp := [8]float64{}
	for _i := 0; _i < 10; _i++ {
		for _j := 0; _j < 5; _j++ {
			add(&zr, &zr, &tmp)
			mul(&tmp, &zi, &tmp)
			add(&tmp, &ci, &zi)

			minus(&tr, &ti, &tmp)
			add(&tmp, cr, &zr)

			mul(&zr, &zr, &tr)
			mul(&zi, &zi, &ti)
		}
		add(&tr, &ti, &absz)
		terminate := true
		for i := 0; i < 8; i++ {
			if absz[i] <= 4.0 {
				terminate = false
				break
			}
		}
		if terminate {
			return 0
		}
	}
	accu := byte(0)
	for i := 0; i < 8; i++ {
		if absz[i] <= 4.0 {
			accu |= byte(0x80) >> i
		}
	}
	return accu
}

func add(a, b, r *[8]float64) {
	for i := 0; i < 8; i++ {
		r[i] = a[i] + b[i]
	}
}

func minus(a, b, r *[8]float64) {
	for i := 0; i < 8; i++ {
		r[i] = a[i] - b[i]
	}
}

func mul(a, b, r *[8]float64) {
	for i := 0; i < 8; i++ {
		r[i] = a[i] * b[i]
	}
}
