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
	for i := range size {
		xloc[i/8][i%8] = float64(i)*inv - 1.5
	}
	fmt.Printf("P4\n%d %d\n", size, size)

	pixels := make([]byte, size*chunk_size)
	for chunk_id := range size {
		ci := float64(chunk_id)*inv - 1.0
		offset := chunk_id * chunk_size
		for i := range chunk_size {
			if r := mbrot8(&xloc[i], ci); r > 0 {
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
	var zr, zi, tr, ti, absz, tmp [8]float64
	for range 10 {
		for range 5 {
			add(&zr, &zr, &tmp)
			mul(&tmp, &zi, &tmp)
			add(&tmp, &ci, &zi)

			minus(&tr, &ti, &tmp)
			add(&tmp, cr, &zr)

			mul(&zr, &zr, &tr)
			mul(&zi, &zi, &ti)
		}
		add(&tr, &ti, &absz)
		if !(absz[0] <= 4.0 || absz[1] <= 4.0 ||
			absz[2] <= 4.0 || absz[3] <= 4.0 ||
			absz[4] <= 4.0 || absz[5] <= 4.0 ||
			absz[6] <= 4.0 || absz[7] <= 4.0) {
			return 0
		}
	}
	accu := byte(0)
	for i := range 8 {
		if absz[i] <= 4.0 {
			accu |= byte(0x80) >> i
		}
	}
	return accu
}

func add(a, b, r *[8]float64) {
	r[0], r[1] = a[0]+b[0], a[1]+b[1]
	r[2], r[3] = a[2]+b[2], a[3]+b[3]
	r[4], r[5] = a[4]+b[4], a[5]+b[5]
	r[6], r[7] = a[6]+b[6], a[7]+b[7]
}
func minus(a, b, r *[8]float64) {
	r[0], r[1] = a[0]-b[0], a[1]-b[1]
	r[2], r[3] = a[2]-b[2], a[3]-b[3]
	r[4], r[5] = a[4]-b[4], a[5]-b[5]
	r[6], r[7] = a[6]-b[6], a[7]-b[7]
}

func mul(a, b, r *[8]float64) {
	r[0], r[1] = a[0]*b[0], a[1]*b[1]
	r[2], r[3] = a[2]*b[2], a[3]*b[3]
	r[4], r[5] = a[4]*b[4], a[5]*b[5]
	r[6], r[7] = a[6]*b[6], a[7]*b[7]
}
