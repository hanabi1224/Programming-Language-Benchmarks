// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// Contributed by Kristian Meyer
// Translated Jeremy Zerfas's C gcc #9 program to use goroutines instead of openmp.

package main

import (
	"crypto/md5"
	"flag"
	"fmt"
	"log"
	"runtime"
	"strconv"
	"sync"
)

// This is the square of the limit that pixels will need to exceed in order to
// escape from the Mandelbrot set.
const LIMIT_SQUARED = 4.0

// This controls the maximum amount of iterations that are done for each pixel.
const MAXIMUM_ITERATIONS = 50

// targeting a q6600 system, 1 cpu worker per core.
const POOL = 4

var image_Width_And_Height int
var initial_r []float64
var initial_i []float64
var pixels []byte

func calc(wg *sync.WaitGroup, band int) {
	for y := band; y < image_Width_And_Height; y += POOL {

		prefetched_Initial_i := initial_i[y]
		for x_Major := 0; x_Major < image_Width_And_Height; x_Major += 8 {

			// pixel_Group_r and pixel_Group_i will store real and imaginary
			// values for each pixel in the current pixel group as we perform
			// iterations. Set their initial values here.
			var pixel_Group_r [8]float64
			var pixel_Group_i [8]float64
			for x_Minor := 0; x_Minor < 8; x_Minor++ {
				pixel_Group_r[x_Minor] = initial_r[x_Major+x_Minor]
				pixel_Group_i[x_Minor] = prefetched_Initial_i
			}

			// Assume all pixels are in the Mandelbrot set initially.
			var eight_Pixels byte = 0xff

			iteration := MAXIMUM_ITERATIONS
			for {
				var current_Pixel_Bitmask byte = 0x80
				for x_Minor := 0; x_Minor < 8; x_Minor++ {
					r := pixel_Group_r[x_Minor]
					i := pixel_Group_i[x_Minor]

					pixel_Group_r[x_Minor] = r*r - i*i +
						initial_r[x_Major+x_Minor]
					pixel_Group_i[x_Minor] = 2.0*r*i + prefetched_Initial_i

					// Clear the bit for the pixel if it escapes from the
					// Mandelbrot set.
					if r*r+i*i > LIMIT_SQUARED {
						eight_Pixels &= ^current_Pixel_Bitmask
					}

					current_Pixel_Bitmask >>= 1
				}
				iteration--
				if eight_Pixels == 0 || iteration == 0 {
					break
				}
			}

			pixels[y*image_Width_And_Height/8+x_Major/8] = eight_Pixels
		}
	}
	wg.Done()
}

func main() {
	runtime.GOMAXPROCS(POOL)
	var size int
	flag.Parse()
	if flag.NArg() > 0 {
		var err error
		size, err = strconv.Atoi(flag.Arg(0))
		if err != nil {
			log.Fatal(err)
		}
	}

	// Ensure image_Width_And_Height are multiples of 8.
	image_Width_And_Height = (size + 7) / 8 * 8
	// The image will be black and white with one bit for each pixel. Bits with
	// a value of zero are white pixels which are the ones that "escape" from
	// the Mandelbrot set. We'll be working on one line at a time and each line
	// will be made up of pixel groups that are eight pixels in size so each
	// pixel group will be one byte. This allows for some more optimizations to
	// be done.
	pixels = make([]byte, image_Width_And_Height*image_Width_And_Height/8)

	// Precompute the initial real and imaginary values for each x and y
	// coordinate in the image.
	initial_r = make([]float64, image_Width_And_Height)
	initial_i = make([]float64, image_Width_And_Height)

	// todo: multi-thread this part too
	for xy := 0; xy < image_Width_And_Height; xy++ {
		initial_r[xy] = 2.0/float64(image_Width_And_Height)*float64(xy) - 1.5
		initial_i[xy] = 2.0/float64(image_Width_And_Height)*float64(xy) - 1.0
	}

	var wg sync.WaitGroup
	for i := 0; i < POOL; i++ {
		wg.Add(1)
		go calc(&wg, i)
	}
	wg.Wait()

	// Output the image size to stdout.
	fmt.Printf("P4\n%d %d\n", image_Width_And_Height, image_Width_And_Height)
	hasher := md5.New()
	hasher.Write(pixels)
	// Output the image md5 hash to stdout.
	fmt.Printf("%x\n", hasher.Sum(nil))
}
