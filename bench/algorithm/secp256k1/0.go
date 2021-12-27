// This implementation uses CGO bindings to libsecp256k1

package main

import (
	"fmt"
	"os"
	"strconv"

	"github.com/ethereum/go-ethereum/crypto/secp256k1"
)

func main() {
	n := 1
	if len(os.Args) > 1 {
		var err error
		n, err = strconv.Atoi(os.Args[1])
		_ = err
	}

	privateKey := []byte{45, 238, 146, 112, 121, 40, 60, 60, 79, 202, 62, 249, 112, 255, 77, 56, 182, 69, 146, 227, 254, 10, 176, 218, 217, 19, 45, 112, 181, 188, 118, 147}

	curve := secp256k1.S256()
	px := curve.Gx
	py := curve.Gy
	for _i := 0; _i < n; _i++ {
		px, py = curve.ScalarMult(px, py, privateKey)
	}

	fmt.Printf("%x,%x\n", px, py)
}
