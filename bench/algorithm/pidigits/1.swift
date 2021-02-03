
import BigInt

let arguments = CommandLine.arguments
let digitsToPrint = arguments.count > 1 ? Int(arguments[1])! : 27
var digitsPrinted = 0

var k = BigInt(1)
var n1 = BigInt(4)
var n2 = BigInt(3)
var d = BigInt(1)
var u: BigInt
var v: BigInt
var w: BigInt
while true {
    u = n1 / d
    v = n2 / d
    if u == v {
        print(u, terminator:"")
        digitsPrinted += 1
        let digitsPrintedModTen = digitsPrinted % 10
        if digitsPrintedModTen == 0 {
            print("\t:\(digitsPrinted)")
        }

        if digitsPrinted >= digitsToPrint {
            if digitsPrintedModTen > 0 {
                for _ in 0..<(10 - digitsPrintedModTen) {
                    print(" ", terminator:"")
                }
                print("\t:\(digitsPrinted)")
            }

            break
        }

        let toMinus = u * 10 * d;
            n1 = n1 * 10 - toMinus;
            n2 = n2 * 10 - toMinus;
    } else {
        let k2 = k * 2
        u = n1 * (k2 - 1)
        v = n2 * 2
        w = n1 * (k - 1)
        n1 = u + v
        u = n2 * (k + 2)
        n2 = w + u
        d = d * (k2 + 1)
        k = k + 1
    }
}
