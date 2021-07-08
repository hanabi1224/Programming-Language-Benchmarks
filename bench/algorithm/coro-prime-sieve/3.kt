import kotlin.jvm.internal.iterator
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.*

fun main(args: Array<String>) {
    var n = if (args.size > 0) args[0].toInt() else 100
    var stream = generate().iterator()
    repeat(n) {
        val prime = stream.next()
        println(prime)
        stream = filter(stream, prime).iterator()
    }
}

fun generate(): Sequence<Int> = sequence {
    var i = 2
    while (true) {
        yield(i)
        i += 1
    }
}

fun filter(stream: Iterator<Int>, prime: Int): Sequence<Int> = sequence {
    while (true) {
        val i = stream.next()
        if (i % prime != 0) {
            yield(i)
        }
    }
}
