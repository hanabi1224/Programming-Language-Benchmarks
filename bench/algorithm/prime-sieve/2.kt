// Multiple thread version by add withContext(Default) 
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.*
import kotlinx.coroutines.Dispatchers.Default

fun main(args: Array<String>) =
        runBlocking {
            withContext(Default) {
                var n = if (args.size > 0) args[0].toInt() else 100
                var ch = generate()
                repeat(n) {
                    val prime = ch.receive()
                    println(prime)
                    val chNext = filter(ch, prime)
                    ch = chNext
                }
                coroutineContext.cancelChildren()
            }
        }

fun CoroutineScope.generate(): ReceiveChannel<Int> =
        produce {
            var i = 2
            while (true) {
                send(i++)
            }
        }

fun CoroutineScope.filter(channelIn: ReceiveChannel<Int>, prime: Int): ReceiveChannel<Int> =
        produce {
            for (i in channelIn) {
                if (i % prime != 0) {
                    send(i)
                }
            }
        }
