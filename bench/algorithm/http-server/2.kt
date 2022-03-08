import io.jooby.body
import io.jooby.runApp
import io.ktor.client.*
import io.ktor.client.call.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.http.*
import kotlin.random.Random
import kotlin.system.exitProcess
import kotlinx.coroutines.*
import kotlinx.serialization.*
import kotlinx.serialization.json.Json

val httpClient = HttpClient(io.ktor.client.engine.cio.CIO)

fun main(args: Array<String>) {
    val n = if (args.isNotEmpty()) args[0].toInt() else 10
    val port = Random.nextInt(20000, 50000)
    runServer(port)
    val api = "http://localhost:$port/api"
    var sum = 0
    runBlocking {
        val tasks = (1..n).map { i -> async(Dispatchers.IO) { sendRequest(api, i) } }
        tasks.forEach { t -> sum += t.await() }
    }
    println(sum)
    exitProcess(0)
}

suspend fun sendRequest(api: String, value: Int): Int {
    while (true) {
        try {
            val response: HttpResponse =
                    httpClient.request(api) {
                        method = HttpMethod.Post
                        body = Json.encodeToString(Payload(value))
                    }
            return response.receive<Int>()
        } catch (e: Exception) {}
    }
}

fun runServer(port: Int) {
    runApp(emptyArray()) {
        serverOptions {
            host = null
            setPort(port)
            isHttp2 = false
            defaultHeaders = false
        }
        post("/api") {
            val payloadText = ctx.body.value()
            val payload = Json.decodeFromString<Payload>(payloadText)
            payload.value
        }
    }
}

@Serializable data class Payload(val value: Int)
