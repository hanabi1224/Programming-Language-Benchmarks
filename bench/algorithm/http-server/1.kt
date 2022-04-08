import io.ktor.client.*
import io.ktor.client.call.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.server.application.*
import io.ktor.server.cio.*
import io.ktor.server.engine.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.util.*
import kotlin.random.Random
import kotlinx.coroutines.*
import kotlinx.serialization.*
import kotlinx.serialization.json.Json

val httpClient = HttpClient(io.ktor.client.engine.cio.CIO)

fun main(args: Array<String>) {
    val n = if (args.size > 0) args[0].toInt() else 10
    val port = Random.nextInt(20000, 50000)
    val engine = runServer(port)
    val api = "http://localhost:$port/api"
    var sum = 0
    runBlocking {
        val tasks = (1..n).map { i -> async(Dispatchers.IO) { sendRequest(api, i) } }
        tasks.forEach { t -> sum += t.await() }
    }
    println(sum)
    // exitProcess(0)
    engine.stop(0, 0)
}

@OptIn(InternalAPI::class)
suspend fun sendRequest(api: String, value: Int): Int {
    while (true) {
        try {
            val response: HttpResponse =
                    httpClient.post(api) {
                        body = Json.encodeToString(Payload(value))
                    }
            return response.body<Int>()
        } catch (e: Exception) {}
    }
}

fun runServer(port: Int): ApplicationEngine {
    return embeddedServer(CIO, host = "localhost", port = port) {
                routing {
                    post("/api") {
                        val payloadText = call.receiveText()
                        var payload = Json.decodeFromString<Payload>(payloadText)
                        call.respondText(payload.value.toString())
                    }
                }
            }
            .start(wait = false)
}

@Serializable data class Payload(val value: Int)
