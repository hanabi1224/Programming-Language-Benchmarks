import io.ktor.application.*
import io.ktor.client.*
import io.ktor.client.call.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.http.*
import io.ktor.request.*
import io.ktor.response.*
import io.ktor.routing.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*
import kotlin.random.Random
import kotlinx.coroutines.*
import kotlinx.coroutines.Dispatchers.Default
import kotlinx.coroutines.channels.*
import kotlinx.serialization.*
import kotlinx.serialization.json.Json

val httpClient = HttpClient(CIO)

fun main(args: Array<String>) {
    val n = if (args.size > 0) args[0].toInt() else 10
    val port = Random.nextInt(30000, 40000)
    // println(port)
    val engine = runServer(port)
    val api = "http://localhost:$port/"
    var sum = 0
    runBlocking {
        val tasks = (1..n).map { i -> async(Dispatchers.Default) { sendRequest(api, i) } }
        tasks.forEach { t -> sum += t.await() }
    }
    println(sum)
    engine.stop(0, 0)
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

fun runServer(port: Int): ApplicationEngine {
    return embeddedServer(Netty, host = "localhost", port = port) {
                routing {
                    get("/") {
                        call.respondText("Hello from Kotlin Backend", ContentType.Text.Html)
                    }
                    post("/") {
                        val payloadText = call.receiveText()
                        var payload = Json.decodeFromString<Payload>(payloadText)
                        call.respondText(payload.value.toString())
                    }
                }
            }
            .start(wait = false)
}

@Serializable data class Payload(val value: Int)
