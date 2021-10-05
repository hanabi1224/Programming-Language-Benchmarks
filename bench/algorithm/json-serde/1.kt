import java.io.File
import java.security.MessageDigest
import kotlinx.serialization.*
import kotlinx.serialization.json.*

fun main(args: Array<String>) {
    val fileName = if (args.size > 0) args[0] else "sample"
    val n = if (args.size > 1) args[1].toInt() else 10
    val file = File("$fileName.json")
    val jsonStr = file.readText()
    val data = Json.parseToJsonElement(jsonStr)
    printHash(data)
    val array = buildJsonArray { repeat(n) { add(Json.parseToJsonElement(jsonStr)) } }
    printHash(array)
}

fun printHash(data: JsonElement) {
    val format = Json { prettyPrint = false }
    val str = format.encodeToString(data)
    // println(str)
    val md5 = MessageDigest.getInstance("MD5")
    val hash = md5.digest(str.toByteArray())
    println(hash.toHex())
}

fun ByteArray.toHex() = joinToString(separator = "") { byte -> "%02x".format(byte) }
