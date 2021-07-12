import java.io.File
import java.security.MessageDigest
import kotlinx.serialization.*
import kotlinx.serialization.json.*

fun main(args: Array<String>) {
    val fileName = if (args.size > 0) args[0] else "sample"
    val n = if (args.size > 1) args[1].toInt() else 3
    val file = File("$fileName.json")
    val jsonStr = file.readText()
    var indent = " "
    repeat(n) {
        val data = Json.parseToJsonElement(jsonStr)
        val format = Json {
            prettyPrint = true
            prettyPrintIndent = indent
        }
        indent += " "
        val jsonList = format.encodeToString(data)
        //        println(jsonList)
        val md5 = MessageDigest.getInstance("MD5")
        val hash = md5.digest(jsonList.toByteArray())
        println(hash.toHex())
    }
}

fun ByteArray.toHex() = joinToString(separator = "") { byte -> "%02x".format(byte) }
