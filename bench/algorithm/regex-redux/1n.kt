import kotlin.text.*
import kotlinx.cinterop.*
import platform.posix.*

fun main(args: Array<String>) {
    val fileName = if (args.size > 0) args[0] else "25000_in"
    var content = readAllText(fileName)
    val ilen = content.length
    content = Regex(">.*\n|\n").replace(content, "")
    val clen = content.length
    for(re in arrayOf(
        Regex("agggtaaa|tttaccct"),
        Regex("[cgt]gggtaaa|tttaccc[acg]"),
        Regex("a[act]ggtaaa|tttacc[agt]t"),
        Regex("ag[act]gtaaa|tttac[agt]ct"),
        Regex("agg[act]taaa|ttta[agt]cct"),
        Regex("aggg[acg]aaa|ttt[cgt]ccct"),
        Regex("agggt[cgt]aa|tt[acg]accct"),
        Regex("agggta[cgt]a|t[acg]taccct"),
        Regex("agggtaa[cgt]|[acg]ttaccct"),
    )) {
        val pattern = re.pattern
        val count = re.findAll(content).count()
        println("$pattern $count")
    }

    for ((p, r) in arrayOf(
        "tHa[Nt]" to "<4>", 
        "aND|caN|Ha[DS]|WaS" to "<3>",
        "a[NSt]|BY" to "<2>",
        "<[^>]*>" to "|", 
        "\\|[^|][^|]*\\|" to "-",
        )){
        content = Regex(p).replace(content, r)
    }

    println("\n$ilen\n$clen\n${content.length}")
}

@OptIn(ExperimentalForeignApi::class)
fun readAllText(filePath: String): String {
    val returnBuffer = StringBuilder()
    val file = fopen(filePath, "r")?: 
        throw IllegalArgumentException("Cannot open input file $filePath")

    try {
        memScoped {
            val readBufferLength = 64 * 1024
            val buffer = allocArray<ByteVar>(readBufferLength)
            var line = fgets(buffer, readBufferLength, file)?.toKString()
            while (line != null) {
                returnBuffer.append(line)
                line = fgets(buffer, readBufferLength, file)?.toKString()
            }
        }
    } finally {
        fclose(file)
    }

    return returnBuffer.toString()
}
