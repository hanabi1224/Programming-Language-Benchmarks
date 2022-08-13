import kotlin.text.*
import java.io.File

fun main(args: Array<String>) {
    val fileName = if (args.size > 0) args[0] else "25000_in"
    val file = File(fileName)
    var content = file.readText()
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
