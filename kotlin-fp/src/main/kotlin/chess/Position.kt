package chess

typealias Position = Pair<Int, Int>

fun toIndex(pos: Position): Int {
    return 8 * pos.first + pos.second
}

fun valid(pos: Position) : Boolean {
    val inRange = { x: Int -> x in 0..7 }
    val (f, r) = pos
    return inRange(f) && inRange(r)
}

fun fromString(str: String) : Position? {
    val regex = """([abcdefgh])(\d)""".toRegex()
    val matchResult = regex.find(str) ?: return null

    val (f, r) = matchResult.destructured
    val isValid = f[0] in 'a'..'h' && r.toInt() >= 1 && r.toInt() <= 8
    if (isValid)
        return Pair(r.toInt()-1, f[0].toInt() - 97)
    return null
}

fun main() {
    println(('a'..'h').toList()[0])
}

fun showPosition(pos: Position?): String {
    if (pos == null) return ""
    val (f, r) = pos
    return ('a'..'h').toList()[r] + "" + (r+1)
}
