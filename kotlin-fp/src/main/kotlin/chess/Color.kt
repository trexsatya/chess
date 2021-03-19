package chess

enum class Color {
    BLACK, WHITE;
}

fun other(thisOne: Color): Color {
    if (thisOne == Color.BLACK) return Color.WHITE
    return Color.BLACK
}

//fun main() {
//    print(other(Color.WHITE))
//}