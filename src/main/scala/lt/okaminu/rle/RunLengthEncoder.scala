package lt.okaminu.rle

object RunLengthEncoder extends App {

    case class CharCount(count: Int, char: Char)

    println(encode(args(0)))

    def encode(str: String): String =
    str.foldLeft(Seq.empty[CharCount]){
            case (charCounts, char) if charCounts.nonEmpty && charCounts.last.char == char =>
                charCounts.updated(charCounts.size - 1, charCounts.last.copy(count = charCounts.last.count + 1))
            case (charCounts, char) if charCounts.isEmpty || charCounts.last.char != char =>
                charCounts :+ CharCount(1, char)
            case _ => Seq.empty[CharCount]
        }.foldLeft(""){(result, charCount) => s"$result${charCount.count}${charCount.char}"}
}
