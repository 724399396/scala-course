import scala.io.Source

object x {
//  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords")
//  val words = in.getLines()
  val words = List("Scala", "is", "Fun")

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )

  val charCode: Map[Char, Char] =
  for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit

  def wordCode(word: String) =
  word.toUpperCase map charCode

  wordCode("Java")

  val wordFromNum:Map[String, Seq[String]] =
    words groupBy wordCode withDefaultValue Seq()

  def encode(numbers: String):Set[List[String]] =
    if(numbers.isEmpty) Set(List())
    else
      (for {
        split <- 1 to numbers.length
        word <- wordFromNum(numbers take split)
        rest <- encode(numbers drop split)
      } yield word :: rest).toSet

  encode("7225247386")

  def translate(numbers: String) =
    encode(numbers) map (_ mkString " ")

  translate("7225247386")
}