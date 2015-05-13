object Main extends App {
  val inputs = List(
    "bbaaabababbaabbaaaabbbababbaabbbaabbaaaaabbababaaaabaabbbaaa",
    "bbaaaababbbaababbbbabbabababababaaababbbbbbaabbaababaaaabaaa",
    "aaaababbabbaabbaabbbbbbabbbaaabbaabaabaabbbaabababbabbbbaabb",
    "abbabbbbbababaabaaababbbbaababbabbbabbbbaabbabbaaabbaabbbbbb"
  )
  val outputs = List(9, 12, 11, 15)

  def discrepancy(s: String): Int = Math.abs(s.count(_ == 'a') - s.count(_ == 'b'))

  def stepstring(s: String, start: Int, end: Int, step: Int): String = {
    s.slice(start, end)
      .grouped(step)
      .map(_.head)
      .mkString
  }

  def stepstrings(s: String): Seq[String] =
    (for {
      start <-  (0 to s.length)
      end   <-  (0 to s.length)
      step  <-  (1 to s.length)
    } yield stepstring(s, start, end, step)
    ).filterNot(_.length == 0)

  def maxDiscrepancy(s: String): Int = {
    stepstrings(s)
      .map(x => (x, discrepancy(x)))
      .sortBy((e) => e._2)
      .reverse
      .head
      ._2
  }

  val s = "bbaaabababbaabbaaaabbbababbaabbbaabbaaaaabbababaaaabaabbbaaa"
  println(maxDiscrepancy(Data.inputs.head))
}
