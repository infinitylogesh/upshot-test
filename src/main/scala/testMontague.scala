
import com.workday.montague.ccg._
import com.workday.montague.parser._
import com.workday.montague.semantics.{λ, _}
import example.{Define, Query}

case object Parens extends TerminalCat { val category = "Paren" } // syntactic category for parenthetical expressions

object ArithmeticParser extends SemanticParser[CcgCat](testMontague.lexicon) {
  def parse(str: String): SemanticParseResult[CcgCat] = parse(str, tokenizer = parenTokenizer)

  // We need a custom tokenizer to separate parentheses from adjoining terms
  private def parenTokenizer(str: String) = {
    str.replace("(", " ( ").replace(")", " ) ").trim.toLowerCase.split("\\s+")
  }
}

case object Q1 extends TerminalCat { val category = "Q" }

object testMontague {
  val lexicon = ParserDict[CcgCat]() +
    (Seq("are","gave","capital","is") -> relation("BE")) +
    (Seq("who", "what", "how", "where", "when") -> Q1) +
    (Seq("many tasks are") -> (NP)) +
    (Seq("many tasks") -> (NP)) +
    (Seq("open")->((((S\Q1))/NP)/NP, λ {pred:String => λ { subject:String => λ {noun:String => Define(noun, subject, pred)}  }}))
    (Else -> Seq(N % 0.8, N/N % 0.1, X % 0.1))

  private def relation(relationType: String) = {
    Seq(
      // e.g. "Checkers is a dog"
      ((S\NP)/NP, λ {pred: String => λ {subject: String => Define(relationType, subject, pred)}}),
      // e.g. "Checkers is fluffy"
      ((S\NP)/(N/N), λ {pred: String => λ {subject: String => Define(relationType, subject, pred)}}),
      // e.g. "Who is Checkers?"
      ((S\Q1)/NP, λ {subject: String => λ {question: String => Query(relationType, subject)}})
        )
  }

    print("Check1")

  def main(args: Array[String]): Unit = {
    val input =  "how many tasks are open in prime" //args.mkString(" ")
    val result = ArithmeticParser.parse(input)
    val output = result.bestParse.map(_.semantic.toString).getOrElse("(failed to parse)")

    println(s"Input: $input")
    println(s"Output: $output")
  }
}
