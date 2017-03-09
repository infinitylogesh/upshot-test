
import com.workday.montague.ccg._
import com.workday.montague.parser._
import com.workday.montague.semantics.{λ, _}
import example.{Define, Query}

case object Paren extends TerminalCat { val category = "Paren" } // syntactic category for parenthetical expressions

object testArithmetic extends SemanticParser[CcgCat](test.lexicon) {
  def parse(str: String): SemanticParseResult[CcgCat] = parse(str, tokenizer = parenTokenizer)

  // We need a custom tokenizer to separate parentheses from adjoining terms
  private def parenTokenizer(str: String) = {
    str.replace("(", " ( ").replace(")", " ) ").trim.toLowerCase.split("\\s+")
  }
}

case object Q extends TerminalCat { val category = "Q" }

case object T extends TerminalCat { val category = "T" }

object test {
  val lexicon = ParserDict[CcgCat]() +
    //(Seq("are","gave","capital","is") -> relation("BE")) +
    (Seq("assigned")-> relation("b")) +
    (Seq("who", "what", "how", "where", "when") -> Q) +
    (Seq("tasks") -> T) +
    (Seq("are")->(((NP\Q)/NP),λ {pred: String =>λ{pre:String => Query(pred,pre)}})) +
    (Seq("all")->((NP/NP))) +
    (Seq("the")->Seq((NP/NP),(NP/T))) +
    (Seq("to","by")->(NP/NP)) +
    (Seq("me")->(NP)) +
    (Seq("overdue")->((NP/T),λ{pre:String => Query("test",pre)})) +
    //(Seq("are")->((((S\Q)\NP)\NP)/NP, λ {pred: String =>λ{ pre:String=>λ{ check:String=>λ {subject: String => Define(check+pre, subject, pred)}}}}))+
    /*(Seq("many") -> (NP)) +
    (Seq("now") -> (NP)) +
    (Seq("open")->(((S\Q)\NP)/NP, λ {pred:String => λ { subject:String => Define("test",subject, pred)} }))*/
    (Else -> Seq(N % 0.8, N/N % 0.1, X % 0.1))

  private def relation(relationType: String) = {
    Seq(
      // e.g. "Checkers is a dog"
      ((NP\T)/NP/*,λ{pre:String => Query("pred",pre)}*/)
    )
  }

  print("Check1")

  def main(args: Array[String]): Unit = {
    val input =  "overdue tasks" //args.mkString(" ")
    val result = testArithmetic.parse(input)
    result.debugPrint();
    val output = result.bestParse.map(_.semantic.toString).getOrElse("(failed to parse)")

    println(s"Input: $input")
    println(s"Output: $output")
  }
}
