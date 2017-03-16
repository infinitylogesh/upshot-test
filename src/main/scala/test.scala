
import java.beans.Statement

import com.workday.montague.ccg._
import com.workday.montague.parser._
import com.workday.montague.semantics.{λ, _}
import example.{Define, Query}
import io.Source.stdin

case object Paren extends TerminalCat { val category = "Paren" } // syntactic category for parenthetical expressions
//case object V extends TerminalCat { val category = "V" }

//TODO : Make a REPL out of this , Reading the lexicon and outputing the parsed output.



object testArithmetic extends SemanticParser[CcgCat](test.lexicon) {
  def parse(str: String): SemanticParseResult[CcgCat] = parse(str, tokenizer = parenTokenizer)

  // We need a custom tokenizer to separate parentheses from adjoining terms
  private def parenTokenizer(str: String) = {
    str.replace("(", " ( ").replace(")", " ) ").trim.toLowerCase.split("\\s+")
  }
}

case object Q extends TerminalCat { val category = "Q" }

case object T extends TerminalCat { val category = "T" }

sealed trait Statement;
case class Events(eventType:String) extends Statement;
case class listMy(e:Events,activity:String) extends Statement;

object test {
  val lexicon = ParserDict[CcgCat]() +
    (Seq("events") -> (NP,Form(Events("All")):SemanticState)) +
    (Seq("conferences") -> (NP,Form(Events("conference")):SemanticState)) +
    (Seq("that") -> ((NP\NP),identity)) +
    (Seq("iam","i am") ->((S/V)\NP,λ { n3:Events  => λ { n2:String => listMy(n3,n2)}})) +
    (Seq("participating","participant","a participant") ->(V,Form("participant"):SemanticState)) +
    (Seq("organizing","a organizer") ->(V,Form("organizer"):SemanticState)) +
    (Seq("what") ->(Q,identity)) +
    (Seq("are") ->(NP\Q,identity)) +
    (Seq("all") ->(NP\NP,identity)) +
    (Seq("the") ->((NP/NP)\NP,identity)) +
    (Seq("my") ->(NP/NP,λ { n3:Events  => listMy(n3,"All")}))

  //(Seq("are","gave","capital","is") -> relation("BE")) +
  /*(Seq("assigned")->((((S\NP)\T)/NP),λ{ pre:String=>λ{ check:String=>λ {subject: String => Define(check, subject, pre)}}}))   +
  (Seq("who", "what", "how", "where", "when") -> Q) +
  (Seq("tasks","task") -> T) +
  (Seq("are")->Seq(((NP\Q)/NP),(NP\Q))) +
  (Seq("all","for")->Seq((NP/NP),(NP\NP))) +
  (Seq("on")->((NP/NP)/*,λ{ pred:String => Form(pred) }*/)) +
  (Seq("the")->Seq((NP/NP),(NP/T),(NP\NP))) +
  (Seq("to","by")->(NP/NP)) +
  (Seq("me")->(NP,Form("1"):SemanticState)) +
  (Seq("overdue")->Seq(((NP/T),λ{pre:String => Query("test",pre)}),((S\T),λ{pre:String => Query("test",pre)}))) +
  (Seq("efforts")->(NP)) +
  (Seq("spent")->((((S\NP)\NP)/NP),λ{bun:String =>λ{pre:String => Define(bun,"pred",pre)}})) +
  (Seq("loves")->(((S\NP)/NP),λ{bun:String =>λ{pre:String => Define(bun,"luv",pre)}})) +
  //(Seq("are")->((((S\Q)\NP)\NP)/NP, λ {pred: String =>λ{ pre:String=>λ{ check:String=>λ {subject: String => Define(check+pre, subject, pred)}}}}))+
  /*(Seq("many") -> (NP)) +
  (Seq("now") -> (NP)) +
  (Seq("open")->(((S\Q)\NP)/NP, λ {pred:String => λ { subject:String => Define("test",subject, pred)} }))*/
  (Else -> Seq(N % 0.8, N/N % 0.1, X % 0.1))*/

  private def relation(relationType: String) = {
    Seq(
      // e.g. "Checkers is a dog"
      ((NP\T)/NP/*,λ{pre:String => Query("pred",pre)}*/)
    )
  }

  def main(args: Array[String]): Unit = {
    print(">> ")
    for(line <- stdin.getLines()){
      val output = testArithmetic.parse(line).bestParse;
      println(output);
      print(">> ");
    }
  }

  /*def main(args: Array[String]): Unit = {
    val input =  "events that i am a participant" //args.mkString(" ")
    val result = testArithmetic.parse(input)
    //result.debugPrint();
    val output = result.bestParse.map(_.semantic.toString).getOrElse("(failed to parse)")

    println(s"Input: $input")
    println(s"Output: $output")
  }*/
}
