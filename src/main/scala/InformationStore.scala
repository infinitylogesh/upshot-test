package example

import com.workday.montague.ccg._
import com.workday.montague.parser.{SemanticRepl, Else, ParserDict, SemanticParser}
import com.workday.montague.semantics._

object InfoStore extends SemanticRepl[CcgCat, Statement, Seq[Define]](InfoStoreParser) {
  def performAction(store: Seq[Define], action: Statement): (Seq[Define], String) = {
    action match {
      case definition@Define(_, _, _) =>
        (store :+ definition, "Ok")
      case Query(relation, subject) =>
        val candidates = store.filter(d => d.relation == relation && d.subject == subject)
          .map(_.predicate)
        val output = candidates.length match {
          case 0 => "I don't know"
          case 1 => candidates.head
          case _ => candidates.mkString("{", ", ", "}")
        }
        (store, output)
    }
  }

  def onParseError(line: String): Unit = println("I can't parse that")

  val initialModel: Seq[Define] = Seq()
}

object InfoStoreParser extends SemanticParser[CcgCat](InfoStoreLexicon.lexicon)

// Semantics
sealed trait Statement
case class Define(relation: String, subject: String, predicate: String) extends Statement
case class Query(relation: String, subject: String) extends Statement

// Syntax
case object Q extends TerminalCat { val category = "Q" } // syntactic category for question words

// Lexicon
object InfoStoreLexicon {
  val lexicon = ParserDict[CcgCat]() +
    (Seq("is", "are") -> relation("BE")) +
    (Seq("who", "what", "how", "where", "when") -> Q) +
    (Else -> Seq(N % 0.8, N/N % 0.1, X % 0.1))

  private def relation(relationType: String) = {
    Seq(
      // e.g. "Checkers is a dog"
      ((S\NP)/NP, λ {pred: String => λ {subject: String => Define(relationType, subject, pred)}}),
      // e.g. "Checkers is fluffy"
      ((S\NP)/(N/N), λ {pred: String => λ {subject: String => Define(relationType, subject, pred)}}),
      // e.g. "Who is Checkers?"
      ((S\Q)/NP, λ {subject: String => λ {question: String => Query(relationType, subject)}})
    )
  }
}
