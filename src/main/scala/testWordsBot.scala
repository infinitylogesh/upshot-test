
import com.workday.montague.ccg._
import com.workday.montague.parser._
import com.workday.montague.semantics.{λ, _}
import example.{Define, Query}

//case object Paren extends TerminalCat { val category = "Paren" } // syntactic category for parenthetical expressions

object testArithmetic1 extends SemanticParser[CcgCat](test.lexicon) {
  def parse(str: String): SemanticParseResult[CcgCat] = parse(str, tokenizer = parenTokenizer)

  // We need a custom tokenizer to separate parentheses from adjoining terms
  private def parenTokenizer(str: String) = {
    str.replace("(", " ( ").replace(")", " ) ").trim.toLowerCase.split("\\s+")
  }
}

//case object Q extends TerminalCat { val category = "Q" }

//case object T extends TerminalCat { val category = "T" }

case object Adj extends TerminalCat { val category = "Adj" }
case object Adv extends TerminalCat { val category = "Adv" }
case object N extends TerminalCat { val category = "Noun" }  // montague treats "N" and "NP" as synonyms, but we don't want this by default.
case object Num extends TerminalCat { val category = "#" }
case object Rel extends TerminalCat { val category = "Rel" }
case object V extends TerminalCat { val category = "V" }

//case class Events(eventType:String);
//case class listMy(e:Events,activity:String);

object test1 {

  implicit class StringImplicits(val str: String) extends AnyVal {
    // "blah".s = ["blah", "blahs"]
    def s: Seq[String] = Seq(str, str + "s")

    // "my" / ["thing", "stuff"] = ["my thing", "my stuff"]
    def /(nextWords: Seq[String]): Seq[String] = nextWords.map(s"$str " +)

    // "my" /?/ ["thing", "stuff"] = ["thing", "stuff", "my thing", "my stuff"]
    def /?/(nextWords: Seq[String]): Seq[String] = nextWords ++ nextWords.map(s"$str " +)
  }

  val lexicon =  ParserDict[CcgCat]() +
    (Seq("a", "an") -> Seq(
      (NP/N, λ {o: ObjectType => Choose(ObjectsInPlay(o))}),  // e.g. "a robot"
      (NP/NP, λ {c: Collection => Choose(c)}),  // e.g. "a robot you control"
      (Num, Form(Scalar(1)): SemanticState)  // e.g. "(draw) a card"
    )) +
    ("a player" -> (NP, Form(Choose(ObjectsInPlay(Kernel))): SemanticState)) +
    ("a tile" -> (NP, Form(Choose(AllTiles)): SemanticState)) +
    ("adjacent" -> (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(AdjacentTo(ThisRobot)))})) +
    ("adjacent to" -> ((NP/NP)\N, λ {o: ObjectType => λ {t: TargetObject => ObjectsMatchingConditions(o, Seq(AdjacentTo(t)))}})) +
    ("after attacking" -> (S\S, λ {a: Action => At(AfterAttack(ThisRobot), a)})) +
    (Seq("all", "each", "every") -> Seq( // Also see Seq("each", "every") below for definitions that DON'T apply to "all".
      (NP/N, λ {o: ObjectType => All(ObjectsInPlay(o))}),
      (NP/NP, λ {c: Collection => All(c)}),
      (NP/PP, λ {c: Collection => All(c)})
    )) +
    ("all" /?/ Seq("attributes", "stats") -> (N, Form(AllAttributes): SemanticState)) +
    ("and" -> (((S/PP)/V)\V, λ {a1: CurriedAction => λ {a2: CurriedAction => λ {t: TargetObject => And(a1.action(t), a2.action(t))}}})) +
    ("at" -> ((S/S)/NP, λ {t: Trigger => λ {a: Action => At(t, a)}})) +
    ("attacks" -> Seq(
      (S\NP, λ {c: Choose => AfterAttack(All(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterAttack(t)})
    )) +
    (Seq("beginning", "start") -> (NP/PP, λ {turn: Turn => BeginningOfTurn(turn.player)})) +
    ("by" -> (PP/Num, identity)) +
    (Seq("can move again", "gains a second move action") -> (S\NP, λ {t: TargetObject => CanMoveAgain(t)})) +
    ("can't attack" -> (S\NP, λ {t: TargetObject => ApplyEffect(t, CannotAttack)})) +
    ("can't be changed" -> (S\NP, λ {t: TargetAttribute => FreezeAttribute(t.target, t.attr)})) +
    (("card".s :+ "a card") -> Seq(
      (N, Form(AnyCard): SemanticState),
      (NP\Num, λ {num: Number => Cards(num)}),
      (NP/Adj, λ {num: Number => Cards(num)}),
      (NP, Form(CardsInHand(Self)): SemanticState),
      (NP/PP, λ {hand: Hand => CardsInHand(hand.player)}),
      (NP\N, λ {cardType: CardType => CardsInHand(Self, cardType)}),
      ((NP/PP)\N, λ {cardType: CardType => λ {hand: Hand => CardsInHand(hand.player, cardType)}})
    )) +
    ("control".s -> ((NP\N)\NP, λ {p: TargetPlayer => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(p)))}})) +
    (Seq("control a", "control an") ->
      ((S/NP)\NP,
        λ {p: TargetPlayer => λ {c: ObjectsMatchingConditions => CollectionExists(ObjectsMatchingConditions(c.objectType, c.conditions :+ ControlledBy(p)))}})
      ) +
    ("cost" -> Seq(
      (N, Form(Cost): SemanticState),
      ((S\NP)/Adv, λ {o: Operation => λ {cp: CardPlay => AttributeAdjustment(All(CardsInHand(cp.player, cp.cardType)), Cost, o)}})
    )) +
    ("damage" -> Seq(
      ((S/PP)\Num, λ {amount: Number => λ {t: Target => DealDamage(t, amount)}}),
      ((S\NP)\Num, λ {amount: Number => λ {t: Target => DealDamage(t, amount)}}),
      ((S/Adj)/PP, λ {t: Target => λ {amount: Number => DealDamage(t, amount)}}),
      ((S\NP)/Adj, λ {amount: Number => λ {t: Target => DealDamage(t, amount)}}),
      (S/PP, λ {t: Target => DealDamage(t, AttributeValue(ThisRobot, Attack))}),  // (by default, a robot deals damage equal to its power)
      (S\Num, λ {amount: Number => DealDamage(Choose(ObjectsInPlay(AllObjects)), amount)})  // (if no target is given, any target can be chosen)
    )) +
    (Seq("deal", "it deals", "takes") -> (X|X, identity)) +  // e.g. deals X damage, takes X damage
    ("destroy" -> (S/NP, λ {t: TargetObject => Destroy(t)})) +
    ("destroyed" -> Seq(
      (S\NP, λ {c: Choose => AfterDestroyed(All(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterDestroyed(t)})
    )) +
    ("draw" -> (S/NP, λ {c: Cards => Draw(Self, c.num)})) +
    ("discard" -> (S/NP, λ {t: TargetObject => Discard(t)})) +
    ("double" -> Seq(
      ((S/PP)/N, λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Multiply(Scalar(2)))}}),
      (V/N, λ {a: Attribute => CurriedAction({t: TargetObject => ModifyAttribute(t, a, Multiply(Scalar(2)))})})
    )) +
    (Seq("each", "every", "each player's", "every player's") -> Seq(
      (Adj, Form(AllPlayers): SemanticState),  // e.g. "each turn"
      (NP/PP, identity)  // e.g. "each of (your turns)"
    )) +
    ("end" -> (NP/PP, λ {turn: Turn => EndOfTurn(turn.player)})) +
    ("energy" -> Seq(
      (NP|Num, λ {amount: Number => Energy(amount)}),
      (NP/Adj, λ {amount: Number => Energy(amount)}),
      (S\S, λ {aa: AttributeAdjustment => AttributeAdjustment(aa.target, Cost, aa.operation)})  // "X costs Y more" == "X costs Y more energy"
    )) +
    ("equal" -> (Adj/PP, identity)) +
    (Seq("for each", "for every") -> (Adj/NP, λ {c: Collection => Count(c)})) +
    ("everything" -> (N, Form(AllObjects): SemanticState)) +
    ("everything adjacent to" -> (NP/NP, λ {t: TargetObject => All(ObjectsMatchingConditions(AllObjects, Seq(AdjacentTo(t))))})) +
    ("gain" -> Seq(  // gain X energy
      (S/NP, λ {e: Energy => ModifyEnergy(Self, Plus(e.amount))}),
      (S/NP, λ {l: Life => ModifyAttribute(All(ObjectsMatchingConditions(Kernel, Seq(ControlledBy(Self)))), Health, Plus(l.amount))})
    )) +
    ("gains" -> ((S/NP)\NP, λ {p: TargetPlayer => λ {e: Energy => ModifyEnergy(p, Plus(e.amount))}})) +  // Y gains X energy
    (Seq("gain", "gains") ->  // Y gain(s) X (attribute)
      (((S\NP)/N)/Num, λ {num: Number => λ {a: Attribute => λ {t: TargetObject => ModifyAttribute(t, a, Plus(num))}}})
      ) +
    ("give" -> (((S/N)/Adj)/NP, λ {t: TargetObject => λ {o: Operation => λ {a: Attribute => ModifyAttribute(t, a, o)}}})) +
    ("hand" -> (NP\Adj, λ {p: TargetPlayer => Hand(p)})) +
    ("halve" -> Seq(
      (((S/PP)/Adv)/N, λ {a: Attribute => λ {r: Rounding => λ {t: TargetObject => ModifyAttribute(t, a, Divide(Scalar(2), r))}}}),
      ((V/Adv)/N, λ {a: Attribute => λ {r: Rounding => CurriedAction({t: TargetObject => ModifyAttribute(t, a, Divide(Scalar(2), r))})}})
    )) +
    (Seq("has", "have") -> Seq(
      ((S/N)/Adj, λ {c: Comparison => λ {a: Attribute => AttributeComparison(a, c)}}),
      (((S\NP)/N)/Adj, λ {o: Operation => λ {a: Attribute => λ {t: TargetObject => AttributeAdjustment(t, a, o)}}})
    )) +
    (Seq("health", "life") -> Seq(
      (N, Form(Health): SemanticState),
      (NP|Num, λ {amount: Number => Life(amount)}),
      (NP/Adj, λ {amount: Number => Life(amount)})
    )) +
    ("if" -> ((S|S)|S, λ {c: GlobalCondition => λ {a: Action => If(c, a)}})) +
    (Seq("in", "of") -> (PP/NP, identity)) +
    ("in combat" -> (S\S, λ {t: AfterDestroyed => AfterDestroyed(t.target, Combat)})) +
    (Seq("in play", "on the board") -> (NP\N, λ {o: ObjectType => ObjectsInPlay(o)})) +
    ("is" -> (X|X, identity)) +
    ("it" -> (NP, Form(ItO): SemanticState)) +
    ("its" -> (Num/N, λ {a: Attribute => AttributeValue(ItO, a)})) +
    ("its controller" -> (NP, Form(ControllerOf(ItO)): SemanticState)) +
    (("kernel".s ++ "core".s) -> (N, Form(Kernel): SemanticState)) +
    ("less" -> (Adv\Num, λ {num: Number => Minus(num)})) +
    ("less than" -> (Adj/Num, λ {num: Number => LessThan(num)})) +
    ("more" -> (Adv\Num, λ {num: Number => Plus(num)})) +
    ("more than" -> (Adj/Num, λ {num: Number => GreaterThan(num)})) +
    ("must" -> (X/X, identity)) +
    ("number" -> Seq(
      (Num/PP, λ {c: Collection => Count(c)}),
      (Num/PP, λ {a: All => Count(a.collection)})
    )) +
    ("object".s -> (N, Form(AllObjects): SemanticState)) +
    (Seq("or", "and") -> ((N/N)\N, λ {o1: ObjectType => λ {o2: ObjectType => MultipleObjectTypes(Seq(o1, o2))}})) +
    ("or less" -> (Adj\Num, λ {num: Number => LessThanOrEqualTo(num)})) +
    ("or more" -> (Adj\Num, λ {num: Number => GreaterThanOrEqualTo(num)})) +
    ("play".s -> ((NP\N)\NP, λ {t: TargetPlayer => λ {c: CardType => CardPlay(t, c)}})) +
    (Seq("played", "comes into play", "enters the board") -> Seq(
      (S\NP, λ {c: Choose => AfterPlayed(All(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterPlayed(t)})
    )) +
    (Seq("power", "attack") -> (N, Form(Attack): SemanticState)) +
    ("reduce" -> (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetObject => λ {num: Number => ModifyAttribute(t, a, Minus(num))}}})) +
    (("robot".s ++ "creature".s) -> (N, Form(Robot): SemanticState)) +
    ("(rounded down)" -> (Adv, Form(RoundedDown): SemanticState)) +
    ("(rounded up)" -> (Adv, Form(RoundedUp): SemanticState)) +
    ("set" -> (((S/PP)/PP)/N, λ {a: Attribute => λ {t: TargetObject => λ {num: Number => SetAttribute(t, a, num)}}})) +
    ("speed" -> (N, Form(Speed): SemanticState)) +
    ("structure".s -> (N, Form(Structure): SemanticState)) +
    ("take control" -> (S/PP, λ {t: TargetObject => TakeControl(Self, t)})) +
    ("takes damage" -> Seq(
      (S\NP, λ {c: Choose => AfterDamageReceived(All(c.collection))}), // For this and other triggers, replace Choose targets w/ All targets.
      (S\NP, λ {t: TargetObject => AfterDamageReceived(t)})
    )) +
    ("to" -> Seq(
      (PP/NP, identity),
      (PP/Num, identity)
    )) +
    ("that" -> ((NP\N)/S, λ {c: Condition => λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(c))}})) +
    (Seq("that player", "they") -> (NP, Form(ItP): SemanticState)) +
    ("the" -> (X/X, identity)) +
    ("this" / Seq("robot", "creature", "structure") -> (NP, Form(ThisRobot): SemanticState)) +
    ("this" / Seq("robot's", "creature's", "structure's") -> (NP/N, λ {a: Attribute => TargetAttribute(ThisRobot, a)})) +
    ("total" -> Seq(
      ((Num/PP)/N, λ {a: Attribute => λ {c: Collection => AttributeSum(c, a)}}),
      ((Num/PP)/N, λ {a: Attribute => λ {all: All => AttributeSum(all.collection, a)}})
    )) +
    ("turn".s -> (NP\Adj, λ {p: TargetPlayer => Turn(p)})) +
    (Seq("when", "whenever") -> ((S/S)/S, λ {t: Trigger => λ {a: Action => At(t, a)}})) +
    (Seq("you", "yourself") -> (NP, Form(Self): SemanticState)) +
    ("your" -> Seq(
      (NP/N, λ {o: ObjectType => ObjectsMatchingConditions(o, Seq(ControlledBy(Self)))}),
      (NP/NP, λ {c: ObjectsMatchingConditions => All(ObjectsMatchingConditions(c.objectType, c.conditions :+ ControlledBy(Self)))}),
      (Adj, Form(Self): SemanticState)
    )) +
    ("your opponent" -> (NP, Form(Opponent): SemanticState)) +
    (IntegerMatcher -> (Num, {i: Int => Form(Scalar(i))}))


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

  def main(args: Array[String]): Unit = {
    val input =  "less 5" //args.mkString(" ")
    val result = testArithmetic.parse(input)
    //result.debugPrint();
    val output = result.bestParse.map(_.semantic.toString).getOrElse("(failed to parse)")

    println(s"Input: $input")
    println(s"Output: $output")
  }
}


