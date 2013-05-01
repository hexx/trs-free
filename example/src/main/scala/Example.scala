import com.github.hexx.trs2._

import scala.util.parsing.combinator.RegexParsers

import scalaz._, Scalaz._

object Example {
  lazy val addMultRules = parseRules("""
    add(0,     X) -> X
    add(s(X),  Y) -> s(add(X, Y))
    mult(0,    X) -> 0
    mult(s(X), Y) -> add(Y, mult(X, Y))
  """).get

  lazy val mult2by3 = parseTerm(s"mult(${n(2)}, ${n(3)})").get

  lazy val six = toInt(rewriteToNF(addMultRules, mult2by3).apply(0).shows).get

  lazy val mergeSortRules = parseRules("""
    leq(0,    X)    -> true
    leq(s(X), 0)    -> false
    leq(s(X), s(Y)) -> leq(X, Y)

    split(nil)                  -> app(nil, nil)
    split(cons(X, nil))         -> app(cons(X, nil), nil)
    split(cons(X, cons(Y, XS))) -> split1(X, Y, split(XS))

    split1(X, Y, app(XS, YS)) -> app(cons(X, XS), cons(Y, YS))

    merge(nil,         XS)          -> XS
    merge(XS,          nil)         -> XS
    merge(cons(X, XS), cons(Y, YS)) -> ifmerge(leq(X, Y), X, Y, XS, YS)

    ifmerge(true,  X, Y, XS, YS) -> cons(X, merge(XS, cons(Y, YS)))
    ifmerge(false, X, Y, XS, YS) -> cons(Y, merge(cons(X, XS), YS))

    mergesort(nil)                   -> nil
    mergesort(cons(X, nil))          -> cons(X, nil)
    mergesort(cons(X, cons(Y, XS)))  -> mergesort1(split(cons(X, cons(Y, XS))))

    mergesort1(app(XS, YS)) -> merge(mergesort(XS), mergesort(YS))
  """).get

  object NaturalNumberParser extends OptionParsers {
    def zero: Parser[Int] = "0" ^^ (_.toInt)
    def succ: Parser[Int] = "s(" ~> nat <~ ")" ^^ (_ + 1)
    def nat: Parser[Int]  =  succ | zero

    def nil: Parser[List[Int]]  = "nil" ^^ (_ => Nil)
    def cons: Parser[List[Int]] = "cons(" ~> nat ~ "," ~ list <~ ")" ^^ { case x ~ _ ~ xs => (x :: xs) }
    def list                    = cons | nil
  }

  def toInt(n: String): Option[Int] = NaturalNumberParser.parseToOption(NaturalNumberParser.nat, n)

  def n(i: Int): String = if (i == 0) "0" else s"s(${n(i - 1)})"

  def toTermList(l: List[Int]): String = l match {
    case Nil     => "nil"
    case x :: xs => s"cons(${n(x)}, ${toTermList(xs)})"
  }

  def toList(l: String): Option[List[Int]] = NaturalNumberParser.parseToOption(NaturalNumberParser.list, l)

  lazy val unsorted = parseTerm(s"mergesort(${toTermList(List(2, 1, 3))})").get

  lazy val sorted = toList(rewriteToNF(mergeSortRules, unsorted).apply(0).shows).get
}
