import com.github.hexx.trs2._

import scalaz._, Scalaz._

object Example {
  lazy val addMultRules = parseRules("""
    add(z,     X) -> X
    add(s(X),  Y) -> s(add(X, Y))
    mult(z,    X) -> z
    mult(s(X), Y) -> add(Y, mult(X, Y))
  """).get

  lazy val term = parseTerm("mult(s(s(z)), s(s(s(z))))").get

  lazy val six = rewriteToNF(addMultRules, term).apply(0).shows
}
