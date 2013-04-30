package com.github.hexx

import scala.util.parsing.combinator.RegexParsers

import scalaz._, Scalaz._
import Free._

package object trs {
  case class Term0[+A](fun: String, arg: List[A]) {
    def patmat[B, C](t1: Term0[B])(f: (A, B) => Option[List[C]]): Option[List[C]] = t1 match {
      case Term0(fun1, arg1) if fun == fun1 && arg.length == arg1.length =>
        implicitly[Zip[List]].zipWith(arg, arg1)(f).sequence map (_.flatten)
      case _ => None
    }
  }

  object Term0 {
    implicit val term0Functor: Functor[Term0] = new Functor[Term0] {
      def map[A, B](t: Term0[A])(f: A => B): Term0[B] = t match {
        case Term0(fun, arg) => Term0(fun, arg map f)
      }
    }

    implicit def term0Shows[A: Show]: Show[Term0[A]] = new Show[Term0[A]] {
      override def shows(t: Term0[A]): String = t match {
        case Term0(fun, arg) => s"""$fun(${arg.map(_.shows).mkString(", ")})"""
      }
    }

    implicit def term0Equal[A: Equal]: Equal[Term0[A]] = new Equal[Term0[A]] {
      def equal(t1: Term0[A], t2: Term0[A]) = (t1, t2) match {
        case (Term0(fun1, arg1), Term0(fun2, arg2)) => fun1 == fun2 && arg1 === arg2
      }
    }

    implicit val term0Foldable: Foldable[Term0] = new Foldable[Term0] with Foldable.FromFoldMap[Term0] {
      def foldMap[A, B](t: Term0[A])(f: A => B)(implicit F: Monoid[B]): B = t match {
        case Term0(fun, arg) => arg foldMap f
      }
    }

    implicit val term0Traverse: Traverse[Term0] = new Traverse[Term0] {
      def traverseImpl[F[+_], A, B](t: Term0[A])(f: A => F[B])(implicit F: Applicative[F]): F[Term0[B]] = t match {
        case Term0(fun, arg) => (arg traverse f) map (Term0(fun, _))
      }
    }
  }

  type Term[A]  = Free[Term0, A]
  type Subst[A] = List[(A, Term[A])]
  type Rule[A]  = (Term[A], Term[A])

  def f[A](fun: String, arg: List[Term[A]]): Term[A] = Suspend[Term0, A](Term0(fun, arg))
  def c[A](const: String)                  : Term[A] = f(const, List())
  def v[A](_var: A)                        : Term[A] = Return[Term0, A](_var)

  def parseTerm(s: String) : Option[Term[String]]       = Parser.parseToOption(Parser.term, s)
  def parseRule(s: String) : Option[Rule[String]]       = Parser.parseToOption(Parser.rule, s)
  def parseRules(s: String): Option[List[Rule[String]]] = Parser.parseToOption(Parser.rules, s)

  def vars[A](term: Term[A]) = term.toList

  def directSubterms[A](term: Term[A]) = term.resume match {
    case -\/(s) => s.toList
    case \/-(r) => List()
  }

  def properSubterms[A](term: Term[A]): List[Term[A]] = term.resume match {
    case -\/(s) => s.toList >>= subterms
    case \/-(r) => List()
  }

  def subterms[A](term: Term[A]): List[Term[A]] = term :: properSubterms(term)

  def patmat[A](t1: Term[A], t2: Term[A]): Option[Subst[A]] = (t1.resume, t2.resume) match {
    case (\/-(r) , _      ) => Some(List(r -> t2))
    case (-\/(s1), -\/(s2)) => s1.patmat(s2)(patmat)
    case _                  => None
  }

  def applySubst[A](subst: Subst[A], term: Term[A]) = term >>= (a => subst.toMap.get(a) | v(a))

  def rewriteTop1[A](rule: Rule[A], term: Term[A]) = patmat(rule._1, term) map (applySubst(_, rule._2))

  def rewriteTop[A](rules: List[Rule[A]], term: Term[A]) = rules flatMap (rewriteTop1(_, term))

  def rewriteStep[A: Equal](rules: List[Rule[A]], term: Term[A]): List[Term[A]] = {
    def rewriteStep1[A](rules: List[Rule[A]], term: Term[A]): List[Term[A]] = rewriteTop(rules, term) ++ (term.resume match {
      case -\/(s) => s traverse (rewriteStep1(rules, _)) map (Suspend(_))
      case \/-(r) => List()
    })
    rewriteStep1(rules, term).filter(_ /== term)
  }

  def uniq[A: Equal](terms: List[Term[A]]): List[Term[A]] = {
    terms.foldLeft(List(): List[Term[A]]) { (l, t) =>
      if (l.find(_ === t).isEmpty) {
        t :: l
      } else {
        l
      }
    }
  }

  def rewriteToNF[A: Equal](rules: List[Rule[A]], term: Term[A]): List[Term[A]] = {
    val step = rewriteStep(rules, term)
    if (step.isEmpty) {
      List(term)
    } else {
      uniq(step >>= (rewriteToNF(rules, _)))
    }
  }

  implicit def termShow[A: Show]: Show[Term[A]] = new Show[Term[A]] {
    override def shows(t: Term[A]): String = t.resume match {
      case -\/(s) => s.shows
      case \/-(r) => r.shows
    }
  }

  implicit def termEqual[A: Equal]: Equal[Term[A]] = new Equal[Term[A]] {
    def equal(t1: Term[A], t2: Term[A]) = (t1.resume, t2.resume) match {
      case (\/-(r1), \/-(r2)) => r1 === r2
      case (-\/(s1), -\/(s2)) => s1 === s2
      case _                  => false
    }
  }

  implicit val term0Foldable: Foldable[Term] = new Foldable[Term] with Foldable.FromFoldMap[Term] {
    def foldMap[A, B](t: Term[A])(f: A => B)(implicit F: Monoid[B]): B = t.resume match {
      case -\/(s) => s foldMap (_ foldMap f)
      case \/-(r) => f(r)
    }
  }

  implicit val termTraverse: Traverse[Term] = new Traverse[Term] {
    def traverseImpl[F[+_], A, B](t: Term[A])(f: A => F[B])(implicit F: Applicative[F]): F[Term[B]] = t.resume match {
      case -\/(s) => (s traverse (_ traverse f)) map (Suspend(_))
      case \/-(r) => f(r) map (Return(_))
    }
  }

  object Parser extends RegexParsers {
    val funName = """[a-z]+""".r
    val varName = """[A-Z]+""".r

    def _var:  Parser[Term[String]]       = varName ^^ v
    def const: Parser[Term[String]]       = funName ^^ c
    def fun:   Parser[Term[String]]       = funName ~ "(" ~ repsep(term, ",") <~ ")" ^^ { case fun0 ~ _ ~ arg => f(fun0, arg) }
    def term:  Parser[Term[String]]       = fun | const | _var

    def rule:  Parser[Rule[String]]       = term ~ "->" ~ term ^^ { case lhs ~ _ ~ rhs => (lhs, rhs) }
    def rules: Parser[List[Rule[String]]] = rep(rule)

    def parseToOption[T](p: Parser[T], s: String): Option[T] = parseAll(p, s) match {
      case Success(r, _)   => Some(r)
      case NoSuccess(_, _) => None
    }
  }
}
