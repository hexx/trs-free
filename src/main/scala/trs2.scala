package com.github.hexx

import scala.util.parsing.combinator.RegexParsers

import scalaz._, Scalaz._
import Free._

package object trs2 {
  trait Patmat[F[_]] {
    def pzip[A, B](fa: F[A], fb: F[B]): Option[List[(A, B)]]

    def pzipWith[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): Option[List[C]] =
      pzip(fa, fb) map (_ map { case (a, b) => f(a, b) })

    def patmat[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => Option[List[C]]) =
      pzipWith(fa, fb)(f).map(_.sequence).flatten.map(_.flatten)
  }

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

    // implicit def term0Shows[A: Show]: Show[Term0[A]] = new Show[Term0[A]] {
    //   override def shows(t: Term0[A]): String = t match {
    //     case Term0(fun, arg) => s"""$fun(${arg.map(_.shows).mkString(", ")})"""
    //   }
    // }

    // implicit def term0Equal[A: Equal]: Equal[Term0[A]] = new Equal[Term0[A]] {
    //   def equal(t1: Term0[A], t2: Term0[A]) = (t1, t2) match {
    //     case (Term0(fun1, arg1), Term0(fun2, arg2)) => fun1 == fun2 && arg1 === arg2
    //   }
    // }

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

    implicit val term0Patmat: Patmat[Term0] = new Patmat[Term0] {
      def pzip[A, B](t1: Term0[A], t2: Term0[B]): Option[List[(A, B)]] = (t1, t2) match {
        case (Term0(fun1, arg1), Term0(fun2, arg2)) if fun1 == fun2 && arg1.length == arg2.length => Some(arg1 zip arg2)
        case _ => None
      }
    }

    // specific instance for Term[Term0, A] for avoiding diverging implicit expansion
    implicit def term0Show[A: Show]: Show[Term0[Term[Term0, A]]] = new Show[Term0[Term[Term0, A]]] {
      override def shows(t: Term0[Term[Term0, A]]): String = t match {
        case Term0(fun, arg) => s"""$fun(${arg.map(_.shows).mkString(", ")})"""
      }
    }

    implicit def term0Equal[A: Equal]: Equal[Term0[Term[Term0, A]]] = new Equal[Term0[Term[Term0, A]]] {
      def equal(t1: Term0[Term[Term0, A]], t2: Term0[Term[Term0, A]]) = (t1, t2) match {
        case (Term0(fun1, arg1), Term0(fun2, arg2)) => fun1 == fun2 && arg1 === arg2
      }
    }
  }

  type Term[F[+_], +A]  = Free[F, A]
  type Subst[F[+_], A] = List[(A, Term[F, A])]
  type Rule[F[+_], A]  = (Term[F, A], Term[F, A])

  def f[A](fun: String, arg: List[Term[Term0, A]]): Term[Term0, A] = Suspend[Term0, A](Term0(fun, arg))
  def c[A](const: String)                         : Term[Term0, A] = f(const, List())
  def v[A](_var: A)                               : Term[Term0, A] = Return[Term0, A](_var)

  def parseTerm(s: String) : Option[Term[Term0, String]]       = Parser.parseToOption(Parser.term, s)
  def parseRule(s: String) : Option[Rule[Term0, String]]       = Parser.parseToOption(Parser.rule, s)
  def parseRules(s: String): Option[List[Rule[Term0, String]]] = Parser.parseToOption(Parser.rules, s)

  def vars[F[+_]: Foldable, A](term: Term[F, A]) = term.toList

  def directSubterms[F[+_]: Foldable, A](term: Term[F, A]) = term.resume match {
    case -\/(s) => s.toList
    case \/-(r) => List()
  }

  def properSubterms[F[+_]: Foldable, A](term: Term[F, A]): List[Term[F, A]] = term.resume match {
    case -\/(s) => s.toList >>= (subterms(_))
    case \/-(r) => List()
  }

  def subterms[F[+_]: Foldable, A](term: Term[F, A]): List[Term[F, A]] = term :: properSubterms(term)

  def applySubst[F[+_]: Functor, A](subst: Subst[F, A], term: Term[F, A]) = term >>= (a => subst.toMap.get(a) | Return[F, A](a))

  def patmat[F[+_], A](t1: Term[F, A], t2: Term[F, A])(implicit F: Patmat[F]): Option[Subst[F, A]] = (t1.resume, t2.resume) match {
    case (\/-(r) , _      ) => Some(List(r -> t2))
    case (-\/(s1), -\/(s2)) => F.patmat(s1, s2)(patmat(_, _))
    case _                  => None
  }

  def rewriteTop1[F[+_]: Functor: Patmat, A](rule: Rule[F, A], term: Term[F, A]) = patmat(rule._1, term) map (applySubst(_, rule._2))

  def rewriteTop[F[+_]: Functor: Patmat, A](rules: List[Rule[F, A]], term: Term[F, A]) = rules flatMap (rewriteTop1(_, term))

  def rewriteStep[F[+_]: Traverse: Patmat, A: Equal](rules: List[Rule[F, A]], term: Term[F, A])(implicit F: Equal[F[Term[F, A]]]): List[Term[F, A]] = {
    def rewriteStep1[A](rules: List[Rule[F, A]], term: Term[F, A]): List[Term[F, A]] = rewriteTop(rules, term) ++ (term.resume match {
      case -\/(s) => s traverse (rewriteStep1(rules, _)) map (Suspend(_))
      case \/-(r) => List()
    })
    rewriteStep1(rules, term).filter(_ /== term)
  }

  def uniq[F[+_], A: Equal](terms: List[Term[F, A]])(implicit F: Equal[F[Term[F, A]]]): List[Term[F, A]] = {
    terms.foldLeft(List(): List[Term[F, A]]) { (l, t) =>
      if (l.find(_ === t).isEmpty) {
        t :: l
      } else {
        l
      }
    }
  }

  def rewriteToNF[F[+_]: Traverse: Patmat, A: Equal](rules: List[Rule[F, A]], term: Term[F, A])(implicit F: Equal[F[Term[F, A]]]): List[Term[F, A]] = {
    val step = rewriteStep(rules, term)
    if (step.isEmpty) {
      List(term)
    } else {
      uniq((step >>= (rewriteToNF(rules, _))))
    }
  }

  implicit def termShow[F[+_], A: Show](implicit F: Show[F[Term[F, A]]]): Show[Term[F, A]] = new Show[Term[F, A]] {
    override def shows(t: Term[F, A]): String = t.resume match {
      case -\/(s) => s.shows
      case \/-(r) => r.shows
    }
  }

  implicit def termEqual[F[+_], A: Equal](implicit F: Equal[F[Term[F, A]]]): Equal[Term[F, A]] = new Equal[Term[F, A]] {
    def equal(t1: Term[F, A], t2: Term[F, A]) = (t1.resume, t2.resume) match {
      case (\/-(r1), \/-(r2)) => r1 === r2
      case (-\/(s1), -\/(s2)) => s1 === s2
      case _                  => false
    }
  }

  implicit def termFoldable[F[+_]](implicit F: Foldable[F]): Foldable[({type λ[α] = Term[F, α]})#λ] =
    new Foldable[({type λ[α] = Term[F, α]})#λ] with Foldable.FromFoldMap[({type λ[α] = Term[F, α]})#λ] {
      def foldMap[A, B](t: Term[F, A])(f: A => B)(implicit G: Monoid[B]): B = t.resume match {
        case -\/(s) => s foldMap (_ foldMap f)
        case \/-(r) => f(r)
      }
    }

  implicit def termTraverse[F[+_]](implicit F: Traverse[F]): Traverse[({type λ[α] = Term[F, α]})#λ] =
    new Traverse[({type λ[α] = Term[F, α]})#λ] {
      def traverseImpl[G[+_], A, B](t: Term[F, A])(f: A => G[B])(implicit G: Applicative[G]): G[Term[F, B]] = t.resume match {
        case -\/(s) => (s traverse (_ traverse f)) map (Suspend(_))
        case \/-(r) => f(r) map (Return(_))
      }
    }

  object Parser extends RegexParsers {
    val funName = """[a-z]+""".r
    val varName = """[A-Z]+""".r

    def _var:  Parser[Term[Term0, String]]       = varName ^^ v
    def const: Parser[Term[Term0, String]]       = funName ^^ c
    def fun:   Parser[Term[Term0, String]]       = funName ~ "(" ~ repsep(term, ",") <~ ")" ^^ { case fun0 ~ _ ~ arg => f(fun0, arg) }
    def term:  Parser[Term[Term0, String]]       = fun | const | _var

    def rule:  Parser[Rule[Term0, String]]       = term ~ "->" ~ term ^^ { case lhs ~ _ ~ rhs => (lhs, rhs) }
    def rules: Parser[List[Rule[Term0, String]]] = rep(rule)

    def parseToOption[T](p: Parser[T], s: String): Option[T] = parseAll(p, s) match {
      case Success(r, _)   => Some(r)
      case NoSuccess(_, _) => None
    }
  }
}
