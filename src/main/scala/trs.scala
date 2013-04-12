package com.github.hexx

import scala.util.parsing.combinator.RegexParsers

import scalaz._, Scalaz._
import Free._

package object trs {
  case class Term0[+A](fun: String, arg: List[A])

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

  type Term[+A] = Free[Term0, A]

  def f[A](fun: String, arg: List[Term[A]]): Term[A] = Suspend[Term0, A](Term0(fun, arg))

  def c[A](const: String): Term[A] = f(const, List())

  def v[A](_var: A): Term[A] = Return[Term0, A](_var)

  implicit def termShow[A]: Show[Term[A]] = new Show[Term[A]] {
    override def shows(t: Term[A]): String = t.resume match {
      case -\/(s) => s.shows
      case \/-(r) => s"$r"
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
    val funName = """[a-z]""".r
    val varName = """[A-Z]""".r

    def _var:  Parser[Term[String]] = varName ^^ (v(_))
    def const: Parser[Term[String]] = funName ^^ (c(_))
    def fun:   Parser[Term[String]] = funName ~ "(" ~ repsep(term, ",") <~ ")" ^^ { case fun0 ~ _ ~ arg => f(fun0, arg) }
    def term:  Parser[Term[String]] = fun | const | _var
  }
}
