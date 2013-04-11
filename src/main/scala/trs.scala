package com.github.hexx

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
  }

  type Term = Free[Term0, String]

  def f(fun: String, arg: List[Term]): Term = Suspend[Term0, String](Term0(fun, arg))

  def c(const: String): Term = f(const, List())

  def v(_var: String): Term = Return[Term0, String](_var)

  implicit val termShow: Show[Term] = new Show[Term] {
    override def shows(t: Term): String = t.resume match {
      case -\/(s) => s.shows
      case \/-(r) => s"$r"
    }
  }
}
