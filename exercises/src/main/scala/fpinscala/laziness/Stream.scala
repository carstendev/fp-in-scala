package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => cons[A](h(), t().take(n - 1))
      case _ => Empty
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if n > 0 => Some(h(), t().takeViaUnfold(n - 1))
      case _ => None
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) =>
        lazy val head = h()
        if (p(head)) cons[A](head, t().takeWhile(p)) else Empty
      case _ => Empty
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) =>
        lazy val head = h()
        if (p(head)) Some(head, t().takeWhile(p)) else None
      case _ => None
    }
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A]) { (a, b) =>
      if (p(a)) cons[A](a, b) else b
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def headOption: Option[A] = {
    foldRight[Option[A]](None) { (a, _) =>
      Some(a)
    }
  }

  def zipWithViaUnfold[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold(this, b) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B]) { (a, b) =>
      cons(f(a), b)
    }
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A]) { (a, b) =>
      if (p(a)) cons(a, b) else b
    }
  }

  def append[B >: A](other: => Stream[B]): Stream[B] = {
    foldRight(other) { (a, b) =>
      cons[B](a, b)
    }
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B]) { (a, b) =>
      f(a).append(b)
    }
  }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def onesViaUnfold: Stream[Int] = {
    unfold(1)(s => Some(s, s))
  }

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(aa => Some(aa, aa))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(s => Some(s, s + 1))
  }

  def fib: Stream[Int] = {
    def fib(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, fib(f1, f0 + f1))
    }

    fib(0, 1)
  }

  def fibViaUnfold: Stream[Int] = {
    unfold((0, 1))(e => Some(e._1, (e._2, e._1 + e._2)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, zz)) => cons(a, unfold(zz)(f))
      case None => empty
    }
  }
}