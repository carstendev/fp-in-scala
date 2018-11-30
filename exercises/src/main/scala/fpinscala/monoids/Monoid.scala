package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    override def zero: A => A = a => a
  }
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldLeft(as)(List.empty[A])((acc, a) => acc :+ a)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as match {
      case x :+ xs =>
        foldRight(x)(f(xs, z))(f)
      case _ => z
    }

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as match {
      case x :: xs =>
        foldLeft(xs)(f(z, x))(f)
      case _ => z
    }

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = {
    as match {
      case x :: xs =>
        mb.op(f(x), foldMap(xs)(f)(mb))
      case _ => mb.zero
    }
  }
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as match {
      case x +: xs =>
        foldLeft(xs)(f(z, x))(f)
      case _ => z
    }


  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as match {
      case x :+ xs =>
        foldRight(x)(f(xs, z))(f)
      case _ => z
    }
  def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case x +: xs =>
        mb.op(f(x), foldMap(xs)(f)(mb))
      case _ => mb.zero
    }
}

object StreamFoldable extends Foldable[Stream] {
  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as match {
      case x #:: xs =>
        foldLeft(xs)(f(z, x))(f)
      case _ => z
    }


  def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as match {
      case x :+ xs =>
        foldRight(x)(f(xs, z))(f)
      case _ => z
    }

  def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case x #:: xs =>
        mb.op(f(x), foldMap(xs)(f)(mb))
      case _ => mb.zero
    }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Branch(l, r) =>
        mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
      case Leaf(v) => f(v)
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    as match {
      case Branch(l, r) =>
        val lB = foldLeft(l)(z)(f)
        foldLeft(r)(lB)(f)
      case Leaf(v) => f(z, v)
    }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    as match {
      case Branch(l, r) =>
        val rB = foldRight(r)(z)(f)
        foldRight(l)(rB)(f)

      case Leaf(v) => f(v, z)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Some(v) => f(v)
      case _ => mb.zero
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Some(v) => f(z, v)
      case _ => z
    }


  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Some(v) => f(v, z)
      case _ => z
    }
}

