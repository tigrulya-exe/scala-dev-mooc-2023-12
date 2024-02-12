package module2

import scala.language.{existentials, implicitConversions}

object higher_kinded_types{

  def tuple[A, B](a: List[A], b: List[B]): List[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    a.flatMap{ a => b.map((a, _))}

  def tuple[E, A, B](a: Either[E, A], b: Either[E, B]): Either[E, (A, B)] =
    a.flatMap{ a => b.map((a, _))}


  def tuplef[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] = {
    tupleBindable(fa, fb)
  }


  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)]  =
    fa.flatMap(a => fb.map((a, _)))

  implicit def optBindable[A](opt: Option[A]): Bindable[Option, A] =
    new Bindable[Option, A] {
      override def map[B](f: A => B): Option[B] = opt.map(f)

      override def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
    }

  implicit def listBindable[A](list: List[A]): Bindable[List, A] =
    new Bindable[List, A] {
      override def map[B](f: A => B): List[B] = list.map(f)

      override def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f)
    }

  implicit def eitherBindable[E, A](either: Either[E, A]): Bindable[({type l[B] = Either[_, B]})#l, A] =
    new Bindable[({type l[B] = Either[_, B]})#l, A] {

      override def map[B](f: A => B): Either[E, B] = either.map(f)

      override def flatMap[B](f: A => Either[_, B]): Either[_, B] = either.flatMap(f)
    }


  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val either1 = Right(3)
  val either2 = Right(4)

  lazy val r3 = println(tupleBindable(optBindable(optA), optBindable(optB)))
  lazy val r4 = println(tupleBindable(listBindable(list1), listBindable(list2)))

  println(tuplef(optA, optB))
  println(tuplef(list1, list2))
  println(tuplef(either1, either2))
}