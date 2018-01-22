object CH5 {

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Empty => List.empty
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = (this, n) match {
      case (Empty, _) => Empty
      case (_, 0) => Empty
      case (Cons(h, t), x) => Cons(h, () => t().take(x - 1))
    }


    def drop(n: Int): Stream[A] = (this, n) match {
      case (Empty, _) => Empty
      case (stream, 0) => stream
      case (Cons(_, t), x) => t().drop(x - 1)
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }

    def forAll(p: A => Boolean): Boolean =  this match {
      case Cons(h, t) if p(h()) => t().forAll(p)
      case Cons(_,_) => false
      case _ => true
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = t1
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

}
