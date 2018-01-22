object CH3 {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if(as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
    }

    def foldLeft[A,B](as: List[A], z:B)(f: (B, A) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }

    def foldLeft2[A,B](as: List[A], z:B)(f: (B, A) => B): B =
      foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    def concatenate[A](as: List[A], bs: List[A]) =
      foldRight(as, bs)((a, b) => Cons(a, b))

    def addOne(as: List[Int]): List[Int] =
      foldRight(as, Nil: List[Int])((a, b) => Cons(a + 1, b))

    def convertToString(as: List[Double]): List[String] =
      foldRight(as, Nil: List[String])((a, b) => Cons(a.toString, b))

    def map[A,B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      as match {
        case Nil => Nil
        case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
      }
    }

    def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
      foldRight(as, Nil: List[A])((a, b) => if(f(a)) Cons(a, b) else b)
    }

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRight(as, Nil: List[B])((a, b) => concatenate(f(a), b))

    def filterFlatmap[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)(a => if(f(a)) Cons(a, Nil) else Nil )

    def sumLists(as: List[Int], bs: List[Int]): List[Int] =
      (as, bs) match {
        case (_, Nil) => Nil
        case (Nil, _) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, sumLists(t1, t2))
      }

    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
      (as, bs) match {
        case (_, Nil) => Nil
        case (Nil, _) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
      }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      def startsWith(sup: List[A], sub: List[A]): Boolean =
        (sup, sub) match {
          case (_, Nil) => true
          case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
          case _ => false
        }

      sup match {
        case Nil => sub == Nil
        case _ if startsWith(sup, sub) => true
        case Cons(_, t) => hasSubsequence(t, sub)
      }

    }

  }



  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def maximum(tree: Tree[Int]): Int = {
      tree match {
        case Leaf(v) => v
        case Branch(l, r) => maximum(l).max(maximum(r))
      }
    }

    def size[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(l,r) => size(l) + size(r)
      }
    }

    def depth[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 0
        case Branch(l,r) => 1 + (depth(l) max depth(  r))
      }
    }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
      tree match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l,r) => Branch(map(l)(f), map(r)(f))
      }
    }

    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
      tree match {
        case Leaf(v) => f(v)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      }
    }

    def maximumWithFold(tree: Tree[Int]): Int = {
      fold(tree)(a => a)(_ max _)
    }

    def sizeWithFold(tree: Tree[Int]): Int = {
      fold(tree)(a => 1)(1+ _ + _)
    }

    def depthWithFold[A](tree: Tree[A]): Int = {
      fold(tree)(a => 0)((a, b) => 1 + a max b)
    }

    def mapWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
      fold(tree)(a => Leaf(f(a)): Tree[B])((a, b) => Branch(a, b))
    }
  }
}
