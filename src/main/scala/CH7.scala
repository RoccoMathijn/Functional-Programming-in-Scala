import java.util.concurrent._

object CH7 {

  object Par {
    // Exercise 7.1
    type Par[A] = ExecutorService => Future[A]
    def unit[A] (a:  A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone: Boolean = true
      def get(timeout: Long, units: TimeUnit): A = get
      def isCancelled: Boolean = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A, B, C](a: Par[A], b: Par[B]) (f: (A,B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call: A = a(es).get
      })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def run[A] (s: ExecutorService) (a: Par[A]): Future[A] = a(s)

    def asyncf[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
        ps.foldRight(unit(List.empty[A]))((parA, acc) =>
          map2(parA, acc)(_ :: _)
      )
    }
  }
}
