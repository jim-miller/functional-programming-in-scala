import scala.annotation.tailrec
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, _) => Some(h())
  }

  def toList(): List[A] = this match {
    case Cons(h, t) => h() :: t().toList()
    case Empty      => Nil
  }

  def toListSafe(): List[A] = {
    @tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case _          => acc
    }

    loop(this, Nil).reverse
  }
}

case object Empty                                   extends Stream[Nothing]
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
}

/* Exercise 5.1

   Write a function to convert a Stream to a List, which will force its
   evaluation and let you look at it in the REPL. You can convert to the
   regular List type in the standard library. You can place this and other
   functions that operate on a Stream inside the Stream trait.

     def toList: List[A]
 */

val s = Stream((1 to 30000): _*)
s.toListSafe()
