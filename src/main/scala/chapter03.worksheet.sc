// object FpInScala {
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
// }

object List {
  def sum[A](ints: List[Int]): Int = ints match {
    case Nil        => 0
    case Cons(h, t) => h + sum(t)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil else Cons(as.head, List(as.tail: _*))
  }
}
/* Exercise 3.1

 * What will be the result of the following match expression?
 */
val x = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _)))          => x
  case Nil                                   => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t)                            => h + List.sum(t)
  case _                                     => 101
}

// 3 -- matches the third case as Cons(1, Cons(2, Cons(3, Cons(4, _))))

