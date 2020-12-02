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

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(h, t) => t
    case _          => throw new UnsupportedOperationException
  }

  /** Replaces first element of a `List` with a different value.
    *
    * This does not work for empty (`Nil`) `List`s and will throw an
    * `UnsupportedOperationException` if attempted
    *
    * @param a value to take place of the first element
    * @param l `List` to operate on
    * @return the new `List` with replaced first element
    */
  def setHead[A](a: A, l: List[A]): List[A] = l match {
    case Cons(_, t) => Cons(a, t)
    case _          => throw new UnsupportedOperationException
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

/* Exercise 3.2

   Implement the function tail for removing the first element of a List. Note
   that the function takes constant time. What are different choices you could
   make in your implementation if the List is Nil? We’ll return to this
   question in the next chapter.
 */
val l: List[Int] = Cons(1, Cons(2, Nil))
List.tail(l)

/* Exercise 3.3

   Using the same idea, implement the function setHead for replacing the first
   first element of a List with a different value.
 */
List.setHead(2, l)

