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
    case Cons(_, _) => drop(l, 1)
    case _          => throw new UnsupportedOperationException
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(h, t) => if (n > 1) drop(t, n - 1) else t
    case _          => Nil
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil             => Nil
    case as @ Cons(h, t) => if (f(h)) dropWhile(t, f) else as
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

  /**
    * returns a `List` consisting of all but the last element of a `List`
    *
    * @param l `List` to process
    * @return `List` of all l items except the last
    */
  def init[A](l: List[A]): List[A] = {
    def loop(acc: List[A], remain: List[A]): List[A] = remain match {
      case Cons(_, Nil) | Nil => acc
      case Cons(h, t)         => loop(Cons(h, acc), t)
    }

    def reverse(as: List[A], res: List[A] = Nil): List[A] = as match {
      case Nil => res
      case Cons(h, t) => reverse(t, Cons(h, res))
    }

    reverse(loop(Nil, l))
  }

  def foldRight[A, B](as: List[A], zero: B)(f: (A, B) => B): B = as match {
    case Nil => zero
    case Cons(h, t) => f(h, foldRight(t, zero)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

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
val l: List[Int] = List(1, 2, 3)
List.tail(l)

/* Exercise 3.3

   Using the same idea, implement the function setHead for replacing the first
   first element of a List with a different value.
 */
List.setHead(2, l)

/* Exercise 3.4
 *
 * Generalize tail to the function drop, which removes the first n elements
 * from a list. Note that this function takes time proportional only to
 * the number of elements being dropped—we don’t need to make a copy of
 * the entire List.
 */
List.drop(l, 2)
List.drop(l, 3)
List.drop(l, 4)

/* Exercise 3.5

   Implement dropWhile, which removes elements from the List prefix as
   long as they match a predicate.
 */
List.dropWhile(l, (n: Int) => n <= 2)

/* Exercise 3.6

   Not everything works out so nicely. Implement a function, init, that returns
   a List consisting of all but the last element of a List. So, given
   List(1,2,3,4), init will return List(1,2,3). Why can’t this function be
   implemented in constant time like tail?
 */
assert(List.init(List(1, 2, 3)) == List(1, 2))

/* Exercise 3.7

   Can product, implemented using foldRight, immediately halt the recursion
   and return 0.0 if it encounters a 0.0? Why or why not? Consider how any
   short-circuiting might work if you call foldRight with a large list. This
   is a deeper question that we’ll return to in chapter 5.

   No. Doing so requires another "bailout" parameter. When encountered,
   we could
   return Nil. E.g.

     case Cons(h, t) => if (h == bailout) bailout else f(h, foldRight(...))
 */

/* Exercise 3.8

   See what happens when you pass Nil and Cons themselves to foldRight, like
   this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).10 What do you
   think this says about the relationship between foldRight and the data
   constructors of List?
 */
List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

// Cons(1, foldRight(List(2, 3)))
// Cons(1, Cons(2, foldRight(List(3))))
// Cons(1, Cons(2, Cons(3, foldRight(Nil))))
// Cons(1, Cons(2, Cons(3, Nil)))
