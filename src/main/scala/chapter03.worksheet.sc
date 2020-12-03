import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def concat[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil: List[A])(append)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((a, l) => Cons(f(a), l))
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
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

  /** returns a `List` consisting of all but the last element of a `List`
    *
    * @param l `List` to process
    * @return `List` of all l items except the last
    */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
    case _            => throw new UnsupportedOperationException
  }

  def foldRight[A, B](as: List[A], zero: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), zero)((b, a) => f(a, b))
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((l, r) => Cons(r, l))
  }

  def sum(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldLeft(as, 0)((z, _) => z + 1)

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil else Cons(as.head, List(as.tail: _*))
  }
}

import List._

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
tail(l)

/* Exercise 3.3

   Using the same idea, implement the function setHead for replacing the first
   first element of a List with a different value.
 */
setHead(2, l)

/* Exercise 3.4
 *
 * Generalize tail to the function drop, which removes the first n elements
 * from a list. Note that this function takes time proportional only to
 * the number of elements being dropped—we don’t need to make a copy of
 * the entire List.
 */
drop(l, 2)
drop(l, 3)
drop(l, 4)

/* Exercise 3.5

   Implement dropWhile, which removes elements from the List prefix as
   long as they match a predicate.
 */
dropWhile(l, (n: Int) => n <= 2)

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
foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

// Cons(1, foldRight(List(2, 3)))
// Cons(1, Cons(2, foldRight(List(3))))
// Cons(1, Cons(2, Cons(3, foldRight(Nil))))
// Cons(1, Cons(2, Cons(3, Nil)))

/* Exercise 3.9

   Compute the length of a list using foldRight.
 */
length(List(1, 2, 3, 4, 5))

/* Exercise 3.10

   Our implementation of foldRight is not tail-recursive and will result in a
   StackOver- flowError for large lists (we say it’s not stack-safe).
   Convince yourself that this is the case, and then write another general
   list-recursion function, foldLeft, that is tail-recursive, using the
   techniques we discussed in the previous chapter. Here is its signature

   def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B
 */
foldLeft(l, 1)(_ * _)
foldLeft(List("a", "b", "c"), "")(_.toUpperCase + _.toUpperCase)

/* Exercise 3.11

   Write sum, product, and a function to compute the length of a list
   using foldLeft.
 */

/* Exercise 3.12

   Write a function that returns the reverse of a list (given List(1,2,3) it
   returns List(3,2,1)). See if you can write it using a fold.
 */
foldLeft(l, Nil: List[Int])((x, y) => Cons(y, x))
reverse(l)

/* Exercise 3.13

   Hard: Can you write foldLeft in terms of foldRight? How about the other
   way around? Implementing foldRight via foldLeft is useful because it lets
   us implement foldRight tail-recursively, which means it works even for
   large lists without overflow- ing the stack.

   Yes, by reversing the list first
 */
assert(foldLeft(l, 0)(_ - _) == -6)
assert(foldRight(l, 0)(_ - _) == 2)

/* Exercise 3.14

   Implement append in terms of either foldLeft or foldRight.
 */
append(l, List(4, 5))
append(List("a", "b", "c"), List("1", "2", "3"))

/* Exercise 3.15
 *
 * Hard: Write a function that concatenates a list of lists into a single
 * list. Its runtime should be linear in the total length of all lists. Try
 * to use functions we have already defined.
 */
concat(List(List(1, 2), List(3, 4)))

/* Exercise 3.16

   Write a function that transforms a list of integers by adding 1 to each
   element. (Reminder: this should be a pure function that returns a new List!)
 */
val addOneToEach =
  foldRight(_: List[Int], Nil: List[Int])((a, b) => Cons(a + 1, b))

addOneToEach(List(3, 4, 5))

/* Exercise 3.17

   Write a function that turns each value in a List[Double] into a String. You
   can use the expression d.toString to convert some d: Double to a String.
 */
val doubleToString =
  foldRight(_: List[Double], Nil: List[String])((a, b) => Cons(a.toString, b))

doubleToString(List(3.14159, 2.71828))

/* Exercise 3.18
 *
 * Write a function map that generalizes modifying each element in a list
 * while maintain- ing the structure of the list. Here is its signature:12
 */
map(l)(_ / 2.0)

/* Exercise 3.19

   Write a function filter that removes elements from a list unless
   they satisfy a given predicate. Use it to remove all odd numbers
   from a List[Int].
 */
filter(l)(_ % 2 != 0)

/* Exercise 3.20

   Write a function flatMap that works like map except that the function
   given will return a list instead of a single result, and that list should
   be inserted into the final resulting list. Here is its signature:

     def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]

   For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
   List(1,1,2,2,3,3).
 */
flatMap(List(1, 2, 3))(i => List(i, i))

/* Exercise 3.21
 *
 * Use flatMap to implement filter.
 */
filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0)
