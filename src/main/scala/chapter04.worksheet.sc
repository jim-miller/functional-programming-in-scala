import scala.annotation.tailrec
import scala.{Option => _, Either => _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case _       => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case _       => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    if (this eq None) ob else this
  }

  def filter(f: A => Boolean): Option[A] = {
    if (map(f).getOrElse(false)) this else None
    // flatMap(a => if (f(a)) Some(a) else None)
  }
}

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None else Some(xs.sum / xs.size)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2.0))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      x <- a
      y <- b
    } yield f(x, y)
    // a.flatMap(x => b.map(y => f(x, y))) // Manually calling fMap/map
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(identity)

    // @tailrec
    // def loop(xs: List[Option[A]], acc: List[A]): Option[List[A]] = xs match {
    //   case Nil          => Some(acc)
    //   case None :: _    => None
    //   case Some(x) :: t => loop(t, acc :+ x)
    // }
    // loop(a, List.empty)

    // a.foldRight(Some(Nil): Option[List[A]])((x, y) => map2(x, y)(_ :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(Nil): Option[List[B]])((x, z) => map2(f(x), z)(_ :: _))
  }
}
case object None              extends Option[Nothing]
case class Some[+A](value: A) extends Option[A]

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a)    => Right(f(a))
      case e @ Left(_) => e
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a)    => f(a)
      case e @ Left(_) => e
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case r @ Right(_) => r
      case _            => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap(a => b.map(b => f(a, b)))
      // (this, b) match {
      //   case (x: Left[E], _) => x
      //   case (_, y: Left[E]) => y
      //   case (Right(a), Right(b)) => Right(f(a, b))
      // }
    }
}

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, z) => f(a).map2(z)(_ :: _))
  }
}

case class Left[+E](value: E)  extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

// format: off
/* Exercise 4.1

   Implement all of the preceding functions on Option. As you implement each
   function, try to th8ink about what it means and in what situations you'd use
   it. We'll explore when to use each of these functions next. Here are a few
   hints for solving this exercise:

     * It's fine to use pattern matching, though you should be able to implement
       all the functions besides map and getOrElse without resorting to pattern
       matching.
     * For map and flatMap, the type signature should be enough to determine
       the implementation.
     * getOrElse returns the result inside the Some case of the Option, or if
       the Option is None, returns the given default value.
     * orElse returns the first Option if it's defined; otherwise, it returns
       the second Option.
 */
// format: on

val noneInt: Option[Int] = None
val someInt: Option[Int] = Some(42)

assert(noneInt.map(_ * 2) == None)
assert(noneInt.flatMap(n => None) == None)
assert(noneInt.getOrElse(2) == 2)
assert(noneInt.orElse(Some(42)) == Some(42))
assert(noneInt.filter(_ > 3) == None)

assert(someInt.map(_ * 2) == Some(84))
assert(someInt.flatMap { n: Int => Some(n + 3) } == Some(45))
assert(someInt.getOrElse(13) == 42)
assert(someInt.orElse(Some(3)) == Some(42))
assert(someInt.filter(_ < 3) == None)
assert(someInt.filter(_ > 3) == Some(42))

/* Exercise 4.2

   Implement the variance function in terms of flatMap. If the mean of a
   sequence is m, the variance is the mean of math.pow(x - m, 2) for each
   element x in the sequence. See the definition of vairance on Wikipedia.

     def variance(xs: Seq[Double]): Option[Double]
 */
val vRes = Option.variance(Seq(2.0, 4.0, 6.0, 8.0, 10.0))
assert(vRes == Some(8.0))

/* Exercise 4.3

   Write a generic function map2 that combines two Option values using a binary
   function. If either Option value is None, then the return value is too. Here
   is the signature:

     def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]
 */
val f = (n: Int, s: String) => s"$n is $s"
assert(Option.map2(Some(2), None)(f) == None)

val map2Res = Option.map2(Some(42), Some("the answer"))(f)
assert(map2Res == Some("42 is the answer"))

/* Exercise 4.4

    Write a function sequence that combines a list of Options into one Option
    containing a list of all the values in the original list. If the original
    list contains None even once, the result of the function should be None
    otherwise the result should be Some with a list of all the values. Here
    is its signature:

      def sequence[A](a: List[Option[A]]): Option[List[A]]}
 */
val optSeqResA = Option.sequence(List(None, Some(2), Some(3), None))
val optSeqResB = Option.sequence(List(Some(1), Some(2), Some(3)))
val emptySeq   = Option.sequence(Nil: List[Option[Int]])

assert(optSeqResA == None)
assert(optSeqResB == Some(List(1, 2, 3)))
assert(emptySeq == Some(Nil))

/* Exercise 4.5

   Implement this function (traverse). It's straightforward to do using map and
   sequence, but try for a more efficient implementation that only looks at the
   list once. In fact, implement sequence in terms of traverse.

     def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]
 */
val listOfInts  = List(1, 2, 3)
val intToDouble = (n: Int) => Some(n.toDouble)

// straightforward, less efficient
val traverseAsSeqenceMap = Option.sequence(listOfInts.map(n => intToDouble(n)))
val traverseRes          = Option.traverse(listOfInts)(intToDouble)
assert(traverseRes == traverseAsSeqenceMap)

import scala.util.{Failure, Success, Try}
val divLifeBy: Int => Option[Int] = n =>
  Try(42 / n) match {
    case Success(d) => Some(d)
    case _          => None
  }
assert(Option.traverse(List(100, 0))(divLifeBy) == None)

/* Exercise 4.6

   Implement versions of map, flatMap, orElse, and map2 on Either
   that operate on the Right value
 */
val times3 = (n: Int) => n * 3
assert(Right(2).map(times3) == Right(6))
assert(Left("foobar").map(times3) == Left("foobar"))

assert(Right(3).flatMap(times3.andThen(Right(_))) == Right(9))

assert(Right(42).orElse(Right(0)) == Right(42))
assert(Left("foo").orElse(Right(6)) == Right(6))

val aPlusB = (a: Int, b: Int) => s"Ints: $a, $b"
val aEither: Either[String, Int] = Right(2)
val bEither: Either[String, Int] = Right(3)
assert(aEither.map2(bEither)(aPlusB) == Right("Ints: 2, 3"))
// map2 is a for comprehension akk sequence of map/flatMap operations
aEither.flatMap(a => bEither.map(b => aPlusB(a, b)))
aEither.map2(Left("foo"))(aPlusB)

/* Exercise 4.7

   Implement sequence and traverse for Either. These should return the first
   error that's encountered, if there is one.
*/
val eList: Either[String, List[Int]] = Either.sequence(List(Right(1), Right(2), Right(3)))
val eListError = Either.sequence(List(Left("foo"), Right(6), Left("bar")))
val eStrInt = Either.traverse(List(1, 2, 3))(n => if (n > 1) Left("error") else Right(n + 1))
val eStrBool = Either.traverse(List(1, 2, 3))(n => Right(n % 2 == 0): Either[String, Boolean])

assert(eList == Right(List(1, 2, 3)))
assert(eListError == Left("foo"))
assert(eStrInt == Left("error"))
assert(eStrBool == Right(List(false, true, false)))

/* Exercise 4.8

   In this implementation, map2 is only able to report one error, even if both
   the name and the age are invalid. What would you need to change in order to
   report bother errors? Would you change map2 or the signature of mkPerson? Or
   could you create a new type that captures this requirement better than
   Either does, with some additional structure? How would orElse, traverse,
   and sequence behave differently for that data type?

   map2 relies on sequential operations (flatMap/for) and has a data type that
   only holds a single error value. You would need a new data type (i.e non-empty
   list/chain). Flatmapping over a left value would have to be replaced with a
   function that appended that list of erorrs rather than returning itself when
   using functions like sequence/traverse.
*/
