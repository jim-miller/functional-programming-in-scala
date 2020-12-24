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

}

case object None extends Option[Nothing]
case class Some[+A](value: A) extends Option[A]

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

