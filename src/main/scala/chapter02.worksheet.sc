import scala.annotation.tailrec
/* Exercise 2.1

   Write a recursive function to get the nth Fibonacci number
   The first two Fibonacci numbers are 0 and 1. The nth number is always the
   sum of the previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your
   definition should use a local tail-recursive function.
 */
def fib(n: Int): Int = {

  @tailrec
  def loop(n: Int, prev: Int, next: Int): Int = {
    if (n == 0) {
      prev
    } else {
      loop(n - 1, next, prev + next)
    }

  }
  loop(n, 0, 1)
}

assert(fib(8) == 21)

val fibScan: LazyList[BigInt] = BigInt(0) #:: fibScan.scanLeft(BigInt(1))(_ + _)

(0 to 10).map(fib)
(0 to 10).map(fibScan)

/* Exercise 2.2

   Implement isSorted, which checks whether an Array[A] is sorted according
   to a given comparison function
 */
def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  if (as.size <= 1) true else as.sliding(2).forall(a => ordered(a(0), a(1)))
}

assert(isSorted(Array(3), (x: Int, y: Int) => y > x))
assert(isSorted(Array(4, 3, 1), (x: Int, y: Int) => y < x))

/* Exercise 2.3

   Let’s look at another example, currying, which converts a function f of
   two arguments into a function of one argument that partially applies f.
   Here again there’s only one implementation that compiles. Write this
   implementation.
 */
def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
  (a: A) => (b: B) => f(a, b)
  // (a: A) => f(a, _)
}
val sum = (x: Int, y: Int) => x + y
val curried = curry(sum)
sum(1, 2)
curried(1)(2)
sum.curried(1)(2)
sum.tupled((1, 2))

/* Exercise 2.4

   Implement uncurry, which reverses the transformation of curry. Note
   that since => associates to the right, A => (B => C) can be written
   as A => B => C.
*/
def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
  (a: A, b: B) => f(a)(b)
}

/* Exercise 2.5

 * Implement the higher-order function that composes two functions.
*/
def compose[A,B,C](f: B => C, g: A => B): A => C = {
  (a: A) => f(g(a))
  // g andThen f
  // f compose g
}
