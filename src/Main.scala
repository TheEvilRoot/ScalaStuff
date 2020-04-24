import scala.annotation.tailrec

object Main {

  @tailrec
  def size[A](in: List[A], length: Int = 0): Int = in match {
    case Nil => length
    case _ :: rest => size(rest, length + 1)
  }

  @tailrec
  def at[A](in: List[A], index: Int, current: Int = 0): Option[A] = in match {
    case a :: _ if index == current => Option(a)
    case _ :: rest => at(rest, index, current + 1)
    case Nil => Option.empty
  }

  @tailrec
  def find[A](in: List[A], value: A, index: Int = 0): Int = in match {
    case first :: _ if first == value => index
    case _ :: rest => find(rest, value, index + 1)
    case Nil => -1
  }

  @tailrec
  def count[A](in: List[A], value: A, c: Int = 0): Int = in match {
    case a :: rest if a == value => count(rest, value, c + 1)
    case _ :: rest => count(rest, value, c)
    case Nil => c
  }

  @tailrec
  def sub[A](in: List[A], from: Int, to: Int, index: Int = 0, range: List[A] = List()): List[A] = in match {
    case a :: rest if index >= from && index < to => sub(rest, from, to, index + 1, range.appended(a))
    case _ :: rest => sub(rest, from , to, index + 1, range)
    case Nil => range
  }

  @tailrec
  def remove[A](in: List[A], value: A, ret: List[A] = List()): List[A] = in match {
    case a :: rest if a == value => remove(rest, value, ret)
    case a :: rest => remove(rest, value, ret.appended(a))
    case Nil => ret
  }

  def foldr[A, B](f: (A, B) => B, a: B, c: List[A]): B = c match {
    case Nil => a
    case r :: rest => f(r, foldr(f, a, rest))
  }

  @tailrec
  def foldl[A, B](f: (B, A) => B, a: B, c: List[A]): B = c match {
    case Nil => a
    case r :: rest => foldl(f, f(a, r), rest)
  }

  def main(args: Array[String]): Unit = {
    println(foldr[Int, Float]((x, y) => x / y, 2f, List(8, 12, 24, 4)))
    println(foldl[Int, Float]((x, y) => x / y, 64f, List(4, 2, 4)))
  }

}
