package learning.week5
import math.Ordering

object MySort extends App {
  def msort[T](xs: List[T])(ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs,ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs,ys1)
      }
      val(fst, snd) = xs splitAt n
      merge(msort(fst)(ord), msort(snd)(ord))
    }
  }

  println(msort(List(5,2,1,4,5))(Ordering.Int))

  def concact[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys)(_ :: _)
  // foldLeft 会报错, :: 不是List[T]的方法
}
