object exercise {
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if(a > b) 1
    else f(a) * product(f)(a + 1, b)
  product(x => x * x)(3,4)

  def fact(n: Int) = product(x => x)(1, n)
  fact(5)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, init: Int)
               (a: Int, b: Int): Int =
  if(a > b) init
  else combine(f(a), mapReduce(f,combine, init)(a + 1, b))
}