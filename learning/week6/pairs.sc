object pairs {
  def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

  val n = 7
  val xss = (1 until n) map (i =>
    (1 until i) map (j => (i, j)))

  (xss foldRight Seq[(Int,Int)]())(_ ++ _)

  (1 until n) flatMap (i =>
    (1 until i) map (j => (i , j))) filter (pair =>
      isPrime(pair._1 + pair._2))

  for (i <- 1 to 5; j <- 1 to 3) yield (i,j)
}