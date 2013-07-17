object StreamSol {
  
  val primes = 2 #:: sieve(3)
  
  def sieve(n: Int): Stream[Int] = {
    if (primes.takeWhile(v => math.pow(v,2) <= n).exists( v => n % v == 0 )) sieve(n + 2)
    else n #:: sieve(n + 2)
  }
  
  println(primes.take(1000).sum)

}