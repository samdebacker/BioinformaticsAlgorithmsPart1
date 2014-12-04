package weeks

import scala.math._

object Week3 {
  /**
   * @param k the length of the k-mer
   * @param n the length of each DNA string
   * @param t the number of DNA strings
   * @return
   */
  def expectedNumberOfOccurences(k: Int, n: Int, t: Int): Double = (n - k + 1) * pow(.25, k) * t

  type DNA = String
  def motifEnumeration(dna: Set[DNA], k: Int, d: Int): Set[String] = {
    for {
      singleDna <- dna
      i <- 0 until (singleDna.length - k)
      kMer = singleDna.substring(i, i + k)
      neighbour <- Week1.neighbours(kMer, d) if dna.forall(Week1.countApproxPattern(_, neighbour, d) > 0)
    } yield neighbour
  }
}
