import scala.math._

object Week3 {
  def expectedNumberOfOccurences(k: Int, L: Int, n: Int): Double = (L - k + 1) * pow(.25, k) * n

  type DNA = Set[String]
  def motifEnumeration(dna: DNA, k: Int, d: Int): Set[String] = {
    for {
      singleDna <- dna
      i <- 0 until (singleDna.length - k)
      kMer = singleDna.substring(i, i + k)
      neighbour <- Week1.neighbours(kMer, d) if dna.forall(Week1.countApproxPattern(_, neighbour, d) > 0)
    } yield neighbour
  }
}
