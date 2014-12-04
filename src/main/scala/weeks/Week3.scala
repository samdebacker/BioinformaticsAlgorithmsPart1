package weeks

import breeze.linalg._

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

  def count(motifs: IndexedSeq[DNA]): DenseMatrix[Double] = {
    def toIndex(nucleotide: Char) = nucleotide match {
      case 'A' => 0
      case 'C' => 1
      case 'G' => 2
      case 'T' => 3
      case _ => ???
    }
    val k = motifs.head.length
    val m = DenseMatrix.zeros[Double](4, k)
    for (motif <- motifs; j <- 0 until motif.size) {
      val nucleotide = motif.charAt(j)
      m(toIndex(nucleotide),j) += 1
    }
    m
  }

  def profile(motifs: IndexedSeq[DNA]): DenseMatrix[Double] = {
    val t = motifs.length
    val c = count(motifs)
    c :/ t.toDouble
  }

  def consensus(motifs: IndexedSeq[DNA]): String = {
    def fromIndex(index: Int) = index match {
      case 0 => 'A'
      case 1 => 'C'
      case 2 => 'G'
      case 3 => 'T'
      case _ => ???
    }
    val p = profile(motifs)
    val k = motifs.head.length
    val s = new StringBuilder
    for (j <- 0 until k) {
      s.append(fromIndex(argmax(p(::,j))))
    }
    s.toString()
  }

  def entropy(v: Vector[Double]): Double = {
    def log2(x: Double): Double = log(x) / log(2)
    v.fold(0.0) { case (acc, el) =>
      acc - (if (el == 0.0) 0.0 else el * log2(el))
    }
  }

  def entropy(motifs: IndexedSeq[DNA]): Double = {
    val p = profile(motifs)
    val k = motifs.head.length
    (for {
      j <- 0 until k
    } yield entropy(p(::,j))).sum
  }
}
