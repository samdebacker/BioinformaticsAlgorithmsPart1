package weeks

import breeze.linalg._

import scala.annotation.tailrec
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

  def toIndex(nucleotide: Char) = nucleotide match {
    case 'A' => 0
    case 'C' => 1
    case 'G' => 2
    case 'T' => 3
    case _ => ???
  }

  def count(motifs: IndexedSeq[DNA]): DenseMatrix[Double] = {
    val k = motifs.head.length
    val m = DenseMatrix.zeros[Double](4, k)
    for (motif <- motifs; j <- 0 until motif.size) {
      val nucleotide = motif.charAt(j)
      m(toIndex(nucleotide),j) += 1
    }
    m
  }

  type Profile = DenseMatrix[Double]

  def profile(motifs: IndexedSeq[DNA], addCount: Int = 0): Profile = {
    val t = motifs.length
    val c = count(motifs) :+ (addCount.toDouble)
    c :/ t.toDouble
  }

  def fromIndex(index: Int) = index match {
    case 0 => 'A'
    case 1 => 'C'
    case 2 => 'G'
    case 3 => 'T'
    case _ => ???
  }

  def consensi(p: Profile): Set[String] = {
    @tailrec def consensi_(p: Profile, result: Set[String]): Set[String] = {
      if (p.cols == 0) result
      else {
        val m = breeze.linalg.max(p(::,0))
        val s = for {
          r <- result
          i <- 0 to 3
          if p(i,0) == m
        } yield r + fromIndex(i)
        consensi_(p(::,1 until p.cols), s)
      }
    }
    consensi_(p, Set("")) - ""
  }

  def consensus(motifs: IndexedSeq[DNA]): String = {
    val p = profile(motifs)
    val k = motifs.head.length
    val s = new StringBuilder
    for (j <- 0 until k) {
      s.append(fromIndex(argmax(p(::,j))))
    }
    s.toString()
  }

  def score(motifs: IndexedSeq[DNA]): Int = {
    val c = consensus(motifs)
    val k = motifs.head.length
    (for {
      motif <- motifs
      el <- motif.zip(c)
      if el._1 != el._2
    } yield 1).sum
  }

  def entropy(motifs: IndexedSeq[DNA]): Double = {
    def entropy(v: Vector[Double]): Double = {
      def log2(x: Double): Double = log(x) / log(2)
      v.fold(0.0) { (acc, el) =>
        acc - (if (el == 0.0) 0.0 else el * log2(el))
      }
    }
    val p = profile(motifs)
    val k = motifs.head.length
    (for {
      j <- 0 until k
    } yield entropy(p(::,j))).sum
  }

  def distance(pattern: String, dna: IndexedSeq[DNA]): Int = {
    val k = pattern.length
    dna.map { d =>
      (for {
         i <- 0 to (d.length - k)
         kMer = d.substring(i, i + k)
      } yield Week1.hammingDistance(pattern, kMer)).min
    }.sum
  }

  def bruteForceMedianString(dna: IndexedSeq[DNA], k: Int, all: Boolean = false): Set[String] = {
    val DPs = for {
      i <- 0 until pow(4, k).toInt
      pattern = Week1.numberToPattern(i, k)
      d = distance(pattern, dna)
    } yield (d, pattern)
    val minDistance = DPs.min(Ordering[Int].on[(Int,_)](_._1))._1
    if (all)
      DPs.filter(_._1 == minDistance).map(_._2).toSet
    else
      DPs.find(_._1 == minDistance).map(_._2).toSet
  }

  def probability(text: String, profile: Profile): Double = {
    @tailrec def probability_(text: String, profile: Profile, result: Double): Double = {
      if (text.isEmpty || profile.cols == 0)
        result
      else
        probability_(text.tail, profile(::,1 until profile.cols), result * profile(toIndex(text.head),0))
    }
    probability_(text, profile, 1.0)
  }

  def kMers(text: DNA, k: Int): IndexedSeq[DNA] = {
    @tailrec def kMers_(text: String, result: IndexedSeq[DNA]): IndexedSeq[DNA] = {
      if (text.length < k)
        result
      else
        kMers_(text.tail, result :+ text.take(k))
    }
    kMers_(text, IndexedSeq.empty[DNA])
  }

  def profileMostProbableKmer(text: String, k: Int, profile: Profile): String = {
    kMers(text, k).map { kMer =>
      (probability(kMer, profile), kMer)
    }.maxBy(_._1)._2
    // CAREFUL maxBy uses GT comparison, while max uses GTEQ comparison! and the maxBy looks nicer!
  }

  // http://www.mrgraeme.co.uk/greedy-motif-search/
  def greedyMotifSearch(dna: IndexedSeq[DNA], k: Int, t: Int, addCount: Int = 0): IndexedSeq[String] = {
    val allMotifs = kMers(dna.head, k).map { kMer =>
      val motifs = dna.tail.foldLeft(IndexedSeq(kMer)) { (acc, text) =>
        acc :+ profileMostProbableKmer(text, k, profile(acc, addCount))
      }
      (motifs, score(motifs))
    }
    allMotifs.minBy(_._2)._1
    // CAREFUL minBy uses LT comparison, while min uses LTEQ comparison! and the minBy looks nicer!
    //allMotifs.min(Ordering[Int].on[(IndexedSeq[String],Int)](_._2))._1
  }
}
