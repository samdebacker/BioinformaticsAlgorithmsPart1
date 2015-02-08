/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014 www.iReact.io
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package io.ireact.bioinformatics.part1

import support.{ DNAString, DNAMotif }
import breeze.linalg._

import scala.annotation.tailrec
import scala.math._
import scala.util.Random

object Chapter3 {
  /**
   * @param k the length of the k-mer
   * @param n the length of each DNA string
   * @param t the number of DNA strings
   * @return
   */
  def expectedNumberOfOccurences(k: Int, n: Int, t: Int): Double = (n - k + 1) * pow(.25, k) * t

  type DNA = String
  def motifEnumeration(dna: DNAMotif, k: Int, d: Int): Set[DNAString] = {
    (for {
      singleDna ← dna.value
      singleDnaString = singleDna.value
      i ← 0 until (singleDnaString.length - k)
      kMer = singleDnaString.substring(i, i + k)
      neighbour ← Chapter1.neighbours(kMer, d) if dna.value.forall(ds ⇒ Chapter1.countApproxPattern(ds.value, neighbour, d) > 0)
    } yield DNAString.from(neighbour).get).toSet
  }

  private val FROM_NUCLEOTIDE_TO_INDEX_CONVERTION = Map('A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)
  def toIndex(nucleotide: Char): Int = FROM_NUCLEOTIDE_TO_INDEX_CONVERTION(nucleotide)

  def count(motifs: DNAMotif): DenseMatrix[Double] = {
    val k = motifs.value.head.value.length
    val m = DenseMatrix.zeros[Double](4, k)
    for (motif ← motifs.value; j ← 0 until motif.value.size) {
      val nucleotide = motif.value.charAt(j)
      m(toIndex(nucleotide), j) += 1
    }
    m
  }

  type Profile = DenseMatrix[Double]

  def profile(motifs: DNAMotif, addCount: Int = 0): Profile = {
    val t = motifs.value.length
    val c: Profile = count(motifs) :+ addCount.toDouble
    c :/ t.toDouble
  }

  private val FROM_INDEX_TO_NUCLEOTIDE_CONVERTION = IndexedSeq('A', 'C', 'G', 'T')
  def fromIndex(index: Int): Char = FROM_INDEX_TO_NUCLEOTIDE_CONVERTION(index)

  def consensi(p: Profile): Set[String] = {
    @tailrec def consensi_(p: Profile, result: Set[String]): Set[String] = {
      if (p.cols == 0) result
      else {
        val m = breeze.linalg.max(p(::, 0))
        val s = for {
          r ← result
          i ← 0 to 3
          if p(i, 0) == m
        } yield r + fromIndex(i)
        consensi_(p(::, 1 until p.cols), s)
      }
    }
    consensi_(p, Set("")) - ""
  }

  def consensus(motifs: DNAMotif): DNAString = {
    val p = profile(motifs)
    val k = motifs.value.head.value.length
    val s = new StringBuilder
    for (j ← 0 until k) {
      s.append(fromIndex(argmax(p(::, j))))
    }
    DNAString.from(s.toString()).get
  }

  def score(motifs: DNAMotif): Int = {
    val c = consensus(motifs)
    motifs.value.foldLeft(0) { (d, text) ⇒
      d + Chapter1.hammingDistance(c.value, text.value)
    }
  }

  def entropy(motifs: DNAMotif): Double = {
    def entropy(v: Vector[Double]): Double = {
      def log2(x: Double): Double = log(x) / log(2)
      v.fold(0.0) { (acc, el) ⇒
        acc - (if (el == 0.0) 0.0 else el * log2(el))
      }
    }
    val p = profile(motifs)
    val k = motifs.value.head.value.length
    (for {
      j ← 0 until k
    } yield entropy(p(::, j))).sum
  }

  def distance(pattern: DNAString, dna: IndexedSeq[DNAString]): Int = {
    val k = pattern.value.length
    dna.map { d ⇒
      (for {
        i ← 0 to (d.value.length - k)
        kMer = d.value.substring(i, i + k)
      } yield Chapter1.hammingDistance(pattern.value, kMer)).min
    }.sum
  }

  def bruteForceMedianString(dna: IndexedSeq[DNAString], k: Int, all: Boolean = false): Set[DNAString] = {
    val DPs = for {
      i ← 0 until pow(4, k).toInt
      pattern = DNAString.unsafeFrom(Chapter1.numberToPattern(i, k))
      d = distance(pattern, dna)
    } yield (d, pattern)

    val minimalDP = DPs.minBy(_._1)
    (if (all) DPs.filter(_._1 == minimalDP._1) else Seq(minimalDP)).map(_._2).toSet
  }

  def probability(text: DNAString, profile: Profile): Double = {
    @tailrec def probability_(text: String, profile: Profile, result: Double): Double = {
      if (text.isEmpty || profile.cols == 0)
        result
      else
        probability_(text.tail, profile(::, 1 until profile.cols), result * profile(toIndex(text.head), 0))
    }
    probability_(text.value, profile, 1.0)
  }

  def kMers(text: DNAString, k: Int): IndexedSeq[DNAString] = {
    @tailrec def kMers_(text: String, result: IndexedSeq[DNAString]): IndexedSeq[DNAString] = {
      if (text.length < k)
        result
      else
        kMers_(text.tail, result :+ DNAString.from(text.take(k)).get)
    }
    kMers_(text.value, IndexedSeq.empty[DNAString]).sortBy(_.value)
  }

  def profileMostProbableKmer(text: DNAString, k: Int, profile: Profile): DNAString = {
    kMers(text, k).map { kMer ⇒
      (probability(kMer, profile), kMer)
    }.maxBy(_._1)._2
    // CAREFUL maxBy uses GT comparison, while max uses GTEQ comparison! and the maxBy looks nicer!
  }

  // http://www.mrgraeme.co.uk/greedy-motif-search/
  private def greedyMotifSearch_(addCount: Int)(dna: IndexedSeq[DNAString], k: Int, t: Int): IndexedSeq[DNAString] = {
    val allMotifs = kMers(dna.head, k).map { kMer ⇒
      val motifs = dna.tail.foldLeft(IndexedSeq(kMer)) { (acc, text) ⇒
        acc :+ profileMostProbableKmer(text, k, profile(DNAMotif.unsafeFrom(acc), addCount))
      }
      (motifs, score(DNAMotif.unsafeFrom(motifs)))
    }
    allMotifs.minBy(_._2)._1
    // CAREFUL minBy uses LT comparison, while min uses LTEQ comparison! and the minBy looks nicer!
    //allMotifs.min(Ordering[Int].on[(IndexedSeq[String],Int)](_._2))._1
  }

  val greedyMotifSearch: (IndexedSeq[DNAString], Int, Int) ⇒ IndexedSeq[DNAString] = greedyMotifSearch_(0)
  val greedyMotifSearchWithPseudocounts: (IndexedSeq[DNAString], Int, Int) ⇒ IndexedSeq[DNAString] = greedyMotifSearch_(1)

  def randomizedMotifSearch(dna: IndexedSeq[DNAString], k: Int, t: Int, N: Int = 1000): IndexedSeq[DNAString] = {
    def motifs(profile: Profile, dna: IndexedSeq[DNAString]): DNAMotif = {
      DNAMotif.unsafeFrom(dna.map(profileMostProbableKmer(_, k, profile)))
    }

    (1 to N).foldLeft((IndexedSeq.empty[DNAString], Int.MaxValue)) {
      case ((overalBestMotifs, scoreOveralBestMotifs), _) ⇒
        var currentMotifs = DNAMotif.unsafeFrom(dna.map { text ⇒
          DNAString.unsafeFrom(text.value.drop(Random.nextInt(text.value.length - k + 1)).take(k))
        })
        var bestMotifs = (currentMotifs, score(currentMotifs))
        var continue = true
        while (continue) {
          val currentProfile = profile(currentMotifs, 1)
          currentMotifs = motifs(currentProfile, dna)
          val currentScore = score(currentMotifs)
          if (currentScore < bestMotifs._2) {
            bestMotifs = (currentMotifs, currentScore)
            //println("Current: " + bestMotifs)
          } else
            continue = false
        }
        if (bestMotifs._2 < scoreOveralBestMotifs) {
          //println("Overal: " + bestMotifs)
          (bestMotifs._1.value, bestMotifs._2)
        } else
          (overalBestMotifs, scoreOveralBestMotifs)
    }._1
  }

  def random(prob: Double*): Int = {
    val totalProb = prob.sum
    val roll = Random.nextDouble() * totalProb
    @tailrec def random_(v: Double, prob: Seq[Double], result: Int): Int = {
      if (prob.isEmpty || v <= prob.head) result
      else random_(v - prob.head, prob.tail, result + 1)
    }
    random_(roll, prob, 0)
  }

  def profileRandomlyGeneratedKmer(text: DNAString, k: Int, profile: Profile): DNAString = {
    val km = kMers(text, k)
    val chosenIndex = random(km.map(probability(_, profile)): _*)
    km(chosenIndex)
  }

  def gibbsSampler(dna: IndexedSeq[DNAString], k: Int, t: Int, N: Int, M: Int = 20): IndexedSeq[DNAString] = {
    (1 to M).foldLeft((IndexedSeq.empty[DNAString], Int.MaxValue)) {
      case ((overalBestMotifs, scoreOveralBestMotifs), _) ⇒
        val startMotifs = DNAMotif.unsafeFrom(dna.map { text ⇒
          DNAString.unsafeFrom(text.value.drop(Random.nextInt(text.value.length - k + 1)).take(k))
        })
        val current = (1 to N).foldLeft((startMotifs, score(startMotifs))) {
          case ((bestMotifs, scoreBestMotifs), _) ⇒
            val i = Random.nextInt(t)
            //print(bestMotifs + " " + i)
            val (front, back) = bestMotifs.value.splitAt(i)
            //print(" " + (front ++ back.tail))
            val newProfile = profile(DNAMotif.unsafeFrom(front ++ back.tail), 1)
            //println(p)
            val newMotifs = DNAMotif.unsafeFrom((front :+ profileRandomlyGeneratedKmer(dna(i), k, newProfile)) ++ back.tail)
            val newScore = score(newMotifs)
            if (newScore < scoreBestMotifs)
              (newMotifs, newScore)
            else
              (bestMotifs, scoreBestMotifs)
        }
        if (current._2 < scoreOveralBestMotifs)
          (current._1.value, current._2)
        else
          (overalBestMotifs, scoreOveralBestMotifs)
    }._1
  }
}
