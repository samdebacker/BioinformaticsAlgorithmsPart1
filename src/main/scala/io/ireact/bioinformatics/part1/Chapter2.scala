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

import io.ireact.bioinformatics.part1.support.Peptide

import scala.annotation.tailrec

object Chapter2 {
  val geneticCode = Map(
    "AAA" -> "K",
    "AAC" -> "N",
    "AAG" -> "K",
    "AAU" -> "N",
    "ACA" -> "T",
    "ACC" -> "T",
    "ACG" -> "T",
    "ACU" -> "T",
    "AGA" -> "R",
    "AGC" -> "S",
    "AGG" -> "R",
    "AGU" -> "S",
    "AUA" -> "I",
    "AUC" -> "I",
    "AUG" -> "M",
    "AUU" -> "I",
    "CAA" -> "Q",
    "CAC" -> "H",
    "CAG" -> "Q",
    "CAU" -> "H",
    "CCA" -> "P",
    "CCC" -> "P",
    "CCG" -> "P",
    "CCU" -> "P",
    "CGA" -> "R",
    "CGC" -> "R",
    "CGG" -> "R",
    "CGU" -> "R",
    "CUA" -> "L",
    "CUC" -> "L",
    "CUG" -> "L",
    "CUU" -> "L",
    "GAA" -> "E",
    "GAC" -> "D",
    "GAG" -> "E",
    "GAU" -> "D",
    "GCA" -> "A",
    "GCC" -> "A",
    "GCG" -> "A",
    "GCU" -> "A",
    "GGA" -> "G",
    "GGC" -> "G",
    "GGG" -> "G",
    "GGU" -> "G",
    "GUA" -> "V",
    "GUC" -> "V",
    "GUG" -> "V",
    "GUU" -> "V",
    "UAA" -> "",
    "UAC" -> "Y",
    "UAG" -> "",
    "UAU" -> "Y",
    "UCA" -> "S",
    "UCC" -> "S",
    "UCG" -> "S",
    "UCU" -> "S",
    "UGA" -> "",
    "UGC" -> "C",
    "UGG" -> "W",
    "UGU" -> "C",
    "UUA" -> "L",
    "UUC" -> "F",
    "UUG" -> "L",
    "UUU" -> "F"
  )

  val integerMass = Map(
    'G' -> 57,
    'A' -> 71,
    'S' -> 87,
    'P' -> 97,
    'V' -> 99,
    'T' -> 101,
    'C' -> 103,
    'I' -> 113,
    'L' -> 113,
    'N' -> 114,
    'D' -> 115,
    'K' -> 128,
    'Q' -> 128,
    'E' -> 129,
    'M' -> 131,
    'H' -> 137,
    'F' -> 147,
    'R' -> 156,
    'Y' -> 163,
    'W' -> 186
  )

  val masses: Seq[Int] = integerMass.map(_._2).toSet.toSeq.sorted

  val reverseGenericCode = geneticCode.groupBy(_._2).filterKeys(!_.isEmpty).mapValues(_.keySet)

  def peptideFor(rnaPattern: String): String = {
    val N = rnaPattern.length - 3 + 1
    (0 until N by 3).foldLeft(new StringBuilder(rnaPattern.length / 3)) { (r, i) ⇒
      r.append(geneticCode(rnaPattern.substring(i, i + 3)))
    }.toString
  }

  def countPossiblePatternsTranslatingToPeptide(peptide: String): Int = {
    peptide.foldLeft(1) { (r, c) ⇒ r * reverseGenericCode(c.toString).size }
  }

  /*
  def dnaToRnaPatterns(dnaPattern: String): (String, String) = {
    def transcribe(dnaPattern: String): String = {
      def transcribe(c: Char): Char = if (c == 'T') 'U' else c
      @tailrec def transcribeImpl(dnaPattern: String, result: String): String =
        if (dnaPattern.isEmpty) result
        else transcribeImpl(dnaPattern.tail, result + transcribe(dnaPattern.head))
      transcribeImpl(dnaPattern, "")
    }
    (transcribe(dnaPattern), transcribe(Chapter1.reverseComplement(dnaPattern)))
  }
  */

  def dnaEncodesPeptide(dnaPattern: String, peptide: String): Seq[String] = {
    @inline def transcribe(dnaPattern: String): String = {
      dnaPattern.foldLeft(new StringBuilder()) { (r, c) ⇒
        r.append(if (c == 'T') 'U' else c)
      }.toString
    }
    val L = 3 * peptide.length
    val N = dnaPattern.length - L + 1
    (0 until N).foldLeft(Seq.empty[String]) { (result, i) ⇒
      val dnaPart = dnaPattern.substring(i, i + L)
      if (peptide == peptideFor(transcribe(dnaPart)) || peptide == peptideFor(transcribe(Chapter1.reverseComplement(dnaPart))))
        dnaPart +: result
      else
        result
    }.reverse
  }

  def massOf(peptide: String): Int =
    peptide.map(integerMass(_)).reduce(_ + _)

  def theoreticalSpectrum(cyclicPeptide: String): Seq[Int] = {
    val maxLength = cyclicPeptide.length
    val helper = cyclicPeptide + cyclicPeptide
    (1 until maxLength).foldLeft(Seq(0, massOf(cyclicPeptide))) { (masses, forLength) ⇒
      (0 until maxLength).foldLeft(masses) { (masses, i) ⇒
        val subPeptide = helper.substring(i, i + forLength)
        massOf(subPeptide) +: masses
      }
    }.sorted
  }

  type Spectrum = Seq[Int]

  def peptideFrom(peptide: String): Peptide = Peptide.from(peptide.map(integerMass(_)))

  /**
   * @return (cyclicSpectrum, linearSpectrum) with cyclicSpectrum sorted and linearSpectrum unsorted
   */
  private[this] final def spectra(peptide: Peptide): (Spectrum, Spectrum) = {
    val prefixMass = peptide.value.foldLeft(IndexedSeq(0)) { (result, e) ⇒
      result :+ (result.last) + e
    }
    val peptideMass = prefixMass.last
    val peptideLength = peptide.value.length
    val (cyclicSpectrum, linearSpectrum) =
      (0 until peptideLength).foldLeft((Seq(0), Seq(0))) { (r, i) ⇒
        ((i + 1) to peptideLength).foldLeft(r) {
          case ((cyclicSpectrum, linearSpectrum), j) ⇒
            val currentVal = prefixMass(j) - prefixMass(i)
            (currentVal +: (if ((i > 0) && (j < peptideLength)) (peptideMass - currentVal) +: cyclicSpectrum else cyclicSpectrum),
              currentVal +: linearSpectrum)
        }
      }
    (cyclicSpectrum.sorted, linearSpectrum)
  }

  def cyclicSpectrum(peptide: Peptide): Spectrum = {
    spectra(peptide)._1
  }

  def linearSpectrum(peptide: Peptide): Spectrum = {
    spectra(peptide)._2.sorted
  }

  @inline private[this] final def isConsistentWith(peptide: Peptide, spectrum: Spectrum): Boolean = {
    peptide.value.diff(spectrum).isEmpty
  }

  private[this] final def expand(peptides: Seq[Peptide], massesToUse: Seq[Int] = masses): Seq[Peptide] = {
    for {
      peptide ← peptides
      mass ← massesToUse
    } yield Peptide.from(mass +: peptide.value) // (we prepend but in the end it doesn't matter if the peptide is constructed in reverse, since the reverse will also be in the result
  }

  /* uses massive memory, start sbt with:
      sbt -J-Xmx12G
     well not anymore since we now correctly check the LSP is consistent with the SP, and not my own peptide is consistent with elements in spectrum
   */
  def cyclopeptideSequencingBranchAndBound(spectrum: Spectrum): Set[Peptide] = {
    var results = Set.empty[Peptide]
    val basicAminoAcidsInSpectrum: Seq[Int] = masses.intersect(spectrum)
    var peptides = Seq(Peptide.from(Seq.empty[Int])) // DO NOT .par, due to some side-effect things go wrong
    //println("Peptides = " + peptides.size)
    while (peptides.nonEmpty) {
      peptides = expand(peptides, basicAminoAcidsInSpectrum)
      //println("Peptides expanded = " + peptides.size)

      peptides = peptides.filter { peptide ⇒
        val (cyclicSpectrumSorted, linearSpectrum) = spectra(peptide)
        if (cyclicSpectrumSorted == spectrum) {
          results += peptide
          //print(peptide.mkString("-") + " ")
          false
        } else isConsistentWith(Peptide.from(linearSpectrum), spectrum)
      }
    }
    results
  }

  def score(left: Spectrum, right: Spectrum): Int = {
    @tailrec def scoreImpl(left: Spectrum, right: Spectrum, result: Int): Int = {
      if (left.isEmpty || right.isEmpty) result
      else {
        val l = left.head
        val r = right.head
        if (l == r)
          scoreImpl(left.tail, right.tail, result + 1)
        else if (l < r)
          scoreImpl(left.tail, right, result)
        else // (l > r)
          scoreImpl(left, right.tail, result)
      }
    }
    scoreImpl(left, right, 0)
  }

  def massOf(peptide: Peptide): Int = peptide.value.reduce(_ + _)

  def topWithTies[A](leaderboard: IndexedSeq[A], n: Int)(f: A ⇒ Int): IndexedSeq[A] = {
    if (n >= leaderboard.length) leaderboard
    else {
      val pivot = f(leaderboard(n - 1))
      var i = n
      while (i < leaderboard.length && f(leaderboard(i)) == pivot) i += 1
      leaderboard.take(i)
    }
  }

  def cyclopeptideSequencingLeaderboard(spectrum: Spectrum, n: Int, massesToUse: Seq[Int] = masses): Peptide = {
    case class PeptideData(peptide: Peptide, spectrum: Spectrum, score: Int)

    def expand(peptides: IndexedSeq[PeptideData]): IndexedSeq[PeptideData] = {
      for {
        peptideData ← peptides
        mass ← massesToUse
        newPeptide = Peptide.from(mass +: peptideData.peptide.value)
        newLinearSpectrum = linearSpectrum(newPeptide)
        newScore = score(newLinearSpectrum, spectrum)
      } yield PeptideData(newPeptide, newLinearSpectrum, newScore)
    }

    val parentMass = spectrum.last
    var leaderPeptideData = PeptideData(Peptide.from(Seq.empty[Int]), Seq(0), 1)
    var leaderboard = IndexedSeq(leaderPeptideData)
    println("Leaderboard = " + leaderboard.length)
    while (leaderboard.nonEmpty) {
      leaderboard = expand(leaderboard)
      //println("Leaderboard expanded = " + leaderboard.length)

      leaderboard = leaderboard.filter { currentPeptideData ⇒
        val massCurrentPeptide = massOf(currentPeptideData.peptide)
        if (massCurrentPeptide == parentMass) {
          val currentCyclicpeptideSpectrum = cyclicSpectrum(currentPeptideData.peptide)
          val scoreCurrentCyclicpeptide = score(currentCyclicpeptideSpectrum, spectrum)
          if (scoreCurrentCyclicpeptide > leaderPeptideData.score) {
            leaderPeptideData = currentPeptideData copy (spectrum = currentCyclicpeptideSpectrum, score = scoreCurrentCyclicpeptide)
          }
          true
        } else massCurrentPeptide <= parentMass
      }
      leaderboard = topWithTies(leaderboard.sortBy(_.score)(Ordering[Int].reverse), n)(_.score)
      println("Leaderboard trimmed = " + leaderboard.size)
    }
    println(leaderPeptideData.score)
    leaderPeptideData.peptide
  }

  def spectralConvolution(spectrum: Spectrum): IndexedSeq[Int] = {
    var result = IndexedSeq.empty[Int]
    for (i ← 0 until spectrum.length) {
      for (j ← (i + 1) until spectrum.length) {
        val diff = spectrum(j) - spectrum(i)
        if (diff > 0) {
          result = result :+ diff
        }
      }
    }
    result.sorted(Ordering[Int].reverse)
  }

  def cyclopeptideSequencingConvolution(m: Int, n: Int, spectrum: Spectrum): Peptide = {
    spectralConvolution(spectrum).groupBy(k ⇒ k).map(kv ⇒ (kv._1, kv._2.length))
    val aminosFreq: IndexedSeq[(Int, Int)] = spectralConvolution(spectrum).filter(e ⇒ e >= 57 && e <= 200).groupBy(k ⇒ k).map(kv ⇒ (kv._1, kv._2.length)).toIndexedSeq.sortBy(_._2)(Ordering[Int].reverse)
    //println(aminosFreq)
    val popularAminos: IndexedSeq[Int] = topWithTies(aminosFreq, m)(e ⇒ e._2).map(_._1)
    //println(popularAminos.mkString(" "))
    cyclopeptideSequencingLeaderboard(spectrum, n, popularAminos)
  }
}
