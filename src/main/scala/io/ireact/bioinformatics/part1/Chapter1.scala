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

import scala.annotation.tailrec
import scala.collection
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable

object Chapter1 {
  def patternCount(text: String, pattern: String): Int = {
    val patternLength = pattern.length
    @tailrec
    def patternCountImpl(text: String, result: Int): Int =
      if (text.length < patternLength) result
      else patternCountImpl(text.substring(1), if (text.substring(0, patternLength) == pattern) result + 1 else result)
    patternCountImpl(text, 0)
  }

  def reverseComplement(text: String): String = {
    @inline def complement(p: Char): Char = p match {
      case 'A' ⇒ 'T'
      case 'T' ⇒ 'A'
      case 'G' ⇒ 'C'
      case 'C' ⇒ 'G'
    }
    text.foldLeft(new StringBuilder(text.length)) { (r, c) ⇒ r.append(complement(c)) }.toString.reverse
  }

  def frequentWords(text: String, k: Int) = {
    var maxCount = 0
    val count = new Array[Int](text.length - k + 1)
    for (i ← 0 to (text.length - k)) {
      val pattern = text.substring(i, i + k)
      count(i) = patternCount(text.substring(i), pattern) // we can skip the first i positions
      maxCount = Math.max(maxCount, count(i))
    }
    //println(count.mkString(" "))
    val frequentPatterns = mutable.Set.empty[String]
    for (i ← 0 to (text.length - k)) {
      if (count(i) == maxCount) {
        frequentPatterns += text.substring(i, i + k)
      }
    }
    //println(maxCount)
    frequentPatterns
  }

  def frequentWordsMap(text: String, k: Int): Set[String] = {
    var maxCount = 0
    val frequency = mutable.Map.empty[String, Int]
    for (i ← 0 to (text.length - k)) {
      val pattern = text.substring(i, i + k)
      var updatedFrequency = 1
      if (frequency.contains(pattern)) {
        updatedFrequency = frequency(pattern) + 1
      }
      frequency += pattern -> updatedFrequency
      maxCount = Math.max(maxCount, updatedFrequency)
    }
    //println(frequency)
    val frequentPatterns = mutable.Set.empty[String]
    for ((key, value) ← frequency) {
      if (value == maxCount) {
        frequentPatterns += key
      }
    }
    //println(maxCount)
    collection.immutable.Set[String](frequentPatterns.toArray: _*)
  }

  def findPattern(text: String, pattern: String): List[Int] = {
    @tailrec
    def findPatternImpl(text: String, offset: Int, result: List[Int]): List[Int] = {
      val index = text.indexOf(pattern)
      if (index < 0) result
      else findPatternImpl(text.substring(index + 1), offset + index + 1, result :+ (offset + index))
    }
    findPatternImpl(text, 0, Nil)
  }

  def findClumps(text: String, k: Int, L: Int, t: Int) = {
    def findFrequent(text: String): Iterable[String] = {
      val frequencies = mutable.Map.empty[String, Int]
      for (i ← 0 to (text.length - k)) {
        val pattern = text.substring(i, i + k)
        var updatedFrequency = 1
        if (frequencies.contains(pattern)) {
          updatedFrequency = frequencies(pattern) + 1
        }
        frequencies += pattern -> updatedFrequency
      }
      val result = frequencies.filter(_._2 >= t).keys
      //println(result.m)
      result
    }
    val result = mutable.Set.empty[String]
    for (i ← 0 to (text.length - L)) {
      //result ++= findFrequent(text.substring(i, i + L))
      findFrequent(text.substring(i, i + L)).foreach(pattern ⇒
        if (!result.contains(pattern)) {
          result += pattern
          //println(/*result.size + " " + */ pattern)
        }
      )
    }
    result
  }

  private def numeric(p: Char): Int = p match {
    case 'A' ⇒ 0
    case 'C' ⇒ 1
    case 'G' ⇒ 2
    case 'T' ⇒ 3
  }

  lazy val CONVERTION = Vector('A', 'C', 'G', 'T')

  private def alpha(i: Int): Char = CONVERTION(i)

  private def patternToNumber(pattern: String): Int = {
    @tailrec def patternToNumberImpl(pattern: String, result: Int): Int = {
      if (pattern.length == 0) result
      else patternToNumberImpl(pattern.tail, numeric(pattern.head) + (result * 4))
    }
    patternToNumberImpl(pattern, 0)
  }

  def numberToPattern(n: Int, l: Int): String = {
    @tailrec def numberToPatternImpl(n: Int, result: List[Char]): String = {
      if (result.length == l) result.mkString
      else numberToPatternImpl(n / 4, alpha(n % 4) :: result)
    }
    numberToPatternImpl(n, Nil)
  }

  def computeFrequencies(text: String, k: Int): Array[Int] = {
    val frequencies = new Array[Int](Math.pow(4, k).toInt)
    for (i ← 0 to (text.length - k)) {
      val pattern = text.substring(i, i + k)
      frequencies(patternToNumber(pattern)) += 1
    }
    frequencies
  }

  def fastFrequentWords(text: String, k: Int): Set[String] = {
    val frequencies = computeFrequencies(text, k)
    val max = frequencies.reduce(Math.max)
    //println(max)
    for {
      i ← (0 until Math.pow(4, k).toInt).toSet
      if frequencies(i) == max
    } yield numberToPattern(i, k)
  }

  def fastFindClumps(text: String, k: Int, L: Int, t: Int): mutable.Set[String] = {
    val clump = new Array[Boolean](Math.pow(4, k).toInt)
    val result = mutable.Set.empty[String]
    for (i ← 0 to (text.length - L)) {
      val frequencies = computeFrequencies(text.substring(i, i + L), k)
      for (i ← 0 until frequencies.length) {
        if ((frequencies(i) >= t) && !clump(i)) {
          clump(i) = true
          val pattern = numberToPattern(i, k)
          result += pattern
          println(result.size + " " + pattern)
        }
      }
    }
    result
  }

  def superFastFindClumps(text: String, k: Int, L: Int, t: Int): mutable.Set[String] = {
    def findFrequencies(text: String): mutable.Map[String, Int] = {
      val frequencies = mutable.Map.empty[String, Int]
      for (i ← 0 to (text.length - k)) {
        val pattern = text.substring(i, i + k)
        var updatedFrequency = 1
        if (frequencies.contains(pattern)) {
          updatedFrequency = frequencies(pattern) + 1
        }
        frequencies += pattern -> updatedFrequency
      }
      frequencies
    }
    def updateFrequencies(drop: String, add: String, frequencies: mutable.Map[String, Int], result: mutable.Set[String]) = {
      if (drop != add) {
        if (frequencies.contains(drop)) {
          val newFrequency = frequencies(drop) - 1
          if (newFrequency == 0) {
            frequencies.remove(drop)
          } else {
            frequencies += drop -> newFrequency
          }
        }
        var newFrequency = 1
        newFrequency += frequencies.getOrElse(add, 0)
        frequencies += add -> newFrequency
        if ((newFrequency >= t) && !result.contains(add)) {
          result += add
          //println(/*result.size + " " + */ add)
        }
      }
    }

    val frequencies = findFrequencies(text.substring(0, L))
    val result = mutable.Set.empty[String]
    frequencies.filter(_._2 >= t).keys.foreach(pattern ⇒
      if (!result.contains(pattern)) {
        result += pattern
        println( /*result.size + " " + */ pattern)
      }
    )
    for (i ← 0 until (text.length - L)) {
      updateFrequencies(text.substring(i, i + k), text.substring(i + L - k + 1, i + L + 1), frequencies, result)
    }
    result
  }

  def skew(text: String): Array[Int] = {
    def weight(p: Char) = p match {
      case 'C' ⇒ -1
      case 'G' ⇒ 1
      case _   ⇒ 0
    }
    val result = new Array[Int](text.length + 1)
    result(0) = 0
    for (i ← 1 to text.length) {
      result(i) = result(i - 1) + weight(text(i - 1))
    }
    result
  }

  def xSkew(op: (Int, Int) ⇒ Int)(text: String): IndexedSeq[Int] = {
    val s = skew(text)
    val m = s.reduce(op)
    for {
      i ← 0 until s.length
      if s(i) == m
    } yield i
  }

  def minSkew = xSkew(math.min) _
  def maxSkew = xSkew(math.max) _

  // O(k)
  def hammingDistance(l: String, r: String): Int = {
    @tailrec def hammingDistanceImpl(l: String, r: String, d: Int): Int =
      if (l.isEmpty) d
      else hammingDistanceImpl(l.tail, r.tail, d + (if (l.head == r.head) 0 else 1))
    hammingDistanceImpl(l, r, 0)
  }

  def findApproxPattern(text: String, pattern: String, d: Int): List[Int] = {
    @tailrec def findApproxPatternImpl(text: String, offset: Int, result: List[Int]): List[Int] =
      if (text.length < pattern.length) result
      else {
        val currentD = hammingDistance(text.substring(0, pattern.length), pattern)
        findApproxPatternImpl(text.tail, offset + 1, if (currentD <= d) result :+ offset else result)
      }
    findApproxPatternImpl(text, 0, Nil)
  }

  // O(n.k)
  def countApproxPattern(text: String, pattern: String, d: Int): Int = {
    @tailrec def countApproxPatternImpl(text: String, count: Int): Int =
      if (text.length < pattern.length) count
      else {
        val currentD = hammingDistance(text.substring(0, pattern.length), pattern)
        countApproxPatternImpl(text.tail, if (currentD <= d) count + 1 else count)
      }
    countApproxPatternImpl(text, 0)
  }

  def neighbours(pattern: String, d: Int): Set[String] = {
    if (d == 0) Set(pattern)
    else if (pattern.length == 1) Set("A", "C", "G", "T")
    else {
      val neighbourhood = mutable.Set.empty[String]
      val tailNeighbours = neighbours(pattern.tail, d)
      tailNeighbours.foreach { aTailNeighbour ⇒
        if (hammingDistance(pattern.tail, aTailNeighbour) < d)
          neighbourhood ++= Set("A", "C", "G", "T").map(_ + aTailNeighbour)
        else
          neighbourhood += pattern.head + aTailNeighbour
      }
      neighbourhood.toSet
    }
  }

  private def mostFrequentApproxPatternsImpl(text: String, k: Int, d: Int, countFunction: (String, String, Int) ⇒ Int): Set[String] = {
    var maxCount = 0
    val hood = mutable.Set.empty[String]
    for (i ← 0 to (text.length - k)) {
      val pattern = text.substring(i, i + k)
      val n = neighbours(pattern, d)
      hood ++= n
    }
    val frequencies = mutable.Map.empty[String, Int]
    hood.foreach { pattern ⇒
      val countApprox = countFunction(text, pattern, d)
      maxCount = Math.max(maxCount, countApprox)
      frequencies += pattern -> countApprox
    }
    frequencies.filter(_._2 == maxCount).keySet.toSet
  }

  def mostFrequentApproxPatterns(text: String, k: Int, d: Int): Set[String] = mostFrequentApproxPatternsImpl(text, k, d, countApproxPattern)

  private def countApproxPatternAndComplementPattern(text: String, pattern: String, d: Int): Int = countApproxPattern(text, pattern, d) + countApproxPattern(text, reverseComplement(pattern), d)

  def mostFrequentWithComplementApproxPatterns(text: String, k: Int, d: Int): Set[String] = mostFrequentApproxPatternsImpl(text, k, d, countApproxPatternAndComplementPattern)
}
