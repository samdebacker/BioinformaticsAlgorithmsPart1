/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 www.iReact.io
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

import io.ireact.bioinformatics.part1.support.DNAString
import spray.json._

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object Chapter5 {
  def dpChange(money: Int, coins: Set[Int]): Int = {
    val l = (0 to money).map { m ⇒
      (m → (if (m == 0) 0 else Int.MaxValue))
    }
    val minNumCoins = ListMap(l: _*)
    val result = minNumCoins.tail.foldLeft(minNumCoins) {
      case (minNumCoins, (m, _)) ⇒
        coins.foldLeft(minNumCoins) { (minNumCoins, c) ⇒
          if (m >= c && (minNumCoins(m - c) + 1 < minNumCoins(m)))
            minNumCoins.updated(m, minNumCoins(m - c) + 1)
          else
            minNumCoins
        }
    }
    result(money)
  }

  def manhattanTourist(n: Int, m: Int)(down: IndexedSeq[IndexedSeq[Int]] /* n x (m + 1) */ , right: IndexedSeq[IndexedSeq[Int]] /* (n + 1) x m */ ): Int = {
    val downTranspose: IndexedSeq[IndexedSeq[Int]] = down.transpose
    var s = IndexedSeq.tabulate(n + 1, m + 1) {
      case (0, 0) ⇒ 0
      case (i, 0) ⇒ downTranspose(0).take(i).sum
      case (0, j) ⇒ right(0).take(j).sum
      case _      ⇒ 0
    }
    for (i ← 1 to n) {
      for (j ← 1 to m) {
        val m = math.max(s(i - 1)(j) + down(i - 1)(j), s(i)(j - 1) + right(i)(j - 1))
        s = s.updated(i, s(i).updated(j, m))
      }
    }
    s(n)(m)
  }

  def longestCommonSubsequence(v: DNAString, w: DNAString): DNAString = {
    def lcsBacktrack: IndexedSeq[IndexedSeq[Char]] = {
      val n = v.value.length
      val m = w.value.length
      var s = IndexedSeq.fill(n + 1, m + 1)(0)
      var backtrack = IndexedSeq.fill(n + 1, m + 1)(' ')
      for (i ← 1 to n) {
        for (j ← 1 to m) {
          val x = s(i - 1)(j - 1) + (if (v.value(i - 1) == w.value(j - 1)) 1 else 0)
          val m = math.max(math.max(s(i - 1)(j), s(i)(j - 1)), x)
          s = s.updated(i, s(i).updated(j, m))
          val b: Char =
            if (s(i)(j) == s(i - 1)(j)) '↓'
            else if (s(i)(j) == s(i)(j - 1)) '→'
            else '↘'
          backtrack = backtrack.updated(i, backtrack(i).updated(j, b))
        }
      }
      backtrack
    }
    @tailrec def output(backtrack: IndexedSeq[IndexedSeq[Char]], i: Int, j: Int, result: StringBuilder): DNAString = {
      if (i == 0 || j == 0) DNAString.unsafeFrom(result.toString.reverse)
      else backtrack(i)(j) match {
        case '↓' ⇒ output(backtrack, i - 1, j, result)
        case '→' ⇒ output(backtrack, i, j - 1, result)
        case _   ⇒ output(backtrack, i - 1, j - 1, result.append(v.value(i - 1)))
      }
    }
    output(lcsBacktrack, v.value.length, w.value.length, new StringBuilder)
  }

  def topologicalOrdering[T](nodes: Set[T], graph: Map[T, Seq[T]]): IndexedSeq[T] = {
    var inDegrees: Map[T, Int] = Chapter4.inDegreesOf(graph, Map(nodes.toSeq.map { node ⇒ (node, 0) }: _*))
    var candidates = inDegrees.filter(_._2 == 0).keySet
    var list = IndexedSeq.empty[T]
    var g = graph
    while (candidates.nonEmpty) {
      val a = candidates.head
      list = list :+ a
      candidates = candidates.tail
      g(a).foreach { b ⇒
        g = g.updated(a, g(b).filterNot(_ == b))
        inDegrees = inDegrees.updated(b, inDegrees(b) - 1)
        if (inDegrees(b) == 0)
          candidates = candidates + b
      }
    }
    list
  }

  def longestPath[T](source: T, sink: T, graph: IndexedSeq[(T, (T, Int))]): (Int, Seq[T]) = {
    val nodes = graph.flatMap { case (k, v) ⇒ Seq(k, v._1) }.toSet
    val sRaw = nodes.toSeq.map(_ → (Int.MinValue, Seq.empty[T]))
    val g = graph.foldLeft(Map.empty[T, Seq[T]].withDefaultValue(Seq.empty[T])) {
      case (g, (f, (t, _))) ⇒ g.updated(f, g(f) :+ t)
    }
    val initS: Map[T, (Int, Seq[T])] = Map(sRaw.toSeq: _*) + (source → (0, Seq(source)))
    val s = topologicalOrdering(nodes, g)
      .foldLeft(initS) { (s, b) ⇒
        val pred = graph.filter { case (_, (t, _)) ⇒ t == b }
        if (pred.nonEmpty) {
          val max = pred.map {
            case (f, (t, w)) ⇒
              (s(f)._1 + w, s(f)._2 :+ t)
          }.maxBy(_._1)
          s.updated(b, max)
        } else
          s
      }
    s(sink)
  }

  private[this] lazy val blossum62: Map[Char, Map[Char, Int]] = Map(
    'A' → Map('A' → 4, 'C' → 0, 'D' → -2, 'E' → -1, 'F' -> -2, 'G' → 0, 'H' → -2, 'I' -> -1, 'K' → -1, 'L' → -1, 'M' → -1, 'N' → -2, 'P' → -1, 'Q' → -1, 'R' → -1, 'S' → 1, 'T' → 0, 'V' → 0, 'W' → -3, 'Y' → -2),
    'C' → Map('A' → 0, 'C' → 9, 'D' → -3, 'E' → -4, 'F' -> -2, 'G' → -3, 'H' → -3, 'I' -> -1, 'K' → -3, 'L' → -1, 'M' → -1, 'N' → -3, 'P' → -3, 'Q' → -3, 'R' → -3, 'S' → -1, 'T' → -1, 'V' → -1, 'W' → -2, 'Y' → -2),
    'D' → Map('A' → -2, 'C' → -3, 'D' → 6, 'E' → 2, 'F' -> -3, 'G' → -1, 'H' → -1, 'I' -> -3, 'K' → -1, 'L' → -4, 'M' → -3, 'N' → 1, 'P' → -1, 'Q' → 0, 'R' → -2, 'S' → 0, 'T' → -1, 'V' → -3, 'W' → -4, 'Y' → -3),
    'E' → Map('A' → -1, 'C' → -4, 'D' → 2, 'E' → 5, 'F' -> -3, 'G' → -2, 'H' → 0, 'I' -> -3, 'K' → 1, 'L' → -3, 'M' → -2, 'N' → 0, 'P' → -1, 'Q' → 2, 'R' → 0, 'S' → 0, 'T' → -1, 'V' → -2, 'W' → -3, 'Y' → -2),
    'F' → Map('A' → -2, 'C' → -2, 'D' → -3, 'E' → -3, 'F' -> 6, 'G' → -3, 'H' → -1, 'I' -> 0, 'K' → -3, 'L' → 0, 'M' → 0, 'N' → -3, 'P' → -4, 'Q' → -3, 'R' → -3, 'S' → -2, 'T' → -2, 'V' → -1, 'W' → 1, 'Y' → 3),
    'G' → Map('A' → 0, 'C' → -3, 'D' → -1, 'E' → -2, 'F' -> -3, 'G' → 6, 'H' → -2, 'I' -> -4, 'K' → -2, 'L' → -4, 'M' → -3, 'N' → 0, 'P' → -2, 'Q' → -2, 'R' → -2, 'S' → 0, 'T' → -2, 'V' → -3, 'W' → -2, 'Y' → -3),
    'H' → Map('A' → -2, 'C' → -3, 'D' → -1, 'E' → 0, 'F' -> -1, 'G' → -2, 'H' → 8, 'I' -> -3, 'K' → -1, 'L' → -3, 'M' → -2, 'N' → 1, 'P' → -2, 'Q' → 0, 'R' → 0, 'S' → -1, 'T' → -2, 'V' → -3, 'W' → -2, 'Y' → 2),
    'I' → Map('A' → -1, 'C' → -1, 'D' → -3, 'E' → -3, 'F' -> 0, 'G' → -4, 'H' → -3, 'I' -> 4, 'K' → -3, 'L' → 2, 'M' → 1, 'N' → -3, 'P' → -3, 'Q' → -3, 'R' → -3, 'S' → -2, 'T' → -1, 'V' → 3, 'W' → -3, 'Y' → -1),
    'K' → Map('A' → -1, 'C' → -3, 'D' → -1, 'E' → 1, 'F' -> -3, 'G' → -2, 'H' → -1, 'I' -> -3, 'K' → 5, 'L' → -2, 'M' → -1, 'N' → 0, 'P' → -1, 'Q' → 1, 'R' → 2, 'S' → 0, 'T' → -1, 'V' → -2, 'W' → -3, 'Y' → -2),
    'L' → Map('A' → -1, 'C' → -1, 'D' → -4, 'E' → -3, 'F' -> 0, 'G' → -4, 'H' → -3, 'I' -> 2, 'K' → -2, 'L' → 4, 'M' → 2, 'N' → -3, 'P' → -3, 'Q' → -2, 'R' → -2, 'S' → -2, 'T' → -1, 'V' → 1, 'W' → -2, 'Y' → -1),
    'M' → Map('A' → -1, 'C' → -1, 'D' → -3, 'E' → -2, 'F' -> 0, 'G' → -3, 'H' → -2, 'I' -> 1, 'K' → -1, 'L' → 2, 'M' → 5, 'N' → -2, 'P' → -2, 'Q' → 0, 'R' → -1, 'S' → -1, 'T' → -1, 'V' → 1, 'W' → -1, 'Y' → -1),
    'N' → Map('A' → -2, 'C' → -3, 'D' → 1, 'E' → 0, 'F' -> -3, 'G' → 0, 'H' → 1, 'I' -> -3, 'K' → 0, 'L' → -3, 'M' → -2, 'N' → 6, 'P' → -2, 'Q' → 0, 'R' → 0, 'S' → 1, 'T' → 0, 'V' → -3, 'W' → -4, 'Y' → -2),
    'P' → Map('A' → -1, 'C' → -3, 'D' → -1, 'E' → -1, 'F' -> -4, 'G' → -2, 'H' → -2, 'I' -> -3, 'K' → -1, 'L' → -3, 'M' → -2, 'N' → -2, 'P' → 7, 'Q' → -1, 'R' → -2, 'S' → -1, 'T' → -1, 'V' → -2, 'W' → -4, 'Y' → -3),
    'Q' → Map('A' → -1, 'C' → -3, 'D' → 0, 'E' → 2, 'F' -> -3, 'G' → -2, 'H' → 0, 'I' -> -3, 'K' → 1, 'L' → -2, 'M' → 0, 'N' → 0, 'P' → -1, 'Q' → 5, 'R' → 1, 'S' → 0, 'T' → -1, 'V' → -2, 'W' → -2, 'Y' → -1),
    'R' → Map('A' → -1, 'C' → -3, 'D' → -2, 'E' → 0, 'F' -> -3, 'G' → -2, 'H' → 0, 'I' -> -3, 'K' → 2, 'L' → -2, 'M' → -1, 'N' → 0, 'P' → -2, 'Q' → 1, 'R' → 5, 'S' → -1, 'T' → -1, 'V' → -3, 'W' → -3, 'Y' → -2),
    'S' → Map('A' → 1, 'C' → -1, 'D' → 0, 'E' → 0, 'F' -> -2, 'G' → 0, 'H' → -1, 'I' -> -2, 'K' → 0, 'L' → -2, 'M' → -1, 'N' → 1, 'P' → -1, 'Q' → 0, 'R' → -1, 'S' → 4, 'T' → 1, 'V' → -2, 'W' → -3, 'Y' → -2),
    'T' → Map('A' → 0, 'C' → -1, 'D' → -1, 'E' → -1, 'F' -> -2, 'G' → -2, 'H' → -2, 'I' -> -1, 'K' → -1, 'L' → -1, 'M' → -1, 'N' → 0, 'P' → -1, 'Q' → -1, 'R' → -1, 'S' → 1, 'T' → 5, 'V' → 0, 'W' → -2, 'Y' → -2),
    'V' → Map('A' → 0, 'C' → -1, 'D' → -3, 'E' → -2, 'F' -> -1, 'G' → -3, 'H' → -3, 'I' -> 3, 'K' → -2, 'L' → 1, 'M' → 1, 'N' → -3, 'P' → -2, 'Q' → -2, 'R' → -3, 'S' → -2, 'T' → 0, 'V' → 4, 'W' → -3, 'Y' → -1),
    'W' → Map('A' → -3, 'C' → -2, 'D' → -4, 'E' → -3, 'F' -> 1, 'G' → -2, 'H' → -2, 'I' -> -3, 'K' → -3, 'L' → -2, 'M' → -1, 'N' → -4, 'P' → -4, 'Q' → -2, 'R' → -3, 'S' → -3, 'T' → -2, 'V' → -3, 'W' → 11, 'Y' → 2),
    'Y' → Map('A' → -2, 'C' → -2, 'D' → -3, 'E' → -2, 'F' -> 3, 'G' → -3, 'H' → 2, 'I' -> -1, 'K' → -2, 'L' → -1, 'M' → -1, 'N' → -2, 'P' → -3, 'Q' → -1, 'R' → -2, 'S' → -2, 'T' → -2, 'V' → -1, 'W' → 2, 'Y' → 7)
  )

  def globalAlignment(v: String, w: String, scoring: Map[Char, Map[Char, Int]] = blossum62, sigma: Int = 5): (Int, (String, String)) = {
    @tailrec def output(backtrack: IndexedSeq[IndexedSeq[Char]], i: Int, j: Int, rv: StringBuilder, rw: StringBuilder): (String, String) = {
      if (i == 0 || j == 0) {
        var ii = i
        var jj = j
        while (ii > 0) {
          rv.append(v(ii - 1))
          rw.append('-')
          ii = ii - 1
        }
        while (jj > 0) {
          rv.append('_')
          rw.append(w(jj - 1))
          jj = jj - 1
        }
        (rv.toString.reverse, rw.toString.reverse)
      } else backtrack(i)(j) match {
        case '↓' ⇒ output(backtrack, i - 1, j, rv.append(v(i - 1)), rw.append('-'))
        case '→' ⇒ output(backtrack, i, j - 1, rv.append('-'), rw.append(w(j - 1)))
        case _   ⇒ output(backtrack, i - 1, j - 1, rv.append(v(i - 1)), rw.append(w(j - 1)))
      }
    }
    val n = v.length
    val m = w.length
    var s: IndexedSeq[IndexedSeq[Int]] = IndexedSeq.tabulate(n + 1, m + 1) {
      case (0, 0) ⇒ 0
      case (i, 0) ⇒ i * -sigma
      case (0, j) ⇒ j * -sigma
      case _      ⇒ 0
    }
    var backtrack = IndexedSeq.fill(n + 1, m + 1)(' ')
    for (i ← 1 to n) {
      for (j ← 1 to m) {
        val sc = scoring(v(i - 1))(w(j - 1))
        val possibilities = Seq(
          (s(i - 1)(j) - sigma, '↓'),
          (s(i)(j - 1) - sigma, '→'),
          (s(i - 1)(j - 1) + sc, '↘')
        )
        val max = possibilities.maxBy(_._1)
        s = s.updated(i, s(i).updated(j, max._1))
        backtrack = backtrack.updated(i, backtrack(i).updated(j, max._2))
      }
    }
    (s(n)(m), output(backtrack, v.length, w.length, new StringBuilder, new StringBuilder))
  }

  private[this] lazy val pam250: Map[Char, Map[Char, Int]] = """
      {
        "A": { "A": 2, "C": -2, "E": 0, "D": 0, "G": 1, "F": -3, "I": -1, "H": -1, "K": -1, "M": -1, "L": -2, "N": 0, "Q": 0, "P": 1, "S": 1, "R": -2, "T": 1, "W": -6, "V": 0, "Y": -3},
        "C": {"A": -2, "C": 12, "E": -5, "D": -5, "G": -3, "F": -4, "I": -2, "H": -3, "K": -5, "M": -5, "L": -6, "N": -4, "Q": -5, "P": -3, "S": 0, "R": -4, "T": -2, "W": -8, "V": -2, "Y": 0},
        "E": {"A": 0, "C": -5, "E": 4, "D": 3, "G": 0, "F": -5, "I": -2, "H": 1, "K": 0, "M": -2, "L": -3, "N": 1, "Q": 2, "P": -1, "S": 0, "R": -1, "T": 0, "W": -7, "V": -2, "Y": -4},
        "D": {"A": 0, "C": -5, "E": 3, "D": 4, "G": 1, "F": -6, "I": -2, "H": 1, "K": 0, "M": -3, "L": -4, "N": 2, "Q": 2, "P": -1, "S": 0, "R": -1, "T": 0, "W": -7, "V": -2, "Y": -4},
        "G": {"A": 1, "C": -3, "E": 0, "D": 1, "G": 5, "F": -5, "I": -3, "H": -2, "K": -2, "M": -3, "L": -4, "N": 0, "Q": -1, "P": 0, "S": 1, "R": -3, "T": 0, "W": -7, "V": -1, "Y": -5},
        "F": {"A": -3, "C": -4, "E": -5, "D": -6, "G": -5, "F": 9, "I": 1, "H": -2, "K": -5, "M": 0, "L": 2, "N": -3, "Q": -5, "P": -5, "S": -3, "R": -4, "T": -3, "W": 0, "V": -1, "Y": 7},
        "I": {"A": -1, "C": -2, "E": -2, "D": -2, "G": -3, "F": 1, "I": 5, "H": -2, "K": -2, "M": 2, "L": 2, "N": -2, "Q": -2, "P": -2, "S": -1, "R": -2, "T": 0, "W": -5, "V": 4, "Y": -1},
        "H": {"A": -1, "C": -3, "E": 1, "D": 1, "G": -2, "F": -2, "I": -2, "H": 6, "K": 0, "M": -2, "L": -2, "N": 2, "Q": 3, "P": 0, "S": -1, "R": 2, "T": -1, "W": -3, "V": -2, "Y": 0},
        "K": {"A": -1, "C": -5, "E": 0, "D": 0, "G": -2, "F": -5, "I": -2, "H": 0, "K": 5, "M": 0, "L": -3, "N": 1, "Q": 1, "P": -1, "S": 0, "R": 3, "T": 0, "W": -3, "V": -2, "Y": -4},
        "M": {"A": -1, "C": -5, "E": -2, "D": -3, "G": -3, "F": 0, "I": 2, "H": -2, "K": 0, "M": 6, "L": 4, "N": -2, "Q": -1, "P": -2, "S": -2, "R": 0, "T": -1, "W": -4, "V": 2, "Y": -2},
        "L": {"A": -2, "C": -6, "E": -3, "D": -4, "G": -4, "F": 2, "I": 2, "H": -2, "K": -3, "M": 4, "L": 6, "N": -3, "Q": -2, "P": -3, "S": -3, "R": -3, "T": -2, "W": -2, "V": 2, "Y": -1},
        "N": {"A": 0, "C": -4, "E": 1, "D": 2, "G": 0, "F": -3, "I": -2, "H": 2, "K": 1, "M": -2, "L": -3, "N": 2, "Q": 1, "P": 0, "S": 1, "R": 0, "T": 0, "W": -4, "V": -2, "Y": -2},
        "Q": {"A": 0, "C": -5, "E": 2, "D": 2, "G": -1, "F": -5, "I": -2, "H": 3, "K": 1, "M": -1, "L": -2, "N": 1, "Q": 4, "P": 0, "S": -1, "R": 1, "T": -1, "W": -5, "V": -2, "Y": -4},
        "P": {"A": 1, "C": -3, "E": -1, "D": -1, "G": 0, "F": -5, "I": -2, "H": 0, "K": -1, "M": -2, "L": -3, "N": 0, "Q": 0, "P": 6, "S": 1, "R": 0, "T": 0, "W": -6, "V": -1, "Y": -5},
        "S": {"A": 1, "C": 0, "E": 0, "D": 0, "G": 1, "F": -3, "I": -1, "H": -1, "K": 0, "M": -2, "L": -3, "N": 1, "Q": -1, "P": 1, "S": 2, "R": 0, "T": 1, "W": -2, "V": -1, "Y": -3},
        "R": {"A": -2, "C": -4, "E": -1, "D": -1, "G": -3, "F": -4, "I": -2, "H": 2, "K": 3, "M": 0, "L": -3, "N": 0, "Q": 1, "P": 0, "S": 0, "R": 6, "T": -1, "W": 2, "V": -2, "Y": -4},
        "T": {"A": 1, "C": -2, "E": 0, "D": 0, "G": 0, "F": -3, "I": 0, "H": -1, "K": 0, "M": -1, "L": -2, "N": 0, "Q": -1, "P": 0, "S": 1, "R": -1, "T": 3, "W": -5, "V": 0, "Y": -3},
        "W": {"A": -6, "C": -8, "E": -7, "D": -7, "G": -7, "F": 0, "I": -5, "H": -3, "K": -3, "M": -4, "L": -2, "N": -4, "Q": -5, "P": -6, "S": -2, "R": 2, "T": -5, "W": 17, "V": -6, "Y": 0},
        "V": {"A": 0, "C": -2, "E": -2, "D": -2, "G": -1, "F": -1, "I": 4, "H": -2, "K": -2, "M": 2, "L": 2, "N": -2, "Q": -2, "P": -1, "S": -1, "R": -2, "T": 0, "W": -6, "V": 4, "Y": -2},
        "Y": {"A": -3, "C": 0, "E": -4, "D": -4, "G": -5, "F": 7, "I": -1, "H": 0, "K": -4, "M": -2, "L": -1, "N": -2, "Q": -4, "P": -5, "S": -3, "R": -4, "T": -3, "W": 0, "V": -2, "Y": 10}
      }
    """.parseJson.asJsObject.fields.map {
    case (k, v) ⇒
      k(0) -> v.asJsObject.fields.map {
        case (k, v) ⇒
          k(0) -> v.toString().toInt

      }
  }

  def localAlignment(v: String, w: String, scoring: Map[Char, Map[Char, Int]] = pam250, sigma: Int = 5): (Int, (String, String)) = {
    @tailrec def output(backtrack: IndexedSeq[IndexedSeq[Char]], i: Int, j: Int, rv: StringBuilder, rw: StringBuilder): (String, String) = {
      backtrack(i)(j) match {
        case '↓' ⇒ output(backtrack, i - 1, j, rv.append(v(i - 1)), rw.append('-'))
        case '→' ⇒ output(backtrack, i, j - 1, rv.append('-'), rw.append(w(j - 1)))
        case '↘' ⇒ output(backtrack, i - 1, j - 1, rv.append(v(i - 1)), rw.append(w(j - 1)))
        case 'B' ⇒ (rv.toString.reverse, rw.toString.reverse)
      }
    }
    val n = v.length
    val m = w.length
    var s: IndexedSeq[IndexedSeq[Int]] = IndexedSeq.tabulate(n + 1, m + 1) {
      case (0, 0) ⇒ 0
      case (i, 0) ⇒ i * -sigma
      case (0, j) ⇒ j * -sigma
      case _      ⇒ 0
    }
    var backtrack = IndexedSeq.fill(n + 1, m + 1)(' ')
    for (i ← 1 to n) {
      for (j ← 1 to m) {
        val sc = scoring(v(i - 1))(w(j - 1))

        val possibilities = Seq(
          (0, 'B'),
          (s(i - 1)(j) - sigma, '↓'),
          (s(i)(j - 1) - sigma, '→'),
          (s(i - 1)(j - 1) + sc, '↘')
        )
        val max = possibilities.maxBy(_._1)
        s = s.updated(i, s(i).updated(j, max._1))
        backtrack = backtrack.updated(i, backtrack(i).updated(j, max._2))
      }
    }
    val allMax = s.map(_.max).max
    val index = s.flatten.indexOf(allMax)
    val (r, c) = (index / (m + 1), index % (m + 1))
    (s(r)(c), output(backtrack, r, c, new StringBuilder, new StringBuilder))
  }

  def editDistance(v: String, w: String): Int = {
    val n = v.length
    val m = w.length
    var d: IndexedSeq[IndexedSeq[Int]] = IndexedSeq.tabulate(n + 1, m + 1) {
      case (0, 0) ⇒ 0
      case (i, 0) ⇒ i
      case (0, j) ⇒ j
      case _      ⇒ 0
    }
    for (i ← 1 to n) {
      for (j ← 1 to m) {
        if (v(i - 1) == w(j - 1)) {
          d = d.updated(i, d(i).updated(j, d(i - 1)(j - 1)))
        } else {
          val possibilities = Seq(
            d(i - 1)(j) + 1,
            d(i)(j - 1) + 1,
            d(i - 1)(j - 1) + 1
          )
          val min = possibilities.min
          d = d.updated(i, d(i).updated(j, min))
        }
      }
    }
    d(n)(m)
  }
}
