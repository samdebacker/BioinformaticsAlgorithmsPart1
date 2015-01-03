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

package chapters

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
          val x = s(i - 1)(j - 1) + (if (v.value.charAt(i - 1) == w.value.charAt(j - 1)) 1 else 0)
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
        case _   ⇒ output(backtrack, i - 1, j - 1, result.append(v.value.charAt(i - 1)))
      }
    }
    output(lcsBacktrack, v.value.length, w.value.length, new StringBuilder)
  }

  def topologicalOrdering(nodes: Set[Int], graph: Map[Int, Seq[Int]]): IndexedSeq[Int] = {
    var inDegrees: Map[Int, Int] = Chapter4.inDegreesOf(graph, Map(nodes.toSeq.map { node ⇒ (node, 0) }: _*))
    var candidates = inDegrees.filter(_._2 == 0).keySet
    var list = IndexedSeq.empty[Int]
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

  def longestPath(source: Int, sink: Int, graph: IndexedSeq[(Int, (Int, Int))]): (Int , Seq[Int]) = {
    val nodes = graph.flatMap { case (k, v) ⇒ Seq(k, v._1) }.toSet
    val sRaw = nodes.toSeq.map(_ → (Int.MinValue, Seq.empty[Int]))
    val g = graph.foldLeft(Map.empty[Int,Seq[Int]].withDefaultValue(Seq.empty[Int])) {
      case (g, (f, (t, _))) ⇒ g.updated(f, g(f) :+ t)
    }
    val initS: Map[Int, (Int, Seq[Int])] = Map(sRaw.toSeq: _*) + (source → (0, Seq(source)))
    val s = topologicalOrdering(nodes, g)
      .foldLeft(initS) { (s, b) ⇒
        val pred = graph.filter { case (f, (t, _)) ⇒ t == b}
        //val pred = g.filter { case (k: Int, v: Seq[Int]) ⇒ v.contains(b) }.keySet
        if (pred.nonEmpty) {
          val max = pred.map { case (f, (t, w))  ⇒
            (s(f)._1 + w, s(f)._2 :+ t)
          }.maxBy(_._1)
          s.updated(b, max)
        } else
          s
      }
    s(sink)
  }
}
