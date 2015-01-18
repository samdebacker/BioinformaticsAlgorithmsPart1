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

import scala.annotation.tailrec

object Chapter6 {
  type Permutation = IndexedSeq[Int]

  def greedySorting(p: Permutation): Seq[Permutation] = {
    @tailrec def reverseInversed(s: Seq[Int], r: Seq[Int] = Seq.empty[Int]): Seq[Int] = {
      if (s.isEmpty) r
      else reverseInversed(s.tail, -s.head +: r)
    }
    (0 until p.length).foldLeft(Seq(p)) { (result, kMinus1) ⇒
      var p = result.head
      var r = result
      if (kMinus1 + 1 != p(kMinus1)) {
        val positionOfK = p.indexWhere(math.abs(_) == kMinus1 + 1)
        p = (p.view(0, kMinus1) ++ reverseInversed(p.view(kMinus1, positionOfK + 1)) ++ p.view(positionOfK + 1, p.length)).toIndexedSeq
        r = p +: r
      }
      if (p(kMinus1) < 0) {
        p = (p.view(0, kMinus1) ++ (-p(kMinus1) +: p.view(kMinus1 + 1, p.length))).toIndexedSeq
        r = p +: r
      }
      r
    }.reverse.tail
  }

  def countBreakpoints(p: Permutation): Int = {
    (0 +: p).zip(p :+ (p.length + 1)).foldLeft(0) {
      case (result, (left, right)) ⇒ result + (if (right - left == 1) 0 else 1)
    }
  }

  type Cycle = IndexedSeq[Int]

  def chromosoneToCycle(p: Permutation): Cycle = {
    p.zipWithIndex.flatMap {
      case (chromosone, index) ⇒
        if (chromosone > 0) Seq(2 * chromosone - 1, 2 * chromosone)
        else Seq(-2 * chromosone, -2 * chromosone - 1)
    }
  }

  def cycleToChromosone(c: Cycle): Permutation = {
    @tailrec def cycleToChromosone_(c: Cycle, result: Cycle): Cycle = {
      if (c.isEmpty) result
      else {
        val twoNodes = c.take(2)
        cycleToChromosone_(c.drop(2), result :+ (if (twoNodes(0) < twoNodes(1)) twoNodes(1) / 2 else -twoNodes(0) / 2))
      }
    }
    cycleToChromosone_(c, IndexedSeq.empty[Int])
  }

  type Genome = Seq[Permutation]
  type GenomePath = Seq[(Int, Int)]

  def coloredEdges(g: Genome): GenomePath = {
    g.foldLeft(Seq.empty[(Int, Int)]) {
      case (result, permutation) ⇒
        val nodes = chromosoneToCycle(permutation :+ permutation.head)
        (1 to permutation.length).foldLeft(result) { (result, j) ⇒
          result :+ (nodes(2 * j - 1), nodes(2 * j))
        }
    }
  }

  def graphToGenome(gp: GenomePath): Genome = {
    val blacks = (1 to gp.length).map { i ⇒ (2 * i - 1, 2 * i) }
    blacks.zip(gp).foldLeft(Seq(IndexedSeq.empty[Int])) {
      case (cycles, ((bl, br), (cl, cr))) ⇒
        val toAdd = if (br == cl) Seq(bl, br) else Seq(br, bl)
        val newHead = cycles.head ++ toAdd
        var result = newHead +: cycles.tail
        if (cr < cl) {
          result = IndexedSeq.empty[Int] +: result
        }
        result
    }.tail.reverse.map(cycleToChromosone(_))
  }
}
