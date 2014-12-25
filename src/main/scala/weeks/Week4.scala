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

package weeks

import scala.annotation.tailrec

object Week4 {
  def compositionKmers(text: DNAString, k: Int) = {
    @tailrec def kMers_(text: String, result: IndexedSeq[DNAString]): IndexedSeq[DNAString] = {
      if (text.length < k)
        result
      else
        kMers_(text.tail, result :+ DNAString.from(text.take(k)).get)
    }
    kMers_(text.value, IndexedSeq.empty[DNAString]).sortBy(_.value)
  }

  def stringSpelledByGenomePath(path: DNAMotif): DNAString = {
    val k = path.k
    DNAString.from(path.value.tail.foldLeft(new StringBuilder(path.value.head.value)) { (acc, el) ⇒
      acc.append(el.value.charAt(k - 1))
    }.toString).get
  }

  def overlap(patterns: DNAMotif): IndexedSeq[(DNAString, DNAString)] = {
    val k = patterns.k
    (for {
      p ← patterns.value
      q ← patterns.value if q.value.take(k - 1) == p.value.tail
    } yield (p, q)).sortBy { case (l, _) ⇒ l.value }
  }

  def deBruijn(text: DNAString, k: Int): IndexedSeq[(DNAString, IndexedSeq[DNAString])] = {
    import weeks.DNAString.StringToDNAString
    (for {
      p ← compositionKmers(text, k)
    } yield (p.value.take(k - 1).toDNA, p.value.tail.toDNA))
      .groupBy(_._1.value)
      .toIndexedSeq
      .map {
        case (k, v) ⇒
          (k.toDNA, v.map(_._2))
      }
      .sortBy { case (k, _) ⇒ k.value }
  }

  def deBruijnFromKmers(kMers: DNAMotif, sort: Boolean = false): IndexedSeq[(DNAString, IndexedSeq[DNAString])] = {
    import weeks.DNAString.StringToDNAString
    val k = kMers.k
    (for {
      kMer ← kMers.value
    } yield (kMer.value.take(k - 1).toDNA, kMer.value.tail.toDNA))
      .groupBy(_._1.value)
      .toIndexedSeq
      .map {
        case (k, v) ⇒
          if (sort)
            (k.toDNA, v.map(_._2))
          else
            (k.toDNA, v.map(_._2).sortBy(_.value))
      }
      .sortBy { case (k, _) ⇒ k.value }
  }

  def eulerianCycle(graph: IndexedSeq[Seq[Int]]): IndexedSeq[Int] = {
    @tailrec def cycle_(start: Int, graph: IndexedSeq[Seq[Int]], cycle: IndexedSeq[Int]): (IndexedSeq[Int], IndexedSeq[Seq[Int]]) = {
      val nextNode = graph(cycle.last).head
      val removedEdgeGraph: IndexedSeq[Seq[Int]] = graph.updated(cycle.last, graph(cycle.last).tail)
      if (nextNode == start) {
        (cycle :+ nextNode, removedEdgeGraph)
      } else {
        cycle_(start, removedEdgeGraph, cycle :+ nextNode)
      }
    }
    @tailrec def eulerianCycle_(cycle: IndexedSeq[Int], restGraph: IndexedSeq[Seq[Int]]): IndexedSeq[Int] = {
      if (restGraph.map(_.size).sum == 0) {
        cycle
      } else {
        val node: Int = cycle.find { node =>
          restGraph(node).size > 0
        }.get
        val nodeIndex = cycle.indexOf(node)
        //println("nodeIndex = " + nodeIndex)
        val (newCycle, newRestGraph) = cycle_(node, restGraph, IndexedSeq(node))
        //println(cycle(nodeIndex) + " " + newCycle.tail.mkString("->") + " " + cycle.drop(nodeIndex + 1).mkString("->") + " " + cycle.tail.take(nodeIndex).mkString("->"))
        val mergedCycle = (cycle(nodeIndex) +: newCycle.tail) ++ cycle.drop(nodeIndex + 1) ++ cycle.tail.take(nodeIndex)
        //println(mergedCycle.mkString("->"))
        //println(newRestGraph)
        eulerianCycle_(mergedCycle, newRestGraph)
      }
    }
    val (cycle, restGraph) = cycle_(0, graph, IndexedSeq(0))
    eulerianCycle_(cycle, restGraph)
  }
}
