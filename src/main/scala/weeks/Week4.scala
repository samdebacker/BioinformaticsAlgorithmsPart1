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
import scala.collection.immutable.ListMap
import scala.util.Random

object Week4 {
  def compositionKmers(text: DNAString, k: Int): IndexedSeq[DNAString] = {
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
    } yield (p, q))
      .sortBy { case (l, _) ⇒ l.value }
  }

  def deBruijn(text: DNAString, k: Int): Map[DNAString, IndexedSeq[DNAString]] = {
    val result = (for (p ← compositionKmers(text, k)) yield (DNAString.unsafeFrom(p.value.take(k - 1)), DNAString.unsafeFrom(p.value.tail)))
      .groupBy(_._1)
      .toIndexedSeq
      .map {
        case (k, v) ⇒
          (k, v.map(_._2))
      }
      .sortBy { case (k, _) ⇒ k.value }
    Map(result: _*)
  }

  def deBruijnFromKmers(kMers: DNAMotif, sort: Boolean = false): Map[DNAString, IndexedSeq[DNAString]] = {
    val k = kMers.k
    val result = (for (kMer ← kMers.value) yield (DNAString.unsafeFrom(kMer.value.take(k - 1)), DNAString.unsafeFrom(kMer.value.tail)))
      .groupBy(_._1)
      .toIndexedSeq
      .map {
        case (k, v) ⇒
          if (sort)
            (k, v.map(_._2))
          else
            (k, v.map(_._2).sortBy(_.value))
      }
      .sortBy { case (k, _) ⇒ k.value }
    Map(result: _*).withDefaultValue(IndexedSeq.empty)
  }

  def eulerianCycle[T](graph: Map[T, Seq[T]]): IndexedSeq[T] = {
    @tailrec def cycle_(start: T, graph: Map[T, Seq[T]], cycle: IndexedSeq[T]): (IndexedSeq[T], Map[T, Seq[T]]) = {
      val nextNode = graph(cycle.last).head
      val removedEdgeGraph: Map[T, Seq[T]] = graph.updated(cycle.last, graph(cycle.last).tail)
      if (nextNode == start) {
        (cycle :+ nextNode, removedEdgeGraph)
      } else {
        cycle_(start, removedEdgeGraph, cycle :+ nextNode)
      }
    }
    @tailrec def eulerianCycle_(cycle: IndexedSeq[T], restGraph: Map[T, Seq[T]]): IndexedSeq[T] = {
      if (restGraph.values.map(_.size).sum == 0) {
        cycle
      } else {
        val node: T = cycle.find { node ⇒
          restGraph(node).size > 0
        }.get
        val nodeIndex = cycle.indexOf(node)
        val (newCycle, newRestGraph) = cycle_(node, restGraph, IndexedSeq(node))
        val mergedCycle = (cycle.view(0, nodeIndex) ++ newCycle ++ cycle.view(nodeIndex + 1, cycle.size)).toIndexedSeq
        eulerianCycle_(mergedCycle, newRestGraph)
      }
    }
    val firstNode: T = graph.head._1
    val (cycle, restGraph) = cycle_(firstNode, graph, IndexedSeq(firstNode))
    eulerianCycle_(cycle, restGraph)
  }

  def eulerianPath[T](graph: Map[T, Seq[T]]): IndexedSeq[T] = {
    @tailrec def increase(nodes: Seq[T], indegrees: Map[T, Int]): Map[T, Int] = {
      if (nodes.isEmpty) indegrees
      else increase(nodes.tail, indegrees.updated(nodes.head, indegrees(nodes.head) + 1))
    }
    @tailrec def inDegreesOf(graph: Map[T, Seq[T]], inDegrees: Map[T, Int]): Map[T, Int] = {
      if (graph.isEmpty) inDegrees
      else {
        inDegreesOf(graph.tail, increase(graph.head._2, inDegrees))
      }
    }
    val nodes = ((for {
      edges ← graph
      node ← edges._2
    } yield node) ++ graph.keys).toSet[T].toSeq
    val inDegrees = inDegreesOf(graph, Map(nodes.map { node ⇒ (node, 0) }: _*))
    val nodeWithInDegreeLTOutDegree_To = inDegrees.find {
      case (node, inDegree) ⇒ inDegree < graph(node).size
    }.get._1
    val nodeWithInDegreeGTOutDegree_From = inDegrees.find {
      case (node, inDegree) ⇒ inDegree > graph(node).size
    }.get._1
    val adjustedGraph = graph.updated(nodeWithInDegreeGTOutDegree_From, nodeWithInDegreeLTOutDegree_To +: graph(nodeWithInDegreeGTOutDegree_From))
    val cycle = eulerianCycle(adjustedGraph)
    var cutPoint = -1
    for (i ← 0 until cycle.size - 1) {
      if (cycle(i) == nodeWithInDegreeGTOutDegree_From && cycle(i + 1) == nodeWithInDegreeLTOutDegree_To) {
        cutPoint = i + 1
      }
    }
    val (head, tail) = cycle.splitAt(cutPoint)
    if (tail.size > 1) tail.init ++ head else tail ++ head.tail
  }

  def stringReconstruction(kMers: DNAMotif): DNAString = {
    val graph = deBruijnFromKmers(kMers)
    val path = eulerianPath(graph)
    stringSpelledByGenomePath(DNAMotif.unsafeFrom(path))
  }

  def binaryStrings(k: Int): IndexedSeq[String] = {
    for (n ← 0 to (math.pow(2, k).toInt - 1)) yield n.toBinaryString.reverse.padTo(k, '0').reverse
  }
  def kUniversalCirculairString(k: Int): String = {
    def deBruijnFromKmers(kMers: IndexedSeq[String]): Map[String, IndexedSeq[String]] = {
      val k = kMers.head.length
      val result = (for (kMer ← kMers) yield (kMer.take(k - 1), kMer.tail))
        .groupBy(_._1)
        .toIndexedSeq
        .map {
          case (k, v) ⇒
            (k, v.map(_._2).sortBy(s ⇒ s))
        }
        .sortBy(_._1)
      // /!\ using a ListMap here to preserve the insertion order
      // that will let us start with the 000's which is a good candidate to find
      // a CIRCULAR k-Universal String (faster), and reduces the number of possible answers
      ListMap(result: _*).withDefaultValue(IndexedSeq.empty)
    }
    def generateKUniversalString(graph: Map[String, Seq[String]]): String = {
      @tailrec def cycle_(start: String, graph: Map[String, Seq[String]], cycle: IndexedSeq[String]): (IndexedSeq[String], Map[String, Seq[String]]) = {
        val selectableNodes = graph(cycle.last)
        // Go randomly select a possible edge, we could do it exhaustively but we need to implement a backtracking algorithm
        val nextNode = selectableNodes(Random.nextInt(selectableNodes.length))
        val removedEdgeGraph: Map[String, Seq[String]] = graph.updated(cycle.last, selectableNodes.filterNot(_ == nextNode))
        if (nextNode == start) {
          (cycle :+ nextNode, removedEdgeGraph)
        } else {
          cycle_(start, removedEdgeGraph, cycle :+ nextNode)
        }
      }
      @tailrec def eulerianCycle_(cycle: IndexedSeq[String], restGraph: Map[String, Seq[String]]): IndexedSeq[String] = {
        if (restGraph.values.map(_.size).sum == 0) {
          cycle
        } else {
          val node: String = cycle.find { node ⇒
            restGraph(node).size > 0
          }.get
          val nodeIndex = cycle.indexOf(node)
          val (newCycle, newRestGraph) = cycle_(node, restGraph, IndexedSeq(node))
          val mergedCycle = (cycle.view(0, nodeIndex) ++ newCycle ++ cycle.view(nodeIndex + 1, cycle.size)).toIndexedSeq
          eulerianCycle_(mergedCycle, newRestGraph)
        }
      }
      // Loop until we have a good candidate (would be nice to walk all the possible path in a deterministic way)
      var found = false
      var candidate: String = null
      while (!found) {
        val firstNode: String = graph.head._1
        val (cycle, restGraph) = cycle_(firstNode, graph, IndexedSeq(firstNode))
        val currentCycle = eulerianCycle_(cycle, restGraph)
        candidate = stringSpelledByPath(currentCycle)
        found = candidate.endsWith(candidate.substring(0, k - 1)) // checks if we have a circular k-Universal String
        //println(s"$candidate is $found")
      }
      candidate.substring(0, candidate.length - (k - 1))
    }
    def stringSpelledByPath(path: IndexedSeq[String]): String = {
      val k = path.head.length
      path.tail.foldLeft(new StringBuilder(path.head)) { (acc, el) ⇒
        acc.append(el.charAt(k - 1))
      }.toString
    }
    val graph = deBruijnFromKmers(binaryStrings(k))
    generateKUniversalString(graph)
  }

  def pairedCompostion(k: Int, d: Int)(text: DNAString): IndexedSeq[(DNAString, DNAString)] = {
    val t = text.value
    (for (i ← 0 to t.length - (2 * k + d)) yield (t.substring(i, i + k), t.substring(i + k + d, i + k + d + k)))
      .sorted
      .map { case (l, r) ⇒ (DNAString.unsafeFrom(l), DNAString.unsafeFrom(r)) }
  }

  def stringSpelledByKDmerPath(k: Int, d: Int)(path: IndexedSeq[(DNAString, DNAString)]): DNAString = {
    val addLastCharacter = { (acc: StringBuilder, el: String) ⇒ acc.append(el.charAt(k - 1)) }
    val left = path.map(_._1.value)
    var builder = left.tail.foldLeft(new StringBuilder(left.head))(addLastCharacter)
    val right = path.view(path.length - d - k, path.length).map(_._2.value)
    builder = right.foldLeft(builder)(addLastCharacter)
    DNAString.unsafeFrom(builder.toString())
  }

  def stringReconstructionFromKDMers(k: Int, d: Int)(kdMers: IndexedSeq[(DNAString, DNAString)]): DNAString = {
    def deBruijnFromKDmers: Map[(DNAString, DNAString), IndexedSeq[(DNAString, DNAString)]] = {
      val result = (for ((left, right) ← kdMers) yield ((left.value.take(k - 1), right.value.take(k - 1)), (left.value.tail, right.value.tail)))
        .groupBy(_._1)
        .toIndexedSeq
        .sortBy { case (k, _) ⇒ k }
        .map {
          case (k, v) ⇒
            ((DNAString.unsafeFrom(k._1), DNAString.unsafeFrom(k._2)),
              v.sortBy(_._2).map {
                case (_, (l, r)) ⇒
                  (DNAString.unsafeFrom(l), DNAString.unsafeFrom(r))
              })
        }
      Map(result: _*).withDefaultValue(IndexedSeq.empty[(DNAString, DNAString)])
    }

    val graph = deBruijnFromKDmers
    val path = eulerianPath(graph)
    stringSpelledByKDmerPath(k - 1, d + 1)(path)
  }

  /*
  def maximalNonBranchingPaths[T](graph: Map[T, Seq[T]]): Set[Seq[T]] = {
    @tailrec def increase(nodes: Seq[T], indegrees: Map[T, Int]): Map[T, Int] = {
      if (nodes.isEmpty) indegrees
      else increase(nodes.tail, indegrees.updated(nodes.head, indegrees(nodes.head) + 1))
    }
    @tailrec def inDegreesOf(graph: Map[T, Seq[T]], inDegrees: Map[T, Int]): Map[T, Int] = {
      if (graph.isEmpty) inDegrees
      else {
        inDegreesOf(graph.tail, increase(graph.head._2, inDegrees))
      }
    }
    val nodes = ((for {
      edges ← graph
      node ← edges._2
    } yield node) ++ graph.keys).toSet[T].toSeq
    val inDegrees = inDegreesOf(graph, Map(nodes.map { node ⇒ (node, 0) }: _*))

    val startNodes = inDegrees.filter { case (node, inDegree) ⇒
      val outDegree = graph(node).size
      outDegree > 0 && inDegree != 1
    }
    startNodes.foldLeft(Set.empty[Seq[T]]) { case (paths, (node, _)) ⇒
      constructPath
    }

    val nodeWithInDegreeLTOutDegree_To = inDegrees.find {
      case (node, inDegree) ⇒ inDegree < graph(node).size
    }.get._1
    val nodeWithInDegreeGTOutDegree_From = inDegrees.find {
      case (node, inDegree) ⇒ inDegree > graph(node).size
    }.get._1
    val adjustedGraph = graph.updated(nodeWithInDegreeGTOutDegree_From, nodeWithInDegreeLTOutDegree_To +: graph(nodeWithInDegreeGTOutDegree_From))
    val cycle = eulerianCycle(adjustedGraph)
    var cutPoint = -1
    for (i ← 0 until cycle.size - 1) {
      if (cycle(i) == nodeWithInDegreeGTOutDegree_From && cycle(i + 1) == nodeWithInDegreeLTOutDegree_To) {
        cutPoint = i + 1
      }
    }
    val (head, tail) = cycle.splitAt(cutPoint)
    if (tail.size > 1) tail.init ++ head else tail ++ head.tail
  }
  */
}
