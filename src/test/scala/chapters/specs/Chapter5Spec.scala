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

package specs

import chapters.Chapter5._
import chapters.DNAString
import org.scalatest.{ FeatureSpec, Matchers }

import scala.io.Source

class Chapter5Spec extends FeatureSpec with Matchers {
  feature("dpChange") {
    scenario("example") {
      dpChange(40, Set(50, 25, 20, 10, 5, 1)) shouldBe 2
    }
    scenario("extra dataset") {
      dpChange(8074, Set(24, 13, 12, 7, 5, 3, 1)) shouldBe 338
    }
    scenario("interactive quiz") {
      dpChange(17907, Set(24, 21, 20, 18, 13, 12, 5, 3, 1)) shouldBe 747
    }
  }

  feature("manhattanTourist") {
    scenario("example") {
      val n = 4
      val m = 4
      val down = IndexedSeq(
        IndexedSeq(1, 0, 2, 4, 3),
        IndexedSeq(4, 6, 5, 2, 1),
        IndexedSeq(4, 4, 5, 2, 1),
        IndexedSeq(5, 6, 8, 5, 3)
      )
      val right = IndexedSeq(
        IndexedSeq(3, 2, 4, 0),
        IndexedSeq(3, 2, 4, 2),
        IndexedSeq(0, 7, 3, 3),
        IndexedSeq(3, 3, 0, 2),
        IndexedSeq(1, 3, 2, 2)
      )
      manhattanTourist(n, m)(down, right) shouldBe 34
    }

    scenario("extra dataset") {
      val n = 17
      val m = 9
      val downRaw = """2 3 4 0 3 1 1 1 1 1
                      |4 2 3 4 3 3 0 4 1 1
                      |4 4 0 1 4 3 2 0 2 2
                      |4 3 0 3 4 4 3 2 4 4
                      |0 1 0 1 3 0 3 0 3 4
                      |3 2 4 4 4 3 1 0 0 0
                      |3 4 3 1 2 3 0 0 4 0
                      |2 4 3 4 1 2 0 3 2 0
                      |1 4 4 1 4 4 3 1 1 4
                      |3 1 2 2 3 3 0 4 0 0
                      |0 2 1 4 1 3 1 3 1 0
                      |1 0 4 0 4 3 3 2 3 1
                      |2 0 0 4 3 4 0 3 0 0
                      |4 1 0 4 3 2 1 1 3 1
                      |2 4 4 3 3 4 0 0 4 3
                      |1 0 2 3 3 0 4 0 2 0
                      |3 1 0 3 2 3 2 2 1 4""".stripMargin.split("[^\\d]+").map(_.toInt)
      val down = IndexedSeq.tabulate(n, m + 1) { (i, j) ⇒ downRaw(i * (m + 1) + j) }
      val rightRaw = """1 0 4 4 3 3 1 0 4
                      |0 2 0 3 3 0 1 2 1
                      |3 2 3 1 1 4 2 4 4
                      |1 3 4 4 2 1 1 1 4
                      |1 4 2 2 3 1 3 2 3
                      |0 3 1 0 1 0 4 1 4
                      |1 3 4 4 1 0 3 2 1
                      |2 3 1 2 3 2 2 2 3
                      |3 2 1 4 0 2 4 2 4
                      |4 0 2 0 1 3 1 4 4
                      |1 3 0 2 2 1 0 3 2
                      |1 4 0 4 4 1 2 4 2
                      |0 2 4 3 4 0 3 2 2
                      |2 3 4 4 0 4 3 0 4
                      |1 0 4 1 3 3 1 4 2
                      |4 3 4 3 2 3 2 2 0
                      |0 1 2 2 4 4 2 4 2
                      |2 3 1 4 4 3 4 0 3""".stripMargin.split("[^\\d]+").map(_.toInt)
      val right = IndexedSeq.tabulate(n + 1, m) { (i, j) ⇒ rightRaw((i * m) + j) }
      manhattanTourist(n, m)(down, right) shouldBe 84
    }

    scenario("interactive quiz") {
      val n = 14
      val m = 14
      val downRaw = """0 3 2 1 0 0 4 2 2 0 4 0 1 1 0
                      |1 1 3 0 3 4 0 1 3 1 1 4 1 0 4
                      |1 4 3 3 4 4 2 3 4 3 3 3 4 3 4
                      |3 3 1 3 1 3 0 2 2 4 3 2 2 1 3
                      |4 3 3 4 3 3 3 0 2 1 0 1 2 3 2
                      |0 1 3 4 3 1 0 3 3 3 1 2 0 3 3
                      |0 3 2 3 3 0 3 2 0 3 1 2 1 4 3
                      |2 4 0 2 0 4 2 3 0 4 1 3 3 2 0
                      |1 2 4 4 3 2 3 1 1 1 2 4 2 4 3
                      |4 3 0 2 1 1 0 2 2 3 0 2 0 1 3
                      |0 1 2 1 3 4 2 0 3 3 0 4 4 3 4
                      |1 4 1 0 3 1 1 3 2 0 2 2 1 4 4
                      |1 2 3 0 2 3 2 3 0 1 2 1 0 1 3
                      |0 1 1 4 2 1 3 1 3 0 4 4 4 1 1""".stripMargin.split("[^\\d]+").map(_.toInt)
      val down = IndexedSeq.tabulate(n, m + 1) { (i, j) ⇒ downRaw(i * (m + 1) + j) }
      val rightRaw = """2 0 4 2 0 1 4 3 0 4 1 3 2 4
                       |1 3 1 1 3 3 3 0 2 4 1 2 3 4
                       |2 1 4 4 4 3 4 0 4 4 4 1 3 0
                       |1 4 3 2 2 3 1 2 4 0 2 0 2 1
                       |1 1 2 1 4 1 1 4 0 0 1 1 4 3
                       |0 1 2 0 4 0 2 2 3 1 1 2 2 1
                       |1 0 0 1 4 2 0 1 0 1 1 0 1 2
                       |1 2 2 2 0 0 3 0 1 2 3 3 1 4
                       |1 3 4 2 0 4 1 1 2 2 4 4 2 1
                       |3 2 4 1 2 0 4 2 1 0 0 3 1 2
                       |0 4 4 1 2 1 0 0 2 3 1 0 3 1
                       |4 2 1 4 3 4 4 2 3 0 3 2 3 3
                       |1 1 3 4 0 4 3 2 4 0 3 3 4 1
                       |4 1 2 2 1 1 0 4 1 3 0 1 4 3
                       |1 4 0 3 4 2 2 2 1 2 0 4 1 2""".stripMargin.split("[^\\d]+").map(_.toInt)
      val right = IndexedSeq.tabulate(n + 1, m) { (i, j) ⇒ rightRaw((i * m) + j) }
      manhattanTourist(n, m)(down, right) shouldBe 87
    }
  }

  feature("longestCommonSubsequence") {
    scenario("example") {
      val v = DNAString("AACCTTGG")
      val w = DNAString("ACACTGTGA")
      longestCommonSubsequence(v, w) shouldBe DNAString("AACTTG")
    }
    scenario("extra dataset") {
      val v = DNAString("ACCGTCTTAGCGATCAACACATTTAACAACGCGCCGCACCCCCCGTCAAACGAGCTTTTGGGCTCTTGTCCTTTTACAAGCTTCACGACGCATACAGCCTTGATCAACGGTTTGATCTGTCTCCCTTCAGCTGGCTTTAAAGGACATACATATGAAGGCCTTAATAAGGTCCGGGAACTCCACATATTCGGTACTGGGCAAACCCCATGAACCACCTCAACATGAAGAGTCCGAGGACTCTCACGATCCACCAATGCAGATCGGAACTGTGCGATCGCGTAATGAGCCGAGTACTTGGTTTGTGTTTAGGTTATGGGGGCCGGGAGCCGGTTCAATATAAGGAAGTAGTTGCAGATTAGTTGTTGCGAACGGTCATAAATTTGATGGGTAAACGTGAACTTAACAAACCGTGATAGCTAATCCTATGCATCCCTTACGTGGATCGACTCGAGTACCCAGGTGAACCGACTACTTGATAACCGGAAATCGCGGTATAAAAGCGCTCACGGTCAGGAGATATACCTCCAAGCAGTAGTCTTTCTGAGCCTAGAGTAGTAAATTACAGGGACGATGTCTTTTACCGAGGCAACATTTTATTGAGAATCACATGAGGCACAGGTAAAGGCGACATCACGATCGAGATCAACCCCTACTTGTTCAAAACATTGAGAACCAGCTCTGTTTTGGAACCTAGAAAGATAACGCATCCGCTTGATATTCCACGGCTTGTCCCTCTTGTGCGGTCCATCTATCGGAGTTTCCTCCGATACGACCCGCAATGTTTCCAGGCGTACGGTACTTTATGAATACACTCGCGCTGTAACCTGTTATGTGAAACACACACGACAGAGCTTCGCGTGGGCCCAGCGACCCGGTAATACTACATCACCGCACACGACCTCGAGCAGTCTTTGCCGGCGTCCGTAAGTAGTCTAAAGTTGTGTTGATGCTTGGGGTTAAAGCTAAATCGTCCGCAGAATACGACTCTCATCCCAAT")
      val w = DNAString("ACCCGCACGCGCTTTGGTCTAGATTCTAGCTCCAACTTGCCTGCTAGATACTCTGTTAAAAGATGGTTTTACAACCCCCTCCTCTGTCCCTGGGGTATTATATAATACGTCGGATAGTCAGGTACAAATACAAGTGGGTGGGAATACTTTTCCTCGGATCCTAGACCACGGATTACTGCGTGGTTGACAAGAGTCGGCCCGGAGGGAAACGTGAAGGTTAGTGCAATTAAAGTCTCTAATGTGAAGCCTCCGCGAAGCGAGGAGTTTCTGAGATCGAGTACTATTTAGAGTTCGAAATCACGGCTTAACCTCACTGCCACGCATAACTTGCCGGCAATCCAGTTTTGCAACGATACTTAATTTGTGCAGCTCATCTTTGCTGTCCAGAAATAGAGCTAGTCGATCTCATCTTGCGGGTAGCCAGAAGTCCTACCGTCTCCTCCATGTAGCTTAAAAATTTCGGTGAGGATCAAAAATGATAAACGTGACAGGTAAGCTCCTACGTCTATCCTATGACCCCCGCGGCAGAATAGGTTGGTAGTGTTAGTGCGTGAGCTGGTAGAATAGAGCACACTTAGGGAAACGGGAACCGTTATGTAGGGCTGCGACACACAAAAAAGTGTTCGTTGGTAAGCTGCCTCTCCACTAAACAGGATTTCTCTGGATGATCCCATCGAAGCAAGTTACGCACCACGCCGAGGCGGACCCTGGTACTAGCTGCCCCCCCCTTTATGGGGCGCTCGTACATCAAGATGATCGCGGACTCAACCTGATTACGAGTTGTCCAAGTAGTCCAGGGTAAGAGAAACTGGAGAGA")
      longestCommonSubsequence(v, w) shouldBe DNAString("ACCGCAGCGTCAATTTACAACGCCGCACCGTAAAGATGGTTTTACAACCCCCTCCCTGTCCGGTTTATTTCTCTAGTCAGGACAAATAAAGTGGTGGGAATACTTTCTCGGACCAGACCACTACTGGTGGTTGACAAGAGTCGGCCCGGAGGGAACTGGTTGTGTTAGTTATGGGCCCCGGAAGGAGAGTTGAGATCGAGTCTATTTGAGTCGAATCACGGCTAACCTATGCACCTACTTGCCGATCCAGTGAACGATACTTATACCATCGCGTAAAAAGGCTAGTCGATATCCTCCAGAGTAGTCTTCTGAGCTAAAAATTCGGGAGATCAAAAATATAAACTGACAGGTAAGCCTACGTCATCAACCCCCGCAAAATTGGAGTGTTTTGGCTAGAAAGAGCACCTTGAAACGGGCCTTTGTGGGTCCACACAGTTTCTGTAAGCTGTTCCACTACGGTCTTATGATCATCGGCAAGTTAGCACCACGCGAGGCGGACCCGGTACTACTCCCCCACGCTCGACATCTTGCGGCTCCTGATTAAGTTGTGTGTCGGGTAAAGAAACTGAGAGA")
    }
    scenario("interactive quiz") {
      val v = DNAString("CACATAACGGCGGCGTGATCTGTATTCGTCCAAGGCCCCTCGATAGCCGCCTGACGAACCCTCTTAGACCTAGTTTGACATATAACGAAGGCATGTAACTAGATGCGTCTTAAATAACGCGGGCAACCGTGTGAAGGAAAGAGCGACAACTCCACCTGGAGATAACGTGACGTTACCTTGATCCTGGGTGAAAACGTTATTCCTTTGCGGGCCGGATACGATGCGTCAGTGGTAGTCTCGGCCCGCCAACCCTCCGCTCTGTTCGCATCTCGTTGATAACCTGCCGGTGTAAGCGCACACATTCCCTTGCATTCTGGTCAAGTCCAGCCAATAAAACGATTAAACCGAGTAATAATCCAGAGTCCCTGTCTACCATCGCATGGCTCCCTTTAGGGCCCGTTCAGGCCGGATAGCGCAGTCGATATAGGCGATCTTCCACTTATTTGAGAGCAGGCGCCCCATCTGATCGCAAGGGACAGAGATTTCGTGTATTTTGAACGCGGCGTCCAGTTAGTCGATGTGCGTGGGCCCTTTTCACGCAGTGTAAATCTGGGCTCAGCCGTGTGAAATAGGCAAATGCAGAGAGTTGCGGAGACTCAGATACATGTTCCCACGTTCAAGGGGTATATATACGGACCCTTCCAAAGGACCGTTCCGAAGAGGGAAACACGTTAAAAGCGTAGAGATGCGCATCGACTTGCATTCTAGACCCACCAGCGAATATTACACCGTTGGAATCAGTACTTTAAATTAACGAGGCAGGATACTGCCCTCGCTTTGAAACACAACTATACAACACCGTGCCGTCCCCATCGTAAACCTAACCCGAACTGTTTTGCGATCCGTGGAGGAGCTAACTGGACAATGACTGGTCGAAATTTTGACGGCTCTTTAGCAGCTTCACGACTTTCAGAACTGGTAACAAGTCA")
      val w = DNAString("GCGATGTGGACAGTTCTCGGCTTGCATTTACGCGACGGTCCGCATATGCGCCCGAATATAGGTTGGCGATGTTGTGCGCTACGCCGACATCGTGGACCGAAGGTCCCCTTGGCTCACAACAAGGCCCCTTCTAACCAGGGTGTCCGCTTTATTTTTTAGATCACGTTTGTAAGCATTCGAAGTTGTGCCAGTAAGTGGGAGTAGCCACGCGTATACCTTTAATAATGAGTTGCGGGTCATCAACCTTATAGTACATAGTAGCCTGTGCAAGTTGGCTCCTATAAGTTGGACAAGTGTGGAATGCGAAGGTTGCCGTTGGGAGACCTTCTTCTCGAGGCGCAGCTGAGATAAGAGGAGCGAATGGGCATTGGAACTTTGTGTAAACCAATGTAAAGTTTTCAGCCTGCCCCGATCCCGCTTTAAACTATCCCTATAAAGCGAAGGCGGCAACACTAAGCGTGTTGTTCTCCAGACTTTCACATAATCGCTACATCATCTAATGATCACTTGAGTACCAACACCTTACTGCCCTTACAGTTCGGGCCGCGGGGGTCTTCGATTATAGCCATGCTAATGTTTAGAACAAGTAACGCATCCTGGTATAGATGGTTCTCGGCCGACAACTAGATCGACAGACAAGCGGGTTAGTAGCACCGGAGACCAGTCTATGGGCTGGATAGGCTACATGTCTTATGTCACTGCTTTCCGACTTTTTTCCTGTAAAGGACACTATGCGCAGGCAGTCTTTTCCAAAGTAAAAACTTAGTTGTGAGCTTATCAGACATTCTGGTAAAGAGTCAAGCACCTGGCGACGC")
      longestCommonSubsequence(v, w) shouldBe DNAString("CATAAGCGGCTGATTTACGCCGGCCCTATGCGCCCGAATATAGGTTGGGATGTTGTGCGCTACGCGCACGTGGAGAAGGTCCCCTGGAAAAGGCTTCTACCGGGTGCGTTATTTTTGCGGGCCGAAGTGCGTAGTGGAGTGCCCGCAACCTTTTTGATTCGTATAACCTTTAGACAATCCTTGCATTGGCTCCATAAGTTAAAGGTAATCGAGTCCGTTAACCTTCTTTAGGCGCAGCGGATAGGAGCGAATGGCGACTTCCATTATTTCAGCGCCCCGATCGCAAACATCTATAAGCGGCGCCATAGCGTGTGCTCCCTTTCACATAATCGCTCACCTAATGATCATTGAGACCAATACTGCCCACGTTCGGGGTTTCGATTAAGCCGTAAGAGAACAGTAAGCGTAAGATGGCTCGCGCACTAGACCACAGCGTATACACCGGGACAGTCTATGGGCGGATACTCTGCTTTGCACCTTCCACTTCCTGTAAACCTACCCAGTTTTTCCAAGTAAAAACTTGTTTGAGCTTTAGCACTTAGATCAGACTGGACGC")
    }
  }

  feature("topologicalOrdering") {
    scenario("example") {
      val graph = Map(
        0 → Seq(1),
        1 → Seq(2),
        3 → Seq(1),
        4 → Seq(2)
      ).withDefaultValue(Seq.empty[Int])
      val nodes = graph.flatMap { case (k, v) ⇒ k +: v }.toSet
      topologicalOrdering(nodes, graph) shouldBe IndexedSeq(0, 3, 4, 1, 2)
    }
    scenario("interactive quiz") {
      def loadGraphOfNodes(fn: String): Map[Int, Seq[Int]] = {
        val regex = """(?i)(\d+) -> ([\d,]+)""".r
        Map(Source.fromFile(fn).getLines().map {
          case regex(left, right) ⇒
            left.toInt → right.split(',').toSeq.map(_.toInt)
        }.toSeq: _*).withDefaultValue(Seq.empty[Int])
      }
      val graph = loadGraphOfNodes("src/main/resources/topologicalOrderingInteractiveQuiz.txt")
      val nodes = graph.flatMap { case (k, v) ⇒ k +: v }.toSet
      topologicalOrdering(nodes, graph) shouldBe IndexedSeq(0, 5, 10, 14, 1, 6, 9, 13, 24, 2, 7, 3, 12, 17, 18, 11, 20, 27, 16, 19, 22, 4, 29, 8, 21, 15, 25, 28, 26, 23, 30, 31)
    }
  }

  feature("longestPath") {
    scenario("example") {
      val source = 0
      val sink = 4
      val graph = IndexedSeq(
        0 → (1, 7),
        0 → (2, 4),
        2 → (3, 2),
        1 → (4, 1),
        3 → (4, 3)
      )
      longestPath(source, sink, graph) shouldBe (9, Seq(0, 2, 3, 4))
    }

    def loadGraphWithWeights(fn: String): IndexedSeq[(Int, (Int, Int))] = {
      val regex = """(?i)(\d+)->([\d]+):([\d]+)""".r
      Source.fromFile(fn).getLines().map {
        case regex(f, t, w) ⇒
          f.toInt → (t.toInt, w.toInt)
      }.toIndexedSeq

    }

    scenario("extra dataset") {
      val source = 0
      val sink = 44
      val graph = loadGraphWithWeights("src/main/resources/longestPathExtraDataset.txt")
      longestPath(source, sink, graph) shouldBe (62, Seq(0, 14, 29, 44))
    }

    scenario("interactive quiz") {
      val source = 2
      val sink = 42
      val graph = loadGraphWithWeights("src/main/resources/longestPathInteractiveQuiz.txt")
      longestPath(source, sink, graph) shouldBe (67, List(2, 13, 19, 36, 42))
    }
  }
}
