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

package io.ireact.bioinformatics.part1.specs

import io.ireact.bioinformatics.part1.Chapter5._
import io.ireact.bioinformatics.part1.support.DNAString
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
    scenario("quiz") {
      val v = DNAString("TGTACG")
      val w = DNAString("GCTAGT")
      longestCommonSubsequence(v, w) shouldBe DNAString("GTAG")
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

    scenario("quiz") {
      val source = 'a'
      val sink = 'g'
      val graph = IndexedSeq(
        'a' → ('b', 5),
        'a' → ('c', 6),
        'a' → ('d', 5),
        'b' → ('c', 2),
        'b' → ('f', 9),
        'c' → ('e', 4),
        'c' → ('f', 3),
        'c' → ('g', 7),
        'd' → ('e', 4),
        'd' → ('f', 5),
        'e' → ('g', 2),
        'f' → ('g', 1)
      )
      longestPath(source, sink, graph) shouldBe (15, Seq('a', 'b', 'f', 'g'))
    }
  }

  feature("globalAlignment") {
    scenario("example") {
      globalAlignment("PLEASANTLY", "MEANLY") shouldBe (8, ("PLEASANTLY", "-MEA--N-LY"))
    }
    scenario("extra dataset") {
      val v = "ILYPRQSMICMSFCFWDMWKKDVPVVLMMFLERRQMQSVFSWLVTVKTDCGKGIYNHRKYLGLPTMTAGDWHWIKKQNDPHEWFQGRLETAWLHSTFLYWKYFECDAVKVCMDTFGLFGHCDWDQQIHTCTHENEPAIAFLDLYCRHSPMCDKLYPVWDMACQTCHFHHSWFCRNQEMWMKGDVDDWQWGYHYHTINSAQCNQWFKEICKDMGWDSVFPPRHNCQRHKKCMPALYAGIWMATDHACTFMVRLIYTENIAEWHQVYCYRSMNMFTCGNVCLRCKSWIFVKNYMMAPVVNDPMIEAFYKRCCILGKAWYDMWGICPVERKSHWEIYAKDLLSFESCCSQKKQNCYTDNWGLEYRLFFQSIQMNTDPHYCQTHVCWISAMFPIYSPFYTSGPKEFYMWLQARIDQNMHGHANHYVTSGNWDSVYTPEKRAGVFPVVVPVWYPPQMCNDYIKLTYECERFHVEGTFGCNRWDLGCRRYIIFQCPYCDTMKICYVDQWRSIKEGQFRMSGYPNHGYWFVHDDHTNEWCNQPVLAKFVRSKIVAICKKSQTVFHYAYTPGYNATWPQTNVCERMYGPHDNLLNNQQNVTFWWKMVPNCGMQILISCHNKMKWPTSHYVFMRLKCMHVLMQMEYLDHFTGPGEGDFCRNMQPYMHQDLHWEGSMRAILEYQAEHHRRAFRAELCAQYDQEIILWSGGWGVQDCGFHANYDGSLQVVSGEPCSMWCTTVMQYYADCWEKCMFA"
      val w = "ILIPRQQMGCFPFPWHFDFCFWSAHHSLVVPLNPQMQTVFQNRGLDRVTVKTDCHDHRWKWIYNLGLPTMTAGDWHFIKKHVVRANNPHQWFQGRLTTAWLHSTFLYKKTEYCLVRHSNCCHCDWDQIIHTCAFIAFLDLYQRHWPMCDKLYCHFHHSWFCRNQEMSMDWNQWFPWDSVPRANCLEEGALIALYAGIWANSMKRDMKTDHACTVRLIYVCELHAWLKYCYTSINMLCGNVCLRCKSWIFVKLFYMYAPVVNTIEANSPHYYKRCCILGQGICPVERKSHCEIYAKDLLSFESCCSQKQNCYTDNWGLEYRLFFQHIQMECTDPHANRGWTSCQTAKYWHFNLDDRPPKEFYMWLQATPTDLCMYQHCLMFKIVKQNFRKQHGHANPAASTSGNWDSVYTPEKMAYKDWYVSHPPVDMRRNGSKMVPVWYPPGIWHWKQSYKLTYECFFTVPGRFHVEGTFGCNRWDHQPGTRRDRQANHQFQCPYSDTMAIWEHAYTYVDQWRSIKEGQMPMSGYPNHGQWNVHDDHTNEQERSPICNQPVLAKFVRSKNVSNHEICKKSQTVFHWACEAQTNVCERMLNNQHVAVKRNVTFWWQMVPNCLWSCHNKMTWPTRPEQHRLFFVKMRLKCMHEYLDVAPSDFCRNMQAYMHSMRAILEYQADFDLKRRLRAIAPMDLCAQYDQEIILWSGGYIYDQSLQVVSCEGCSYYADCYVKCINVKEKCMFA"
      val rscore = 1555
      val rv = "ILYPRQSMICMSFCF-WDM--WKKDVPVVLMMFLERRQMQSVF-S-WL--VTVKTDCGKGIYNHR-K--Y-LGLPTMTAGDWHWIKK---Q-NDPHEWFQGRLETAWLHSTFLYWKYFE-CDAVKVCMDTFGLFGHCDWDQQIHTCTHENEPAIAFLDLYCRHSPMCDKLYPVWDMACQTCHFHHSWFCRNQEMWMKGDVDDWQWGYHYHTINSAQCNQWFKEICKDMGWDSVFPPRHNCQRHKKCMPALYAGIW---MA----TDHACTFMVRLIYTENIAEWHQVYCYRSMNMFTCGNVCLRCKSWIFVKN-YMMAPVVNDPMIEA--FYKRCCILGKAWYDMWGICPVERKSHWEIYAKDLLSFESCCSQKKQNCYTDNWGLEYRLFFQSIQMN-TDPH----Y--CQTHVCW-ISAMF-PIYSPFYT--SG-PKEFYMW---LQARI-DQNM---HGHANHYV-TSGNWDSVYTPEKRA--G--V-FP-V-V-------VPVWYPPQMCN--D-YIKLTYEC--E---RFHVEGTFGCNRWD-L-GCRR--YII--FQCPYCDTMKI---CY--VDQWRSIKEGQFRMSGYPNHGYWFVHDDHTNEW-----CNQPVLAKFVRSKIVA---ICKKSQTVFHYAYTPGYNATWPQTNVCERMYGPHDNLLNNQQNVTFWWKMVPNCGMQILISCHNKMKWPT--S-HYVF---MRLKCMHVLMQMEYLDHFTGPGEGDFCRNMQPYMHQDLHWEGSMRAILEYQAEHH-RRAFRA----ELCAQYDQEIILWSGGWGVQDCGFHANYDGSLQVVSGEPCSMWCTTVMQYYADCWEKCMFA"
      val rw = "ILIPRQQMGCFPFPWHFDFCFWSAHHSLVVP--LNP-QMQTVFQNRGLDRVTVKTDC----HDHRWKWIYNLGLPTMTAGDWHFIKKHVVRANNPHQWFQGRLTTAWLHSTFLY-KKTEYC-LVR---HS-NCC-HCDWDQIIHTCAF-----IAFLDLYQRHWPMCDKLY------C---HFHHSWFCRNQEMSM--D---W---------N--Q---WFP-------WDSV-P-RANCLE-EGALIALYAGIWANSMKRDMKTDHACT--VRLIYVCELHAWLK-YCYTSINML-CGNVCLRCKSWIFVKLFYMYAPVVNTIEANSPHYYKRCCILGQ------GICPVERKSHCEIYAKDLLSFESCCSQK-QNCYTDNWGLEYRLFFQHIQMECTDPHANRGWTSCQTAKYWHFNLDDRPP-KEFYMWLQATPTDLCMYQHCLMFKIVKQNFRKQHGHANPAASTSGNWDSVYTPEKMAYKDWYVSHPPVDMRRNGSKMVPVWYPPGIWHWKQSY-KLTYECFFTVPGRFHVEGTFGCNRWDHQPGTRRDRQANHQFQCPYSDTMAIWEHAYTYVDQWRSIKEGQMPMSGYPNHGQWNVHDDHTNEQERSPICNQPVLAKFVRSKNVSNHEICKKSQTVFHWA-C---EA---QTNVCERMLN-NQHVAV-KRNVTFWWQMVPNC----LWSCHNKMTWPTRPEQHRLFFVKMRLKCMH-----EYLD--VAPS--DFCRNMQAYMH-------SMRAILEYQADFDLKRRLRAIAPMDLCAQYDQEIILWSGGY-I--------YDQSLQVVSCEGCSYYADCYVKCI-NVKEKCMFA"
      globalAlignment(v, w) shouldBe (rscore, (rv, rw))
    }
    scenario("interactive quiz") {
      val v = "MPITMIIPFQFLFYCTYYTAWIMCWCTNMYQQYYVRFGPRNKRIGWEPPECCNWWYSRHDKYVGQQFWWAAKNFPKNFNELGNVPGYDGDVNDFATHMAFAPRKSLLNDQLRTLSNIFAKCKKRSAGGLYMDIKVDWYLLWLCSWWCDRDNVPFLHWRTFVIEGETHCKTENLLIIRVNQQYIFKWYLQVFNHRIWYPLTKAQQSGAPWRAPKYRMMEQLVKQNHEQQIMTLWSDLDTRYLEYMCDSEILCFAYYQADHERGMQHNTMFSPNKDPGWMRQWFWGDRNYIFYTRLIKTNQHEFMHCDNVHGLLFCPRHAINHNYQKRCPDKTKERELHRDHLSTEHQQLNQEQETFCNYWVESDKRRWITKYRSMILLLFGQDGNDPIWGTLAEKSALHLWPYKEDTYGCMSQNLDSNGGDWSYVCGDQDMTIIGGNGIVADHCEAAKIYKAAEHMTTVTAVMDNPTERAAHNIPSRRSIKSADFCNAWCKYIVAIHHYTPPQLYLHGTNYMKDAMTKKVHLLKPFDAWKLWTADKGQPPAHKGFHKHQEIEIELRLRSWIYHGIYVRPHSHPCPRKCRYENRITSMDGNAAIAWHVEAELELMPECQILVLDLWHHLHAKWAVKRFHTIKVRKASEENIFKIGFNVISRICIFQCVLEDEMTDEWGRYYLRHRHTEYIVRQVYIDWCWASPMPKFIDEAYAVNMALTANWYNPPSACVEFCIYNEEDYPHGWSVEEDRRTIAVHRSVFFSRVVWIRKVPYYLQHWWPRNGVYCKYYSKLIK"
      val w = "RPITMIIPFQFDYGAARDFYCTYYTAWMQYYVRFGGNPENHRPPECRNWWYSRADKYTSQQFWWAAPNFPKNFNELGNVPLLTIPQQGYDGDVNDFATHMAFWDGRVIVHQPRKSLLVDQLRTLSLIFAKYAKFKFYPMYQSRGGKKVGFYMDICSWWIRFLHNRTFVIEGETHIKTENLRIIAVNQNANHRIWYITDVMLTKAQQFLPKYRMMHCDVKQNGEQQNLWSDLDTRYLEEMCDCVTTHEILCFAEPIYRSHQHQTMFSPNKDPGWMGQWFWGDRLYKTNQHHCDNVFDVQLGLLFCHNYQKICPDKTKESELFNDHHMSLWRVPRQEQETFCNYNMKNRWVHSNRRRWITKYRQVTDDMRECHFPQNGNEPIWLATKSALHLWPYKLRIWEVVKDINGGDWSQIWGGGNPFKFGQPAIVADGRWECEAAKIYYAVVHMTTVTAVTKITRSIKFENANLLHRPWCKYIVAIHHYTPKVMKYMKDAMTKDMMWVIFVHKLGRPFDAWKWVHKHQEIERETPYSRLRTIGVRMWINKWSKLSHGIYVRPHSHPCPRVCNRITAEKMDMNAAWHVEAEGVEYNPEELMPECQILVLDLWHHLHAKWAVKVFHTIKVRKASEENIEYKKFIGFLKFIYQQFQEWYIGVVICSPMPKFMCDCWMVCMGSVLYLVQWAMIPWYAPHIISVFFPTQIRISCVEFLYMVMGRCHGWSVIEDRRTIAVHRGFSRLHFLVTMVWIRKVPYYWWPRHQVYCKYYHKLIK"
      val rscore = 1697
      val rv = "MPITMIIPFQF-L-----FYCTYYTAWIMCWCTNMYQQYYVRFGPRNKRIGWEPPECCNWWYSRHDKYVGQQFWWAAKNFPKNFNELGNVP-------GYDGDVNDFATHMAF--A------PRKSLLNDQLRTLSNIFAKCKKRSAGGLYMDI--K-VDWYLLWLCSWWCDRDNVPFLHWRTFVIEGETHCKTENLLIIRVNQQYIFKWYLQVFNHRIWYPLTKAQQSGAPWRAPKYRMMEQLVKQNHEQQIMTLWSDLDTRYLEYMCD--S--EILCFAYYQADHERGMQHNTMFSPNKDPGWMRQWFWGDRNYIFYTRLIKTNQHEFMHCDNV---H-GLLFCPRHAINHNYQKRCPDKTKERELHRDH-LSTEHQQLNQEQETFCNY-----WVESDKRRWITKYRSMI--LL-L-FGQDGNDPIWGTLAEKSALHLWPYKEDTYGCMSQNLDSNGGDWSYVCGDQDMTIIGGNGIVAD-H--CEAAKIYKAAEHMTTVTAVMDNPTERAAHNIPSRRSIKSADFCNA-WCKYIVAIHHYTPPQL-YLHGTNYMKDAM-TKKVHLL-KPFDAWKLWTADKGQPPAHKG-FHKHQEIEIELRLRSW--IYHGIYVRPHSHPCPRKCRYENRITS--MDGNAAIAWHVEAE-LE-----LMPECQILVLDLWHHLHAKWAVKRFHTIKVRKASEENI-FK--IGF-NVISRICIFQ-CVLEDEMTDEWGRYYLRHRHTEYIVRQVYIDWCWASPMPKFIDEAYAVNMALTANWYNPPSACVEFCIYNEEDYPHGWSVEEDRRTIAVHR--S-V-FFSRVVWIRKVPYYLQHWWPRNGVYCKYYSKLIK"
      val rw = "RPITMIIPFQFDYGAARDFYCTYYTAW-M-------Q-YYVRFGG-NPE-NHRPPECRNWWYSRADKYTSQQFWWAAPNFPKNFNELGNVPLLTIPQQGYDGDVNDFATHMAFWDGRVIVHQPRKSLLVDQLRTLSLIFAKYAKFKFYPMYQSRGGKKVGFYMD-ICSWWI-R----FLHNRTFVIEGETHIKTENLRIIAVNQN--------A-NHRIWY-ITDVMLTKAQQFLPKYRMMHCDVKQNGEQQ--NLWSDLDTRYLEEMCDCVTTHEILCFA--EPIY-RSHQHQTMFSPNKDPGWMGQWFWGDR--L-Y----KTNQH---HCDNVFDVQLGLLFC--H--N--YQKICPDKTKESELFNDHHMSLWRVP-RQEQETFCNYNMKNRWVHSNRRRWITKYRQVTDDMRECHFPQNGNEPIW--LATKSALHLWPYKLRIWEVV-K--DINGGDWSQIWGGGNPFKFGQPAIVADGRWECEAAKIYYAVVHMTTVTAVT-KIT-RS---I--K--FENANLLHRPWCKYIVAIHHYTPKVMKYMKDA-MTKDMMWVIFVHKLGRPFDAWK-WV-HKHQEIERETPYSRLRTIGVRMWINKWSKLSHGIYVRPHSHPCPRVC---NRITAEKMDMNAA--WHVEAEGVEYNPEELMPECQILVLDLWHHLHAKWAVKVFHTIKVRKASEENIEYKKFIGFLKFIYQQ--FQEWYIGVVICSPMPKF-MCDCWMVCMGSVLYLVQ-WAM-IPWYAPHIISVFFP-T-Q-IRI-S-CVEF-LYMVMGRCHGWSVIEDRRTIAVHRGFSRLHFLVTMVWIRKVPYY---WWPRHQVYCKYYHKLIK"
      globalAlignment(v, w) shouldBe (rscore, (rv, rw))
    }
  }

  feature("localAlignment") {
    scenario("example") {
      localAlignment("MEANLY", "PENALTY") shouldBe (15, ("EANL-Y", "ENALTY"))
    }
    scenario("extra dataset") {
      val v = "AMTAFRYRQGNPRYVKHFAYEIRLSHIWLLTQMPWEFVMGIKMPEDVFQHWRVYSVCTAEPMRSDETYEQKPKPMAKWSGMTIMYQAGIIRQPPRGDRGVSDRNYSQCGKQNQAQLDNNPTWTKYEIEWRVQILPPGAGVFEGDNGQNQCLCPNWAWEQPCQWGALHSNEQYPNRIHLWAPMSKLHIKIEKSSYNRNAQFPNRCMYECEFPSYREQVDSCHYENVQIAFTIFSGAEQKRKFCSCHFWSNFIDQAVFSTGLIPWCYRRDDHSAFFMPNWNKQYKHPQLQFRVAGEGTQCRPFYTREMFTKVSAWRIAGRFAGPYERHHDAHLELWYQHHKVRTGQQLGIIWNNRDKTRNPCPFSAYYNKLPWWKINQNAFYNCLQNIAHSTHDETHEFNPVKCIDWLQGTMVPTECKKGFVHEKCECYRNPGPPLHDMYHQMEDIFGVRFDCLTGWKHLSDYNPCQERRNINDFYIFAYEIAPAVKNLVLSPQPLADATKKCAFNYTPLDQSPVVIACKWYIHQPICMLLIVLICAMDKYNAHMIVIRTTEGQQPMHACRMTEGPGMCMKEPLVTFTLPAQWQWPNHEFKYVYMYVLNYHLSQYTYTDEGHAGGQHYSFNVAVDVGMAWGHNRCYCQPACYSQQETQTRTIDYEKWQYMKHQAFKWGLWFCEQERHAWFKGQNRCEMFTAKMTRMGADSNLDQYKLMLAQNYEEQWEQPIMECGMSEIIEIDPPYRSELIFTFWPFCTYSPWQNLIKCRCNNVIEEMDQCVPLTFIGFGVKQAGGIQAWAFYKEEWTSTYYLMCQCMKSDKAQYPYEIILFWMQPMDTGEQEPPQQNMWIFLPHSWFFDWCCNAPWSEICSSRHDHGQCQDAFYPCELFTVFDDIFTAEPVVCSCFYDDPM"
      val w = "WQEKAVDGTVPSRHQYREKEDRQGNEIGKEFRRGPQVCEYSCNSHSCGWMPIFCIVCMSYVAFYCGLEYPMSRKTAKSQFIEWCDWFCFNHWTNWAPLSIVRTSVAFAVWGHCWYPCGGVCKTNRCKDDFCGRWRKALFAEGPRDWKCCKNDLQNWNPQYSQGTRNTKRMVATTNQTMIEWKQSHIFETWLFCHVIIEYNWSAFWMWMNRNEAFNSIIKSGYPKLLLTQYPLSQGSTPIVKPLIRRDQGKFWAWAQMWWFREPTNIPTADYCHSWWQSRADLQNDRDMGPEADASFYVEFWYWVRCAARTYGQQLGIIWNNRLKTRNPCPYSADGIQNKENYVFWWKNMCTKSHIAFYYCLQNVAHYTHDVTAEFNPVKCIDWLQGHMVLSSWFKYNTECKKLFVHEKCECYRMFCGVVEDIFGVRFHTGWKHLSTAKPVPHVCVYNPSVQERRNINDFYIFYEIAPAVKNLVLSAQPLHDYTKKCAFNYTPITITRIISTRNQIIWAHVVIACQFYSPHQMLLIELAMDKYCADMNVRRSTEGHQPMHACRSTFGPGMAAKEPLVTFTLVAFWQWPNHEFQYVYMYTEDKIIQIGPHLSNGCEMVEYCVDCYAKRPCYRAYSAEAQYWRMITEAEDYSYKTRNAIAATATVRGQYCHPFRWLGIVWMAHHDCFFANECGTICIPQMAEMRPPETTPYEIDIIFMMFWKEHMSTTILDVVGMYRPATFSHWHDAHHQCEPYLTPLMCQSKLVFDAAFTQVGVKGVWYHTEKLELMAGFNHMKFKKEEAQQSCFYWFQDCPDYDPPDAVRKTDEKHIRAHGEIWWLMRYYCMYHILHIASRHEWMHLRWDQACTNPGYELFEFIPWVLRRYVVYDKIRYNYSYRNSASMEFV"
      val rscore = 1062
      val rv = "YQAGIIRQPPRGD-RGVSDRNYSQCGKQ-NQ-AQLDNNPTWTKYEIEWRVQI-LPPGAGVFEGDNGQNQCLCPNW--A-W-EQPCQW----GALHS-NEQYPNRIHLWAPMSKLHIKIEKSSYN-RNAQ-FPNRCMYECE-FPSY-REQVDSCHYENVQIAF-TIFSGAEQKRKFCSCHFWSNFIDQAVFSTGLI-PWCYRRDDHSAFFMPNWNKQ--YKHPQLQFRVAGEGTQCRPFYTREMFTKVSAWRIAGRFAGPYERHHDAHLELWY-QHHKVRT-GQQLGIIWNNRDKTRNPCPFSAY-Y-NK--LP-WWK-I-NQ-N-AFYNCLQNIAHSTHDETHEFNPVKCIDWLQGTMV-P------TECKKGFVHEKCECYRNPGPPLHDMYHQMEDIFGVRFDCLTGWKHLS------D---YNPC-QERRNINDFYIFAYEIAPAVKNLVLSPQPLADATKKCAFNYTPLDQSPVVIACK---WYIHQPI-CMLL----IVLIC-AMDKYNAHMIVIRTTEGQQPMHACRMTEGPGMCMKEPLVTFTLPAQWQWPNHEFKYVYMYVLNYHLSQYTYTDEGHAGGQHYSFNVAVDVGMAWGHNRCYCQPACYSQQETQTRTIDYEKWQYMKHQAFKWGLWFCEQER-HA--WFKGQNRCEMFTAKMTRMGADSNLDQYKLMLAQNYEEQWEQPIMECGMSEIIEIDPPYRSELIFTFWPFCTYSPWQNLIKCRCNNVIEEMDQCVP-LTF-IGFGVKQAGGIQA-WAFYKE--EWTSTYYLMCQCMKSDKAQYPYEIILFWMQ--P-MDTGE--QEPPQQNMWIFLPHSWFFDWCCNAPWSEICSSRHD--H---GQ-CQDAFYPCELFTVF"
      val rw = "Y-P-MSRKTAKSQFIEWCDW-F--CFNHWTNWAPLSIVRTSVAFAV-W-GHCWYPCG-GVCKTNRCKDD-FCGRWRKALFAEGPRDWKCCKNDLQNWNPQYSQGTR--NTK-RMVATTNQTMIEWKQSHIFETW-LF-CHVIIEYNWSAF-W-MWMNRNEAFNSIIKSGYPKLLL-T-QY-P-L-SQG--STPIVKPL-IRRD-QGKFW-A-WAQMWWFREPT-NIPTA-D-Y-CHSW--WQ--SR-ADLQ-NDRDMGP-EADASFYVEFWYWVRCAARTYGQQLGIIWNNRLKTRNPCPYSADGIQNKENYVFWWKNMCTKSHIAFYYCLQNVAHYTHDVTAEFNPVKCIDWLQGHMVLSSWFKYNTECKKLFVHEKCECYRM----FCGV---VEDIFGVRFH--TGWKHLSTAKPVPHVCVYNPSVQERRNINDFYIF-YEIAPAVKNLVLSAQPLHDYTKKCAFNYTPITITRIISTRNQIIW-AHVVIACQFYSPHQMLLIELAMDKYCADMNVRRSTEGHQPMHACRSTFGPGMAAKEPLVTFTLVAFWQWPNHEFQYVYMYTED-KIIQIG-PHLSN-GCEMVEYCVDC-YAK-RPCYRAYSAEAQYWRMITEAEDYSYKTRNAIAATATVRGQ-YCHPFRWLGIVWM-AHHDC-FFANECGTICI-PQMAEMRPPETTPYEI--DIIFMMF-WKE--HMSTTIL-DVVGMYRP-ATFSHWHDAHH-QCEPYLTPL-MCQSKLVFDAAFT--QVG-VKGVW-YHTEKLELMAGFNHM-K-FKKEEAQ---QSCFYWFQDCPDYDPPDAVRKTDEKHIRAHGEIWWLMRYYCMYHILHI-ASRHEWMHLRWDQACTNPGY--ELFE-F"
      localAlignment(v, w) shouldBe (rscore, (rv, rw))
    }
    scenario("interactive quiz") {
      val v = "MGSWHSRRRIQAAIWPWSIMFWTDGCNTDGEYQEADQRNTMKVLCVKGNSFAKWKHASRCHGKIQVLPHYAFPPPPKWMAEDNRECVKIWNSRAQNINTGDGSQPKSPMKRVSAEKFNCIGNGENNRARGCWDGFDETQMWAGWLLIWMSESKHNCADCCAAGKTGHAEVCWFLDMVHEWCQWTDLYVWEPVVMCNKRTEHMESNHWIRQESDLYFKLTQNKPHKDGCDFLGCSFIMEIWHQIQKIMANAGQAQSLRNMQSCFEYHYPGMHVKMHYECLVQQFIPHHAAWPQRHSYYGASDQAKKRNNDYPPEPYPGQNQFQEKCRNFYCLRIINCVSDVWYCLMVHCDELGQHSLIFQARFLVDGWPGSQPHNVQYDLVVCMKSNAMFHCWCNQYFCSVFGNSNGIYSFHFMRQTNYMMQQTCQQYQNQGVLKHMDAMRHQCRKSFKFTCWMELAQACEEYKGYPDDEISIDDYIEPRDQQRHDRWMCRLINRCTQFTIGFNWMPAYYPTLPPEFHHNGAGHDSKPSHHEVIWNWEIMCAVTPSGIKAQMTRFPWLQLEVQNSKCYCPHDLKIKINGRNSYCMQLHWTISYAMGHQWVRINLIYKMELCCIMGNMEDDLFKDTMSKGADYDKIGMRSVRQLWWGKRWNMPYKGMWASRCTVWNCAWCKEEDCLRLKLSRDVACKHSWCSMSRMVRMIWPRYYGWARAQTSSGFLWYQFGWIPDHIGETTDGLSIRTMRDNTIQVLEEKVCPWCWFTNKQLGRTDAANEHNWKNVQSDWDWIMKNCCRLGDVHSPCCKANSSQFEAFGKDIRHMLQGKIWNYCYNWWNYYFLVIMDEKNVHLHCAYIKQVIMLQHLAFGYQSPVKKKTGDMPKRKYKHAFGGWNHEWGMADIDCEANKRR"
      val w = "RGGKKVTLLQGSIATAYKHTIPAFNWWVLFLWMNRGKSTEINDNGGDISFIYHKQTHMKGAVYIWAHWLDYGPEPLAFEAGEEKMRMFSQDWWLNQAQVLQMIWIIMNMSEITHDMYDRMLAESMLKIRNWAVYPPQEDCSMDRNNRMIWPHSQWFWMKWNLITGRFLYQMQNIWCNWNNTNVRNCHSIAPAAFGCDSSEVSWPKDFSCLVKYHRNFLVIAIAYVNVTLFGLFCNWFRKYWHFYHMDIKMTWEYDKRAVGNFWWYQVMFRTIEEYFDEMERWEWDSNRLPMTNQDKSIGQSEFSIYDNDSLRNMQSRWKLTGFPYHYPGMHIKMHYECLVAQFIPHHAAWPQRHSHYSDAFAKRQNDQFQKRIINCVSDVWYEPLMVHCDELIQHSLIFNWWKFCNHAARMLVDEWPGHYQRMLLQPHVQMQCNVQACLRNKQQVNCSDMPEQYFCCFIYSFHFMRCQQYQNQGVCKHMDAMRHQCRFTCWMELCHFQWNNYKCVDYIIPRDQQRHDRWMCRYKNRCTQFTMNMACYGFNKRESMPAYYEFDNFTLPPEFLVNGMAMHYMIRMGHDSKPSHHEIIWNWEIMCAVTPSGIKAQMTRKEVREKPWLTLEVQVSKCYCPHDLPKAYNNIKINGRNSYCMQLHWTISYAMELCCIMGNMVLHGDVIYVWLFTTRWQEDQYYYSEHCHNCLEETLKKSYGFDERMHRLDWPCDWSEKSKQEGWDHPCCVWDPCDFMVDDMQQRAKIDGLSEAEIPEDLPILSNNGPEKWLVRMWPFYAHMSSPVPERLQYQWTSKFAGVPCWQGLKADCGVDTHPTAENKVKSREGIETVYRDWVEPVCVFLFLGFPGSCAHNGNIPFNVLLAAQCHAARVSVHGKKWDSGCGFLPAFNINKRIWWFDTAHCELQETQEPKEPDPNQCYEWVTTELHFEILHFRIGWCWSSTMRKRMDFIHWVVDSRRSFS"
      val rscore = 1167
      val rv = "RRRIQAAIWPWSIMF-WTD-GCNTDGEYQEADQRNTM--KVLCVKGNSF--AKW-KHAS-R-C-HGKIQVLPHYAFPPPPKWMAEDNRECVK-IWNSRAQNINTGDGSQPK-S-PMKRVSA-EKFNCIGNGENNRA-RGCWDGFDE-TQM-W---AGW-L--L--IW-M-SESK-HNCADCCAAGKTG-HA-EVCW---FLDMVH-EWCQW-T-DL-YVWEPVV-M-CNK-RT--E--HMESN-HWIRQESDL-YFKLTQNKPHKDGCDFLGCSFIMEIWH-QIQKI-MAN---A-GQAQ-S------LRNMQS----C-FEYHYPGMHVKMHYECLVQQFIPHHAAWPQRHSYYGASDQAKKRNNDYPPEPYPGQNQFQEKCRNFYCLRIINCVSDVWY-CLMVHCDELGQHSLIF-------QA-RFLVDGWPG---S---QPH-NVQYDLVVCMKSNAMFHCW--CNQYFCSVFGNSNGIYSFHFMRQTNYMMQQTCQQYQNQGVLKHMDAMRHQCRKSFKFTCWMELAQACEEYKGYPDDEISIDDYIEPRDQQRHDRWMCRLINRCTQFTI-----GFN-W--MPAYY---P-TLPPEFHHNG-A-------GHDSKPSHHEVIWNWEIMCAVTPSGIKAQMTR--F---PWLQLEVQNSKCYCPHDL-K----IKINGRNSYCMQLHWTISYAMGHQWVRINLIYKMELCCI-M-GNM--EDDLF-KDT-MS-KGADYDKI-GMRS-VRQLWWGKRWNMPYKGM-WASRCTVWN-CAW-CKE-EDCLRLK-LSR-DVACKHS-WCSMS--R-MVRMIWPRYYGWARAQTSS--GFLWY-QF-GWIPDHIGETTD-GLSIRTMRDNTIQVLE--EKVCP-W----CWFTNKQLGRTDAANEHNWKNVQSDWDWIMKNC-C-RLGDVHSPCCK-ANSSQF-EAFGKDIRHMLQGKIWNYCYNWWNYYFLVIMDE-KNVHL-HCAYIKQVIMLQHLAFGYQSPVKK-KTGDMPKRKYKHAFGGW"
      val rw = "KHTIPAFNW-W-VLFLWMNRGKSTEIN-DNGGDISFIYHKQTHMKGAVYIWAHWLDYGPEPLAFEAGEEKM-R-MFSQD-WWL-NQA-QVLQMIWIIMNMSEITHDMYDRMLAESMLKIRNWAVYPPQEDCSMDRNNRMIW-PHSQWFWMKWNLITGRFLYQMQNIWCNWNNTNVRNC-HSIAPAAFGCDSSEVSWPKDFSCLVKYHR-NFLVIAIAYVNVTLFGLFCNWFRKYWHFYHMDIKMTWEYDKRAVGNFWWYQVMFRTIE-EYFD-E--MERWEWDSNRLPMTNQDKSIGQSEFSIYDNDSLRNMQSRWKLTGFPYHYPGMHIKMHYECLVAQFIPHHAAWPQRHSHY--SDAFAKRQND--------Q--FQ-K-R------IINCVSDVWYEPLMVHCDELIQHSLIFNWWKFCNHAARMLVDEWPGHYQRMLLQPHVQMQCNVQACLRNKQQVNCSDMPEQYFCC-F-----IYSFHFMR---------CQQYQNQGVCKHMDAMRHQCR--F--TCWMEL---CH-FQ-WNNYK-CVD-YIIPRDQQRHDRWMCRYKNRCTQFTMNMACYGFNKRESMPAYYEFDNFTLPPEFLVNGMAMHYMIRMGHDSKPSHHEIIWNWEIMCAVTPSGIKAQMTRKEVREKPWLTLEVQVSKCYCPHDLPKAYNNIKINGRNSYCMQLHWTISYAMELCCIMGNMVLHGDVIYVWLFTTRWQEDQYYYSEHCHNCLEETLKKSYGFDERMHRLDWPCDWSEKSKQEGWDHPCCVWDPCDFMVDDMQQRAKIDGLSEAEIPEDLPILSNNGPEKWLVRM-WP-FYAHMSSPVPERLQYQWTSKFAG-VPCWQGLKADCGVDTHPTAENKVKSREGIETVYRDWVEPVCVFLF--LGFPGSCA-HN-GNIPFN-VLLAAQCHAARVS-VHGK--KWDSGCGFLPAF--NI-N--K-RIW-W-FDT-AHCELQETQEPKEPDPNQC-YEWVTTEL-HFEI-LHFRIGWCWSSTMRKRM--D-FIHW"
      localAlignment(v, w) shouldBe (rscore, (rv, rw))
    }
  }

  feature("editDistance") {
    scenario("example") {
      editDistance("PLEASANTLY", "MEANLY") shouldBe 5
    }
    scenario("extra dataset") {
      val v = "GGACRNQMSEVNMWGCWWASVWVSWCEYIMPSGWRRMKDRHMWHWSVHQQSSPCAKSICFHETKNQWNQDACGPKVTQHECMRRRLVIAVKEEKSRETKMLDLRHRMSGRMNEHNVTLRKSPCVKRIMERTTYHRFMCLFEVVPAKRQAYNSCDTYTMMACVAFAFVNEADWWKCNCAFATVPYYFDDSCRMVCGARQCYRLWQWEVNTENYVSIEHAEENPFSKLKQQWCYIPMYANFAWSANHMFWAYIANELQLDWQHPNAHPIKWLQNFLMRPYHPNCGLQHKERITPLHKSFYGMFTQHHLFCKELDWRIMAHANRYYCIQHGWHTNNPMDPIDTRHCCMIQGIPKRDHHCAWSTCDVAPLQGNWMLMHHCHHWNRVESMIQNQHEVAAGIKYWRLNRNGKLPVHTADNYGVLFQRWWFLGWYNFMMWHYSLHFFAVNFYFPELNAGQMPRFQDDQNRDDVYDTCIWYFAWSNTEFMEVFGNMMMYSRPMTKMGFHGMMLPYIAINGLRSISHVNKGIGPISGENCNLSTGLHHYGQLRMVMCGYCTPYRTEVKNQREMISAVHCHQHIDWRWIWCSGHWFGSNKCDLRIEDLQNYEPAKNKSNWPYMKECRKTEPYQDNIETMFFHQHDLARDSGYIANGWHENCRQHQDFSNTFAGGHKGTPKGEHMRRSLYVWDTDCVEKCQWVPELFALCWWTPLPDGVPVMLGTYRQYMFGLVVLYWFEVKYSCHNSWDYYNFHEGTMKDSDPENWCFWGMQIIQFHDHGKPEFFQDPMKQIIKTECTAYNSFMMGHIGKTTIVYLVSYIGRLWMKSCCLTWPPYATAPIKWAEETLLDFGQGPHPKYACHFTHQNMIRLAKLPMYWLWKLMFHE"
      val w = "GMWGFVQVSTQSRFRHMWHWSVHQQSSECAKSICHHEWKNQWNQDACGPKVTQHECMANMPMHKCNNWFWRLVIAVKEEKVRETKMLDLIHRHWLVLNQGRMNEHNVTLRKSPCVKRIMHKWKSRTTFHRFMCLMASEVVPAKRGAQCWRQLGTYATYTVYTMMACVAFAFEYQQDNDNEADWWKCNCAFVPVYFDDSCRPVVGAFQCYRLGLPFGTGWNYAEENPFSKLKQQMHRKTMGECKNMMIWAYCANELQLPIKWGSMYHEHDFQLPPYHPNRFHKIRITILHKSFYGMFTQHHLFCKELDWRIMAWANRYYCIQHGWHTNNPDDPITRHKCMIQGGQNSRNADIRHMPVQCGNWGHAIGLEMPMPMHHCHHANRVESMIQTQHYWGPKLNRNADWWFLGWQNFEIFRMPILRWMGAYEWHYSLHFFAVNFYFPELNAGQMPRFQDDQNNNACYDVWAWSNTEFMEVNGIKKLRFGNMMMYSRPMTKMGFHGMMKSRSISHVNKGIGPISGENCSTGLHHYGQLTEVKNQREMISAVHCHQHIWCKCDLRIEPAKNKGYWPYQKEFCWRKQINSRKTEPYQVAPVINIETMFFDFWYIANGMHENCRRTGHKPNPDCVEKCQWVPELFALCWWRAMPDGVPVMLGTMFGLVVYWFEVKYSCHNSLYRRVTDYYNFHEGTMKDHEVPWNWDNEHCHDHGKAEFFFQMLKIPICDPMKAIIPSTEMVNTPWHPFSFMMGHDGKTTIVYSGSYIGRLWVPSRWKPYAPANWKMPIKWAEETLLMVPHPHFTHQQLWGTTLRLAKLPMYWLWKLMFHHLFGVK"
      editDistance(v, w) shouldBe 400
    }
    scenario("interactive quiz") {
      val v = "PKERRYENHMRHHVEIEHKYAVPPKLEGYQLSVCFNFDPLLSMFLCHVCYCTETVYNDIGRYMTLDFHPNVNQLINTVTLDLYTSIGVPAKAIRYFDMFWYTCSWLMLGPLHIIQHTECPLMGTLFFMQAWLQWINDCVWFDGRPDCKAGWFCCAFQFGPSWCGYISRNPWGRHKWEKHYFMRYKVSYIFMDNCLPWMGEQVNIHNETAAQWYWMIREGDFNSIGIARKLQGAPYYQYADLPFTGDYRRHPCEHAVEKGPRGVRMMTWFHQAKIIPKWFVVNKNFWPSHYTTYWVGQSEEPWMRPDWGCHPLKKGSNIHCNNCHTAFEKMQFGNIWRCSSPYWPLKKLTTTADQHYMLNYDGTPNWCRQRLRYLLAGRFCPKFQNKPSWCWHDPKTISTMHMALGIYCYCESQIIYCVVGTAAEFWQMWCRARVHGPNHSMMQNTSGYNAHSANFSKYEDTHQKLHTPTKPLMVFPDQYMPNFMIIADAQKIGEVCGRKYVRHVMHQERLWHQPWDGILNSDTYMRGRIDHAYETYRNNRQQINPISCPHSCDWNHDHKDHDLVSRIAYWGEHWLVIIQIYVSYLMAMAKKWILRNSLMGREKTHTMITPKGWCMYHDCELGVWTSNATVMILMICESPENIVDPELGRCKLYSWALYAKFGWVDPVCMQPDHAQTDAYQIRDTSNRGGYYWGYFGHNPSHYVAFGSLTEPTNQYQEDPYHDFYDCHLWDSWYMWLSVNKQQFWKSDWWDNGHKIKILYEQGWHQALAYAFCDH"
      val w = "PKINPRFPPSPQENHLWQHNKMRHHVEIEHKYAVPPKLEGYQLSVCFNFDELLSMFLCHVCFCTETVNNDIGRYNQLINIGVPAQRLIHHMYNATFDMFWFTCSEYHSGSVKLMYNPLHIIHTECPLMETLFGLQQINDCDLWFDGRPDCKAGWFAMWTVQMASQRKECDRRGPSWCGYISRKWEKHYFMRYKVSYIQIIHEMSDNCLPWMPEQVNIHNETAAQWYWMIREGDSNIEIGDHRCYADHYDAFKYDTPAVEKGPSALGVRWTYHFAAETWFHQAKIIPKWFVVNKNFWWQTGQSEEPWMRPDWGQHPLKKGSNQNFHCNNCHTAFEWMQFGNRCSSPYWYMEPLKKLTMLHNDTADQHYPLNYDGTCPNARELPNWCRTRLRYALAGRFCSKFHNTQTQHKSTMHMALGIYCYCSYEFWQMWRCACHYMLQPFRKHPADHSMMIGRKGYNWLKSANFSKYEDTWIYVRYFPVENPNFFFGVRADAQKSGEVCGSGYNWAFCERPLWHWPWDGIYNSRTRFDVCLTHKMRGRIDHCTMIFRQNSNRQQINPITCPHSCDLVWRIAYWEHWLVIIQTTMFLMKDEKKWIQRNSLMGRKTHTNAQWCSAPPITPKGWCMYHNSCEMGTKNACVWFMITRMIVMICESPENIRCCIQDVIADPNCSTGLGRCKLYSYNMQLNALYAKFGWVDPVCMQPSHATRDAFQIRDTSNRPNIWGYYWGYFGHWLCQWPSHYVAFAFPTNQYQEDPGLEVSDCHLWDSWYMEPMLLSVNGQQFWKLFAFRTLMNGHKIKILYCQGWHQALAWAMCDH"
      editDistance(v, w) shouldBe 358
    }
  }

  feature("fittingAlignment") {
    scenario("example") {
      val v = DNAString("GTAGGCTTAAGGTTA")
      val w = DNAString("TAGATA")
      val rscore = 2
      val rv = "TAGGCTTA"
      val rw = "TAGA-T-A"
      fittingAlignment(v, w) shouldBe (rscore, (rv, rw))
    }
    scenario("extra dataset") {
      val v = DNAString("CAATCACCCCAATCCCTCAATCCTGGCCCCACGCATAGGCTAATGCCAATCGCGGCCAGGGTATAACCGCCATAACTGTGGGTCAGAAGGGATAAGTTCCACAATCCTATTTTCCTCGAGGCGCTTCGATGCGTTAACGCGTACACTCTGTCGGCCAACCGTGTGGGAGCCGAATTGGCTGGGCTGTTGAACATTCTATCAGTAGATAAACGAAGGTACATCCGAGGTTGTCGATCGACCGCGGGGTCGTAGCGCGTGCATGTTCCTTTCAGGCCCACATACTCCGGAACGGTTCATATCACGACTATTCTTGCACAATCGGACAACGGTGTACCATGGTGGACACCGTAGGAGACCAATACTGCGTAAATCATAAGCATTGGAGAGTGGACTGCTAGCGAGGCTCACCATGGAGTCTCGGTCGGCATCTCCTGACTGCTGTTCCATCGCGTTTTTCTTTTACTCACGCAATAAATCAATACCCCCTAACACAGGCCTGCTCCAGCCTTATTAAGGCCATAGTAGCTCTACATGTAGACCGAACGGAAGCACAGTTTGGTAGAAATTCTTAATCGACTATGGTCCGTGCAGGCCAAAAAAGGAATAATCTTCGAATTCTCACGCCTTCATTAGGGCGCACATGGTGGGGTAAATCACTGCACTCTGTTCGCAGTTAAGCGTTGCAATCAATATCGGCAGAACTCGGAGTCCGTATAAAGCCGCCTCAGCGTGCACACGCCCGTGCGGCACGTCATTAGACGAGGATTCCGGGGGACTGGCCTGTTCGTAATCCACTAAAACAATGGTCCTACCATCTAAAACGCACCGTGTTCCCCTCTACGGGAACCCCCTAGAT")
      val w = DNAString("AGAGCGCAGAGAAGTCATTAGAACATGTAGCACATCGCTTATTAAGGGTCAATACCTAAAGGGCCTAACTATACGCCACACGGAACAGCTC")
      val rscore = 22
      val rv = "AGGGCGCACATG--GTGGGGTA-AATCAC-T-GCAC-TCTG-TTCGCAGTTAAGCGTTGCAATCAATATCGGC-AGAACTCGGAGTCCGTA--TAAAGCCGCCTCAGCGTGCACACGC-C"
      val rw = "AGAGCGCAGA-GAAGTCAT-TAGAA-CATGTAGCACATC-GCTT---A-TTAAG-G--G---TCAATA-C--CTA-AA---GG-G-CC-TAACTATA-C-GCCACA-CG-GAACA-GCTC"
      fittingAlignment(v, w) shouldBe (rscore, (rv, rw))
    }
    scenario("interactive quiz") {
      val v = DNAString("AACACATCTGAGCGCACGAGGAAGATCCGTTCATCCCACCATAGAACTTAGCGATATTAATGGAACTATAGGATGCTGGTACTCCAAATGGAGCGTCCTCATGGGGTTACCTCGTGCACTGGAAGCAGACTTCCACTGGTAGCACCGATTAGCGACCAAAAAAATGTGAAATGGCCAGACCCCCGCCTACGACGAAGTTCATGTCCATCGCATAGTATCACTGAAGCTTAGGTTGGAGATCGACTTTGCGTCAATTCCTGTTCACAAGGGATGATTTACGTGAAAAAAGATGACAAACAAAAACTAGAGTAACCGTTAGAGCTCCACTCCTCGGTGACCCGCTTCCACCCTCTGAATAGAAAGTGATGTACGTCGGTCGATTAACTGCAAAATTGTTCACTGTCACTCAGCAGTGCGAGTGCCAGCGATTTTGCCAATATATTAATTCTGCGTCGACGCTCATGTCGATGTACCGTCGGGCTGTTGAGACTAGAGTCAATATGGTAGATTTAGTTATCTTGGATTACCGCGGCCGTAAAAGCGGATTCTCCCCAAAACAAATCCAAAATAGTTAATTTGTAATCAGGAGATACGTCAGTGCGTGAAGATGGCCATGGAGACTGTAAAAACGAGAATGTTGACACTAAACGAGTCTGCCATGCTCATGACATTAAAAGAGTAGTCCATGCAAGGGTACGATCAATCATGGAGAAGGTTCCCGTAAACGTACCACTCGGGACTGTGGGCGCTTGCATCGCTAACGGGTCGCAAAGAAGGTCGGACCAAGCAAGGCAGGCGCTGGATACCAGTAGTATGGGTACTGCTGTTC")
      val w = DNAString("AAAGGGTACCTCACTGACTTCGGCCGAAACGATGTGATTCGCGATATTACACTTTCATTCTTCGCAGGAGCTGTGAGCCCACATACAGA")
      val rscore = 19
      val rv = "AGTGCGAGTGCC--AGC-GATTTTG-CC-AATAT-AT-TAATTCTGCG-TCG-ACGCT--CATGTCGATGTACCGTC-GG-GCTGTTGAGACTAGAGT-CA-A"
      val rw = "AAAG-G-GTACCTCA-CTGACTTCGGCCGAA-ACGATGTGATTC-GCGATATTACACTTTCAT-TC--T-T-C-G-CAGGAGCTGT-GAGCCCACA-TACAGA"
      fittingAlignment(v, w) shouldBe (rscore, (rv, rw))
    }
  }
}
