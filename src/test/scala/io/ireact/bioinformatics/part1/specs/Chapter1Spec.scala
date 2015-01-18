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

package io.ireact.bioinformatics.part1.specs

import io.ireact.bioinformatics.part1.Chapter1._
import io.ireact.bioinformatics.specs.SlowTest
import org.scalatest.{ FeatureSpec, Matchers }

import scala.io.Source

class Chapter1Spec extends FeatureSpec with Matchers {
  feature("patternCount") {
    scenario("example") {
      val text = "TCTCCTTGTCTCCTTGTCGGCGACTCCTTGCTCCTTGACTACTCCTTGACTACTCCTTGACCTCCTTGCCTCCTTGTGCTCCTTGGCTCCTTGTATCTCCTTGGTATACCATCTGAGTCTCCTTGACTCCTTGCACTCCTTGCTCCTTGTTATCTCCTTGTGCTCCTTGAGTAGCTCCTTGCTCCTTGCTTCTCCTTGAGCCGTTTGGGACGACTCCTTGCACCCATTCTCCTTGCCAGTGATTCGCCTCCTTGTCTCCTTGTGCTCCTTGACCTCCTTGGGTTGAAAATCCTCCTTGCTCCTTGGCGCTCCTTGCGAAGCTCCTTGCTCCTTGTCTCCTTGCCTCCTTGCTCCTTGAACTCCTTGCTCCTTGCATTCTCCTTGACTTGACTCCTTGTCGTCTCCTTGCTCCTTGGATGAAGATGCTCCTTGTACACGTGGCTCCTTGCGTGACCTCCTTGGCTCCTTGACCTCCTTGGCTAGCCCTCCTTGGTTAACTCCTTGAAATGCTCCTTGCAATGCCTCCTTGGAAAACTCTCCTTGGACTCCTCCTTGACTCCTTGTCTCTCCTTGCTCCTTGCTCCTTGCTCCTTGACCTCCTTGCTCCTTGCCTCCTTGTGGCATCCCTCCTTGCTCCTTGACTCCTTGTGGATCATGTCTCCTTGCTCCTTGCCTCCTTGTCTCCTTGCTCCTTGACCCTCCTTGCTCCTTGAGGCGCTCCTTGTGGCTCCTTGCTCCTTGCGCTCCTTGCTCCTTGGTGTCATCTCCTTGCTCCTTGCGACCTCCTTGTTCTCCTTGCTCCTTGTCAAACTCCTTGCGATTCTCCTTGGACTCCTTGCTCCTTGGAATCTCCTTGCTCCTTGCTCCTTGTGTATTCTCCTTGCTCCTTGTCTAACTCCTTGCTCCTTGACTCCTTGGCCCTCCTTGCTCCTTGCTCTCCTTGCCTCCTTGGCTCCTTGCGCAAACTCCTTGGGTCTCCTTGGGCACTCCTTGAG"
      val pattern = "CTCCTTGCT"
      patternCount(text, pattern) shouldBe 28
    }
  }

  feature("frequentWords") {
    val text = "TATATCTCGCCGATCTTCGCTGTCCGGACGATCTTCGCGATCTTCGTATATCTCGCGCTCCAAACTATATCTCGCGCTCCAAACCTGTCCGGACTGTCCGGATATATCTCGCCGATCTTCGCTGTCCGGACGGCGGCAGCTGTCCGGATATATCTCGCCGGCGGCAGGCTCCAAACCTGTCCGGACGATCTTCGCTGTCCGGACGATCTTCGCTGTCCGGAGCTCCAAACCGATCTTCGCTGTCCGGAGCTCCAAACTATATCTCGCCTGTCCGGAGCTCCAAACCGGCGGCAGCGATCTTCGTATATCTCGCTATATCTCGCGCTCCAAACCGGCGGCAGCTGTCCGGACGGCGGCAGCGGCGGCAGGCTCCAAACCTGTCCGGATATATCTCGCCGGCGGCAGGCTCCAAACCGGCGGCAGCGATCTTCGCGATCTTCGCTGTCCGGACGATCTTCGCGGCGGCAGCGGCGGCAGGCTCCAAACTATATCTCGCGCTCCAAACCTGTCCGGACTGTCCGGACGATCTTCGTATATCTCGCCTGTCCGGACGATCTTCGGCTCCAAACGCTCCAAACGCTCCAAACCGATCTTCGGCTCCAAACGCTCCAAACTATATCTCGCCGATCTTCGTATATCTCGCCGGCGGCAGGCTCCAAACCTGTCCGGACGATCTTCGCTGTCCGGACTGTCCGGACTGTCCGGACGGCGGCAGCGGCGGCAGGCTCCAAACCGGCGGCAGCTGTCCGGATATATCTCGCCGGCGGCAGGCTCCAAACCGGCGGCAGCTGTCCGGAGCTCCAAACGCTCCAAACCTGTCCGGACGGCGGCAGGCTCCAAACCTGTCCGGACGGCGGCAGCTGTCCGGACTGTCCGGACGGCGGCAGGCTCCAAAC"
    val k = 12
    val expectedResult = Set("GGCGGCAGGCTC", "CGGCAGGCTCCA", "GCGGCAGGCTCC", "GCAGGCTCCAAA", "CGGCGGCAGGCT", "CAGGCTCCAAAC", "GGCAGGCTCCAA")

    scenario("frequentWords") {
      frequentWords(text, k) shouldBe expectedResult
    }
    scenario("frequentWordsMap") {
      frequentWordsMap(text, k) shouldBe expectedResult
    }
  }

  feature("frequentWords ThermotogaPetrophila") {
    val text = Source.fromFile("src/main/resources/ThermotogaPetrophila.txt").mkString
    val k = 9
    val expectedResult = Set("TTTTTCTTT")

    scenario("frequentWords", SlowTest) {
      frequentWords(text, k) shouldBe expectedResult
    }
    scenario("frequentWordsMap") {
      frequentWordsMap(text, k) shouldBe expectedResult
    }
    scenario("fastFrequentWords") {
      fastFrequentWords(text, k) shouldBe expectedResult
    }
  }

  feature("findPattern") {
    val pattern = "CTTGATCAT"

    scenario("example") {
      val text = "ACAATGAGGTCACTATGTTCGAGCT"
      findPattern(text, pattern) shouldBe List.empty[Int]
    }
    scenario("VibrioCholerae") {
      val text = Source.fromFile("src/main/resources/VibrioCholerae.txt").mkString
      findPattern(text, pattern) shouldBe List(60039, 98409, 129189, 152283, 152354, 152411, 163207, 197028, 200160, 357976, 376771, 392723, 532935, 600085, 622755, 1065555)
    }
    scenario("ThermotogaPetrophila") {
      val text = Source.fromFile("src/main/resources/ThermotogaPetrophila.txt").mkString
      val pattern = "ATGATCAAG"
      findPattern(text, pattern) shouldBe List(309348, 545367, 684487, 1176668, 1751372)
    }
  }

  feature("findClumps") {
    val text = "GTACCAGGTATGGCCATGTTACCGCCCTGGCCGAAACCCAGCTGCATGGTTTCACATTCAAGATTGCAGCGGCTTCGATTGGGTTCTAACGGACGTTTGCCTAGATTTCGCCCCGAGGATTGTACAGTAATTCCGTGCAGCGAGGACAATGGGCCACTGCCGGGGTTCGAACTCCAACTACCGCGAGCACATTTGAGCAAACCTATACACAACAAGATGGGATCACTTTATACTAATCGAGAGAAAATGGCGATTCTCTTCACTATGCCTATGCATCGCCTCGTCTTGGTGATCTAATTTGAATTGTGAAGTAGACTTCAAGTGTATAATTCGAGTCGAATCAGCCTTAATTCTAACCTCGTTGTGCTCGTCATCAGTAATAGGCCATAAGGCTTGTCTGCCTAGCATCTATTGCGCCGAAAATGCCAGACCGCATTAGACCGCTCCGCTACGCATTAGACCTTAGACCTTATCGCATTAGACCATCGCATTAGACGCATTAGACCGTGAGTAGCGCATTAGACCTCTCCGCATTAGACCCGCATTAGACCACCTCGCCCCATAAGTCTCGCATTAGACCGAGGCGCATTAGACCACCGCATTAGACCGCTTGCGAAACCCCGGCGCATTCGCATTAGACCGCATTAGACCTTAGACCACGCACTGTCAGCTTCTCATAATCCCCGCATTAGACCCCCCCGCATTAGACCGACCATCTCTCGTTGTAGAGCGCATTAGACCCCTGGATAAAACGGATTCCTCCATTAGCGAGCAAGTATGGCCTCACGCCCTCCAAATGTAGTCACTAGTGACGCATTAGACCACGGGGCGCATTAGACCGCAGAGCGCAGATACGCATTAGACCGCTAATGGAGACTGGTGGCGATTAGAGCGCCCACGCATTAGACCTAATTACGCGCCGCATTAGACCAGACCACACGGGTAGCACGCATTAGACCACCTTATCTTGCGTGAGATTAAGATAGACCGCATTAGACCTGTCGTCCTGACGACCGACGGACCGACGTGTATTTAACTTGTGTCTTGACCGACGTGTTCTTTAGTATATCCCTTTCTTTCTTTAGTATATCTTTAGTATATGATCTTTAGTATAAGATGGCGATCCGCCCAATCTTTAGTATATAATAGGAATTCTTTAGTCTTTAGTATAGACCGACCGACGTGTTCTTCTTTAGTATAAGTGTGGAAACCTGAAGTCTCAAAACGACCGACGTGTGGCGACCTCTTTAGTATAACGTGTATCTTTGGAGACCGACGTGTCACGCGCTCGCCCAACGTATCTTTCTTTAGTATATAGTATAGTGTCAAAGCTGCTATGTAGGACCGAGACCGACGTGTATCAAAATTTTCACAATTTTCACCACAGAAAAAAATTTTCACGCGCAAAAAATTAAAATTTTCACTTAAAAATTTTCACTAAAAATTTTCACTAGACGTTCTTTAGTAAAATTTTCACGTGTACGACCGACGTGTATGGGATGACCGACTCTTTAGTAAAAATTTTCACATAACTTCGTCTCTTTAAAATTTTCACACTTTCACAAGTAAAATTTTCACTTTAGAAAATTTTCACAAAATTTTCACTAACAGGCTTTCACTCATCAGTCGAAAATTTTAAAATTTTCACTGTGTCACGCTGGCATCGTACTCTGAAGAAGGTACCGTATAAAGCTATGTGCAGTTGGTGAGGATGCGGCTTAAAATTTTCACTGGAAAATTTTCACCCAACGCAAAATTTTCACCACAGATTAACCAGTTCAAAATTTTAAAATTTTCACTTTCACGCACCCCGAATGTAGGCCCGCATTTCCTTTAAAATTTTCACGATGTCCGTGTGACACGGGTTCAAAAAAATTTTCACGACACGGGTTCTAAAATTTTCACGTTCGTGAGACACGGAAAATTTTCACCAAAATTTTCACCCGTTAAAATTTTCACCCAAGATTATGACACGGGTTCCGACACGGGTTCGGAGCAGGTCGCGTGAAGACACGGGTTCCCTGACACGGGTTCATAGCCCCAAGACTGTCCTCGACACGGGACACGGGTTCCGGGTTCCCAGGACATCAAATTCTAACGCAGGCGACCATACCGCGGCATGCCACTAAGACACGGGTTCTTTGACACGGGTTCGGTTCGGACACGGGTTCGGTCGGTTCGTGACACGGGTTCTCTATCTAAAGATTGGACGGTCGGTGACACGGGTTCCACGGGTTCCGGGTTCAAGATGTTAGAGGGGGGTCGATCATGTGGTATATCTTTCTGCAGACACGGGTTCCATGTCCGCAGCAGGACACGGGTTCTTCGAACAAAGACACGGGTTCTATAGATTGGACTAGGATCAAATGCTGGCCCAGGGACACGGGGACAGACACGGGTTCACCGCACTATAAGGACACGGGTTCCGATTCCGACACGGGGACACGGGTTCCGGGTCATTACCAAAGTAGAATGTTGTCTACCGACAGACGGCCCCCCGCTTCAGATACCCAGATGTGTACGGATTTACGTGTCTCTGAGCAATGCACAGCCTGCATCTAATTAGGTCGCATTGAAGATAAATCCCCCCATAGCTTATATTAGGGCCTATTATGAAGGCCTAGTTGTTCGGTCGCAACCCTACATGGCAGCTACCGGCTGCTGGACAATTATACTACTCCGTGCCGGAAAACTCTGGAACTTCATGTGAAAACCGAGCACGCAGGATAGCGTCTTGCCCTCTAACAAGGTAGACCAGTCTATCATGGATCATACGTGGTATGGACTGATATTGGGAGCGTCGTACTGATATTGGCGTGGATACTGATATTGGTTACGGAGTAGATACTGATATTGGGAGGCATCTCACTGATATTGGTACCCTTTGACGGTACTCGTCGGATTAATACACTGATATTGGTATACTGAACTGATATTGGTCGCACGGTAAGAAAGAGCTAAAGAGCTCGTTAAAGAGCTCGAAAGAGCTCGTCGGCAAAGAGCTCGTATATTGGAAAGAGCTCGTGACGACTGATATTGGGACATTGTAAGACAAAAGAGCTCGTCTCGTCTGATATTGGGTCAGGTCCGATTCTAGGCGAGATGGTACTGATATTGGAAAGGCATTGTAACAAAAGAGCTCGTGCACTGATATTGGCATTAAAGAGCTCGTCTCGTAGAGCTCGTTAAGACAACTAAAGAGCTCGTGATATTGGGATATTACTAAAGAGCTCGTGACTGATATTGGAGGTGCACTGATATTAAAGAGCTCGTCTCGTAAAAGAGCTCGTGACCAAAAAAGAGCTCGTTGCTTCAGCACTGAACTGATATTGACTGATAAAGAGCTCGTGAAAGAGCTCGTTAAAAGAGCTCGTGAAAGAAAGAGCTCGTTTCAGCCCAAAGAGCTCGTGAGCTCGTCGTGTTCAGTTCAGCAAAGAAAAGAGCTCGTAAGAGCTCGTTTCAGCCCACGTAAAAGAGCTCGTATCAACCCAAAAGAGCTCGTCCCACGCAGAAGCATTGTAAGACGTAAGTTCAGCCCACGTTGCATTGTAATTCAGCCCACGTTCAGCCCACGTGTTTCAGCCTTCAGCCCTTCAGCCCACGTTCAGCCCACGATGGACAAATTTCAGCCCACGTACATTCAGCCCACGTAAGAGGTAGGAGGGCGTCGCACGAGCAGTTCAGCCCACGTAGACACATCTGGCTTATAATGTTCAGCCCACGCGGTCTTCAGCCCACGTGAAGATCGGTACAGAAGATCGGTACCAGCCCACGACGGGCAGCTGGCAAGATCGGTACTCAGCCCACGAAGATCGGTACGTACTGGCATTGCTAAGAGCCAGCTGGCATTTCCTATCCACACCTCGAAGATCGGTAAGATCGGTACTCCTTCAGCTGGCATTATTACCAGCTGGCATTGCATCTATCGTGGTGCTGAAGATCGGTACGCTGCAGCAAGATCGGTACCTGGCAAGATCGGTACCAGCTGGCATTTTCACTTGTTTTCTGACAAGATCGGTACGAAGATCGGTACTCGGTACGGCATTGGCAGCTGGCAAAGATCGGTACCGATTTCTGACCCAAGATCGGTACGTACTGGTATTCTGACCCGACAGCTGAAGATCGGTACTTTCACAGCAAGATCGGTACACCCAGCTGGCAAGATCGGTACAATTAACTGAGTGGACAAACAGCTAAGATCGGTACGATCGGAAAAGATCGGTACCACGACCCGACTGAGCAGAAGATCGGTAAAGATCGGTACGGTACGGTACGCAAGATCGGTACGGCATTCCCGATCAGCTAAGATCGGTACGCTGGCATTCTGAGTGTTATCCAGCTGGCATTTCATTTGAGTGTCAGGCCATCGACTCTTACGCTAGACCCTTATTCTGACCCGATCTGACCCGACCCCAATATCGAAACGATTCAATATCGAAACTTATTCTGACAATATCGAAATTACTTAATTCTGACCCGAGGAGTGACTCCTTCTCACAATATCGAAAAAGAGTGTTAACTGAGTGTTCTGCAATATCGAAATTGGAGCTCGCTGGAGCTCGCCTTTGGAGCTCGCCCTTGGAGCTCGTTTTGGAGCTCGCAAATTTTGGAGCTCGCAACTCAATATCGAAACAATATTGGAGCTTTGGAGCTCGCTCCCCCTTTAACCAGTCACAAGCAATATCGAAAAATGGTTCACAATATCGAAAGCGCTCAATATCCAATATCGTTGGATTGGAGCTCGCATGTTTCCGCAACGGGATTGGAGCTCGCGGCATGGAGTAACTTGGAGCTCGCGCTCGCGTTGGAGCTCGCCTTGGAGCTCGCCTTGGAGCTCGCGCTCGATCAATATCTTGGAGCTCGCCTCCAATATCGAAACAATTGGAGCTCGCAAGTAACACTTGGAGTTGGAGCTCGCTCGTTGGTTGGAGCTCGCGCTCGCAACTCGTTGGAGCTCGCTGCAATATCGAAATAGAGGCAATTGGAGCTCGCGACATTGGAGCTCGCTCCCCGACGCTATCCCATTGTTGGAGCTCGCGGAGCGGAATATTCGACTGAGTTTGGAGCTCGCCTCGTTGGAGCTCGCAGGTGGGTTCGAAGGCTCGCTGGCCAAATGTCCGGAAGCTGCGGTTGAAGCAAGAGTTCTAATCCGCTCTGAACCAGGGTGTGGACACCATCCACTGCATGTGGGCAGTTAACAGTGAGTGCAAGGCAACAGGATTCCCTCCTTGACTGGGGCAAAAGTGTTAAGTGCTTATACAATAGATTCGAAATAAACAGGTTCGCCTTTTCTGGAATGGTTTGTGCCCTTGCTTTGCTTTAGGGGGGATCTGTGCTTAAATAACCGTGGCGTACTCTTAGTAGCAGCAGCCCAGTCAATGATGATGGCACGGCGGACGCTGAAGACTTGTACACGGATTAAGACCCTATTTTGGTGACTATGCACCGCGGTCTACTTGTGCTCGGTCACCTCAAATTGTAAGACGGAAATCTTGTAGGTAATATACTGTAAGGCCGATCCGGCGAAAAGTCTAGGGCATCCTTGGCGGCGATTTTTGTTCCGAATGCGACCCAGATTAGTTTCGGGAGACTAGAAGACCAGTAACTCCCAGCCGTACAGACGTATTCCCGCGCGAGTTGATACGTGCTAAGAACCACCGGTTGGTTTGAGTAGTGGGATTGTAACATATGAGGGATGTTGATCGAACCTAAAGTGAGCGCCGCACACGAATGAGATTTACCACCGTAAGTGCTAGGGTACGGCGAGAACAGAAAGCGGATCTGAGGTGTTTCCCTGCGGAGGACGGTGATATAAGGCAGTGGTACCCAACGGAGGATGTGTCTGTCAGTCCCTCCTTGGCGTTGGCGCAGTCTAAGGTCGGAACCATTGGCTCCAATATCCTTGTAACTTGCAATTAGCCAACTTAATGATAGAAGGTGCAAAAGCCAAGAAGTGTGCGACCCTGGCGCACCAGTTGACGCCTGGTGAGCAGCCCCCAGACATAAAGATGCTCGAGGTAGGGTCTCACATGTCCTTAGATCATCTCGTCTGCCAAGAATGCACGGCGTGTTCGCAAGCGCTGAAGTCGCCAGGAGAGCAGCCGTCTCTCTATCCACGATTGAAAGGTCTGTTCACACTGGCAAGCCCCAGGCCAATCCCTCTATCCGACTCGGCCGTAGTCTGCTGACTTGACCACTCTTTAGACTGATATTGAGATATCATCCAGGCCAGGGACTTGGAATCTGGGCTGGGCATCGTGTATGATTGCCATCGCTCGAATTCCTTTAGCATCGCCCTGACGTGCTGCAAGGTGAAGAGGAGTGAAAGACGACAGCCACCGATTGTCGATGCATGCTCGTCGAGTGTCGGAATTACTAGTGTAATTCGGCATACCCCCTCCTCCTCGTCCCCTTTTTTGTTATGAGACCGAGGGTACCCGCTCCCACAGACAGCCTTAAATTGACTTCTAGAAGTCTAATATTTGCGATGTACGACCATGATTTCCACCCGATAAACGCTGACTTGGAGATAGTCCTAACGGAGGAAAGGGTTGTCGTGCCGTTAGCTCGGATTGTTATCTAGGACACGGATGGCCATTCCGAGCTAAGGTAAGGACCCCGGGTCCCCCGGGTTAAATTCAGGGGCCCGTACCACACGTGCGGCACGTGCGACCGCATCGCCCGAAGTATTAAGGTCCGGTAGACCCGGCATCCACGGCTCCCTCACCCACAGTATAAGCCCCAGGGAGCGTGATCGATTGAGCCCCAGGAAATTGTTAATCGCTTTTCAACTGAGCCGTTCTCTCGCCATTCTCGCCATCGACATCTCGCCATCGCGCAATGCTCTCGCCATCGTCTTCTCGCCATCGATATGTTCTTCTCGCCATCGTCGCCATCGATCTCTCTCGCCATCGCATTCTCGCCATCGCTGGTCTCTCGCCATCGAGTCTTCTCGCCATTCTCGCCATCGGATCTCGCCATCGTTCTCGCCATCGACCCTATTACGATAGTCGTCAAGACAACTCTTGTCTCGCCATTCTCGCCATCGGACATACTCTCGCCATCGCGCTCTCGCCATCGGCGCTCTCGCCATCGCCCTCTCTCGCCATCGCACCCTCCAACACGTGCTCGTCTCGCCATCGCGTGCGGCATTATCTCGCCATCGCGACTGTCTCGCCATCGGCCATCGGGATGCTTATAACTGCCGATACACTGCGCGTTTAAATCTCGCCATCGATCATGGTAAGCTCTTCATTGGTACCTTCGAGTCATAGAAGAACGCTGGACGAAGAGATTCTCGCCATCGGATTCGCGACGCTCTCGTTAGGGAGTTCTCGCCATCGGCCGTCCTACTTTACTGTACCCTAGTTAGTCTTCTCGCCATCGTGCTTCGCTGTTGGACGATCATACTTCCACTTATGGGACTCAATAGTTTACGTCCGAGCCTATGTCGTGTTGCAATGTTCCCTCCTCGCTCAGCACGCTCCTTAAAATATGTCCTACGATGCCAACGGCAGTATGCCGGATTCTTACGAATATCGAAATTTGATCCCCAAAGTTCATGAAATCCGTTGTTGCTCATCTGTGGTCAGTTCCACGAGTACGCGTGACCGGTAGACGTAGTCTTTTTTTAAGTAGGAGCGTGTGTACGTAACTGGTATAACCCGTGAAATCGTCACAGTGACCTATCTTCCAGTCAGATTGTCCCCACGTTCCTACGAAGGGAAGACGGATGAACGGGGCATTCGTATAAACTGTAAGTGGGCACCTATGCGGTAAATGAGCGCGAAGCGGGTAAAGCTACCATGGAAGGTATGCTGTGTAGACAGCACTCGACGAAACAATGCGGGTATCAGGTGCTCTTAAACTACCGAGGCTCACAGCCCAGTGCGGTGGCTCGCCAAAAAGAGGATCAGATTATATAGACTTGAAACCCACCATGGATGTTGTTCAGTTGCCTAGATTCGATCCTAGTACAGACGAGATGCCCGGGTTCGTAATATGTCTTGGAGTCAAGGGAGTCTCTGTCGTTAATCGTTGGAGACGGTCTTGGCTCTCGCTCTTTTTGATGGCAGCCCCTCCCCGTTGATTCCAGAGCCTTAGGACCTCGAAACATCCTCCAAGGACCGATATTCAGCGCGTTGAAGGTGATAATTCCGTGGTGAACGTGGACGTTAATGCGATTCTGGAATTCCACCAGCGATGCACAAGATGTTTAATCCCTGCGTAAGGAAAAGCCTTGACGTACAATCACGTCCCGGTCGACGCCGTCTCAACGTTTGACCGAGTAAATAGGACCAATATAGTATGGTAGGACCAAGACCATTACGCCTAAGCGCAAAGGACCATTCGGGGCTTCATTACCCGTCGCCTGGATAGCTTCACCGATTATGATGCTAGTTTACGTTGACTTTTTTGGGCCTGTTGAATGAGGCTAACCTAGGGGAGACCCTCCGTGTGCTGGTGTGACAAAACCGACCATATGTCTAGGATTACGGTAGTCTGATACAGTACAATGACACGCTAGGTAGCCGATGTTTTCATTGGAAACAGGGGTTATTGGTGAGGTACTTCCCAAGGCACAGATCTATTACCACCCCAATCGTTGCTTCGATCAAACTAGCTCCATTTGTCCGATCCGCAATGTCTCTAGACCCCGGGAACACGCTGAGGGGAACCTATAATACCGACAGGCAAGAAATAACACAGGTAGATCTTTCGCTTCTTCCAGATGTGACTCAGATAAATGCCTAGGTGGGAGCTCTGCTCCTCTTGACGCCTTATGGTCATCGCAGTAGTACACGGCCCCTCCTCGTAATGGTGAAAGTAGAAGCCTTGGACCTTACCCAGACAAGAGAGGTAGCTTGAGCATGAACACTCAAATAATCTGTGCGTCAGACACACTGCCCGCAGTGCAAAGCCCAATAGAACAGACGGGGTTGGGCCCAGCCGAGAAAGCTCGGCCTTCACGCGAGTTCCACTGTGCTGACCGTGAACACGGCAGAAGAATAAGCCAACACCGACGAAGTAAAACCATTCGTGCCCAAATTCTTTCCCTTTGAGATTAATTAAAAGAACATGCCCGACCTGAGTAGACATTGGTGTTTCTTCGCTCCAAGCCGAGCGTGATTCTTCACGTCCACGTCGACGATTGTCCCATGCTCACTGAGCGCCATGCTCGCTGGCCACAGCCGCCGCCCACATGCTACCGATGTCAAGATTAGTCATATAAGGGCGGGATTAATACCCTGAGATTAGGATCACGTGGCCAGAACAAAACAAATTATATTTTTCCTATAAATGCCCTGGTATTCCCGCTGACCCCGTTCATCAGACGCGCCCAAACTA"
    val k = 11
    val L = 501
    val t = 18
    val expectedResult = Set("TCTCGCCATCG", "TTGGAGCTCGC", "AAAGAGCTCGT", "CGCATTAGACC")

    scenario("findClumps") {
      findClumps(text, k, L, t) shouldBe expectedResult
    }
    scenario("fastFindClumps", SlowTest) {
      fastFindClumps(text, k, L, t) shouldBe expectedResult
    }
  }

  feature("neighbours") {
    scenario("example") {
      val pattern = "GAGTTGAAGCA"
      val d = 3
      neighbours(pattern, d).size shouldBe 4984
    }
  }
}