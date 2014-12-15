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

package specs

import org.scalatest.FeatureSpec
import org.scalatest.Matchers._
import weeks.Week4._
import weeks.{DNAMotif, DNAString}

import scala.io.Source

//@DoNotDiscover
class Week4Spec extends FeatureSpec {
  feature("compositionKmers") {
    scenario("example") {
      val text = DNAString("CAATCCAAC")
      val k = 5
      compositionKmers(text, k) shouldBe DNAMotif("AATCC\n     ATCCA\n     CAATC\n     CCAAC\n     TCCAA").value
    }

    scenario("extra dataset") {
      val text = DNAString("ATTGGTGACACAGAGATCACTGGCCTGGTGCGCTTTCTGGTGATCACATTCATAATTGAGGATTACTTGTCCTCTCTTGAGAGTCGCAGCACACCACTTTTAGCCATGGGTCGTCTATAGTTCTTAGAGGCGGTTCTCCGCTTGACTAGTTGGATGCTAGAGCTTAAACACCGCGACGCTGTATTACAAGGGCGAAGACTTCCCCATTGTCTGATTGACCAGCTCTCGGAATTCCACGGGAAGGCCACCGATTGTTTGAGGAGGCGGGGAGGAGCAGTCAGATGGGGCCTCAATCGAAGGGGACCACGCAACCCCCGTCGTGTAGAGGTCCTCTAGATGCCGCCCGCAGGCGACGTAGCGATTCTCGAAATGCGACTCAAAGTAGAGAATAACTGCCGAAATCTCACAACGAATGGGGTGGCACCTGCTGCCTTCCGTTGGAGTGCGTTCACCAACTATTGTAAATAATGCTGCCGTAACATGTAGAGGAACAGCAGCCCTAGGGGCAAACGTTACGCAGTACATTTCTTCTCCGGTGCAAACCGCTCGAGGAATCGCACTTATTACTTTGCGTAATCCAACTTTGCGCAGAAACGCCTGTCAATTCCCACTGAATAGATCTAATGTACCCGGTGAGTACTAGAGGTCAGCGTTTCAAAAAGGGAGGCTCAGAAGTCGGGGTATTCACGCGCATTTTTTCGACGGTGCGTGATTGATCATGTCCTATGACTCATAGTAATAAACCTTTCCAACTTCAGTGTTGTCCACAAGGCGGACTTTTATCGAAGTGAGGAGCAAAATAATATCTACGTTAATCTTTAGGACTGCCCTGAAGTGTAGGCGCCCATATGAAAACGCCGCGGGGCGTTAAGAGGAGTGATGGTATGATTCTTCCCCCGCGTAACTCCAACACCAAGATTGTCCGGATCGTTGACCGGAAACGACCCCGTGCATCGCGACCGGGTTATCACGACACGACCCCGCAAATAGACTATGCCCGGCACCTACGAGTATGTATCTTTCCAGTCATATCAATATGACACGTGTAGGCGTGGAAGTAAAGAAGGATGTACAACCGATCTGTCCTCTAGATATAGCCAAGTGCGCGTCCGGGGCGGTCGTTAATCAAAAGCTCGTCCTGAACTTTCACGAACACGCATTATCTCCTGGCGCGAGCACCCCCGCGGGTGCGGCTCCGGGATCAATCGATCATATCCTTAGAATCGAACGCATTGGATTTCATGTGACAAGGAGTTAGGTTCTGGGAAGAGCTACATCCTTCACGTCCTTTCCTCTAAACCTAGTGCACGCTGTTAACGAACTGGAATGAACGCCTCAGTTTTTTAGCATAGGTAGCATCTTACAGGTTACCACGTCATTCCGTTGCCTCACCGGCACCCGCCTGCTCCGTATGTTGCCTACTCGATTGCACGCCGTGGTTCTTAAAAGACTTCGCAGGCCAGACCTCGGGCCCGTAATATACAAGGTCCTCGTGACTAGACAAACTCCCTGATCAAGAGTCATCTTGCGCATGAGTACGACGTTTCTTTAGTACTAATTACTTGGCTCCCGCCACCGGCTTCTCGTTTCACTTTGAGGACGGTGCAGAGGGAATATCCGTGCTCACAGTGTTCCTGGTAGCCCTCATGAGTTGCTTGCAAGCGACGTATTCAAGCTTTTAGACGTCCAACATTGGGATCCTCCATGACCATAAATGCTCCGGAATTTTAGAAGCCCGAACAACGGGGAGGTACAGACGATTTATAGCAACTCGTTAAGGAGTTGACGAACTATCTACGGTGCCGGCTGAGCTTGTTTCACTAGACTTCTTGAGGGCGTGCGTACGAGTCTAGAGATCCCGCTAGAACTAATCGCTACTCTTGCTCCCCTTTCCTCTTGTACGTCGTTGTGGTAATCTACAACGCCTGGTATCTGTCGTGGTGGTACCTGAGTGCACAAGCCACAGAGGAAACGCATGGGCCTGGAGCGCCGATGTGTGATCAGGTTGACCAACTCCAAGACCATAGTACCTCAAGAGCGATATAATTTTTTAGAACTCCGACCCTCCGTACAAGGGCACAAGTCGCGATCTCAGTCCGACCATCGACATATAACAATACTATTTCTTTCTTGTGGAACACCTACTGCCCACTTGGAAACTAGCCATAAGATGTGCCGGGATGTCTTTCGTCTACAACAGAAAACCGTCCCTAGTCATTAGCTTATCGCCATCGCAGGGCTACCAAGTCTATTCGGAGGGGATTTGGGAGAGGCGCGTCGGATTTCAATAGAGTGTACATTTGGGAACCCCAGCGTGGGTGGACGTCCGACCGCACGGTCTTGCTGATTTGCTCCGGCAACTGGTATGTTGATCCCAGTCGGGCAAGGCAGAGACGTATTTCCCGTCTGGATTGCAATTTAGGGTTTACTCCGCGACGCACATCTTGCCTCTTACCTCTCGTTCGTATGTGGATACACCTCCAGAGTATCCGCAAGCTTTCGCACGTCACCGCTATTAGAGTGCGCCTGATACACGGCAACGTGACATTTGTGGCCACCGTATTGCCTGAGAAGCTCCGGACCAGATGGCGCGGAAGCACGCGTGTATCGTCGTATCACTCGTACTAATGATTAGCCGCGGCATATTGATATTCCATATTGGTATGGCATTTATTACTCGCTACGAGACTACGACTGTCAAGATCCGAAACCACTAGGCGACTCCACGATAACGGGCCTAACCGTGAATGAGATAGTCTTCACGCAGTCAGTGAGATAGGAAGAAACGAGCGTTATACAAGTAAGTGCACCGGGCGCACGCACTCCTGTGCACGCCGCACATTGGGGTAATAAATAGCATCACGTTCTCGTTTACAGTGTCGTAGAGTAAGGCACACCAATGCAGACCCATATAAATCAACTGTAGGTCCGCTTTCTGGGTGCTCAACCAATGCGCGTACTTTCATGCCACCGCATTCGGTACAGTCCGTACTGCGCCCTATACGGAATGGTAAGCGTGCTCGGATTCTATGTAATTTGAGGAGTCGCTAACCAACAGTGTGGGGTATGTCTTCGGGCGCACACATTATCGCTGGTATCTGTTGGCAGCGAGGGTGAGTCACTGGCTTAGTTCGGACTGGGGCCTACGTACAGTTCGTCATCTAATTTTGACTAAGCAACTTAGATTACTTTCGTGCTCTAAACGTCCTGAGCTACTAAGGTGCCCAATACATCCATTCTCTTTGGACTCACACTTCGCACTGCCTTGGTTGTTAGCCCTTAATAAATTGGACGTGCGATATTGGACCCCCATGGACAGTATAAAAGGTGCGCTGGGTTGCTCGGCAAACAATATTGGGAATTCCCCCCAGCAGGGCATAGACACGCGCAGTTCGCCGAGGATGTCCCACGGTCGCCGTTACTATTCAACACCCCCTAGACGTCACAGTAGCTAGAGGATACCATCGGCTCAAACCGGTGTGACGGTAAGGCCTCATGCTTCCAGGTTAGTGCCATCCCGCACTCGGTAGCGAGCTTCGGATCTATCGTGTGCATATGACTACACACGTCGATAACACTGGTAACCGAAATAGTAAGCGCGTAATTAGGGGGACGTTTGAATTGCATTAGAAGGGGGCTAAAGGTTCTCAGCAGGACAAATCGTGGTTGAGAGATTGTTAGGTAAACAGCAAACGTCGGGTTACAAAAAAGAGCTAAGAGCGACGATCGCGCCGGACGTTAAGTTCAGTCACTCCCGGACCCCTGCGGTGTACAGACCATACGAAGCCGTCACCGCTTTGGTCGTGTATTTGGTCGGTCTCCGAAAGTTGCTGCTCGCAGCTAAATAAAGCATCTCAGCACTGGTCGGGGAAGCCAGTTGAACTCAGTAAGCTGCCGGACTTTCTCGTAACTGTCAGCCAATTGACCATTGGATTTCGCCTCCTAAAAGCGTGACAGGCTCCGAGATTTACCGCGCGTCACGCCCGTCTTTGAGCTACTACAATCTATAATAGACTATGGTGCTTTCCCTGGTTAAAAACGAAACTCGAGCGTGCTTCCCTAATGTAGTGCTCCACTCCTAAGAGCTTAGGCACCAGATTCCAATTGACAGGTCCTAAGTAAGCGGTAACCCACGACCCGCGCCGACGATCGTCTAGTGTCATCGTACCGAGAACTCATTCTCGTGGCGGCTAACCAATCATTATAACAGTTTAATGAATATTCCAAACGATATGCCATTCGCTATCGTTGGGGCACGCCGGCTCTTTTGTCGGCCACGAAGAACCCCGACTAACTGCAAGACGGTGCTCCTGGAGTTCCCCACCCGCTCGCCTTATTTTGCGAATTAAAGTTCAGTAAGCGCTTTTCACGTGTAGCGCTGACCAGGACGTTATTTCCACGAGCTCCGCCAGTGCACTTAACGGAGTCATTTCTCCCAATACATATTAAGAGCCGCCTCAAGAATTAGTAACAACCTCAACGTCCGTATCCTACGGTAAATACGCTACGTTAACGGAAACCCGTTTTGCCTTATGGGATGAATGGAATCCTGTGCCGGGAGGCCAGATCTGGAGCTCGGGACTGTGTCAGAAATTCTTGCCCAGGTTAACCTGTTTTAAACAGGTCGCTAGAGGAGTAAATCGCAGCAATGTACTTGTCTTATTTATATTATATTGCCAATACCGTAATGCGAATCTAGAACGGGTGACACTTTGAGGTTAAACCTCGTCCAGTATCCGCGTCCGTTTGCTCTGCGCTTTTCTCCCTGGACCAAAAACGGTACGTGGAGTAGCTTTGGTACCCTGATCACGTTGGTAGTCTTAGAGCACAAGCTGGAGGGTAGAGCAGATGGAAAGTAGGCGATGTTTGATGACGACACTCGTCGCATTCGGCCATCTTCGTAGGGCGATAGGAAACCACCTATCTCGTTTGTAAAGCGCCCGCTGTTGCTGAGTTATACGTCGTCCTCGAGTGCCTGTAATCATTGAGACCATAATGCGAGAACACAGTTTTAGTGTAAGTTTATCAGGTGGGTGCATAAGAACCCTCAATCGCTTGTAAGCTCGATCACGTATTTCACGTGCGGAGTCAGGAAGTTGTCTTAGTGCGATCCCGGACGAGCTGGCCTATCAGGCACCGCCCCGATACGGGATCTGCGGGGAATACGAGTCGAAGGGGGTACATCTAGGAGGGGGGAATCCCGCAGCATCGGCAATCTCGTTGCTCTTACTCCTTTTATGGATCCGCTAGTGCGGTCTGCTAGAGGACACTCCCGCCAATTGTTTCGGCCCGAATCACAACGCATATGGCGAGCCCCAGTTATGGAATTATCGTCTGTACATTCACCGTGGCTGATTGGCCACAACGCCGCTTACTAATCGTGAAGCGATCGCTCTGTGGGCAACGGGTGCGATTTCCTTCTCTATCAACAATTGATGTTGCTCCTATTCATCAGCAGTAAGTTCGCAGCTTAGAAGGAGAGAGGTGCTAAGGCTGCCAGGTTAGATTTAACACGCAAGCGAGTACCAGTACAATTTACCGAGAGTAGCCTAGTCTAACTCTCATGGCGTGCTGTACTTCACTTGAATCTAGCTAACGAAGAAGGGGATAACGTAGCGCATTGGTCAAAGAGCACCTCAGTGCTGAATCGTCTCTCCCGAGGCTCTGCAAGGCCGTACCGCAGAGACTCATCTGGACGGTCTTTTACTTCTCCTGGGAACAGCGATAATGCCCAAATGTCCGAGATCTGTAAAAGGACAGCGCTGCTATGCGATAGACCGGGGGCGAGTACACCCCTTACAACCCCCGAGATCGCAAGATCGAGTATCAAGTCCTAGGCTTGAAACGGCGTAACCGTGGTGGTGGAGGTGCGAGCACTAGTCAATACTCAATATGCCCTCAGTGTTAAGTCACGACAGGCGCCTGGCGGGAAGCTGAATCAGACACGGAGCCTTTTTCGATTGCCTTGGGATGTGTTCCTCACGTTATTGCTTATGCCCGTCGGAACATCTTTTCGCGGTATTACAACCAACGTGAGATTAGAAGCATGATCCCAAAGACGCCAGGTACAGTGGCAAGTTTGATGGAAGTAAGTAGATCGAGATATGAAGCTTGCTAGTTTCTCGATCCTGCTCCCAGCGGGAGATGCTTACGTGAGGGGGCAATTGTCTTACGGAGGTGCCTCTGGATTCCGCTAGGTCTTTGGGGTCGCACGGGAAAGGTCCCCAGCAGAGCACGGGTACCCCTGGATCATGTGCATTAGTCCAAATTCCGTACCGTTAGGTACGTAACTGCTTGCAATCATGCGGAAAAAGTCGAGCCTTTTTTCCGTTTTACCGAGGTAGAGTACGGTTGCGTTTTGAACCGCAGAGTGGGTCTTTTCCAAGGTCGTCCGTGCTACGTATTATGAACTGTAACAGAACTGCAGGTCGTTCCGTGCCACTTGCGCCCCCTCGTAGCAACGACGCCTATACGCGCCATAGACGAAAAAAAGTGGTGCATACCACACCCCTTAGCACCACACTGTAGAGCGTAGCGTAATTGGGCTCTACTGCGCTTCTGCTTGTAGCGTTGAAGACCATTCAGTTGGAGGGTATCGCATGATTACACCGCAGATGCTGCATAAGATGGGCACAAGACGCAGGGATTAGAGGCACCACGCATAGCGTATCCCGGTGCTAACATCGACATCATATCGCCGTACGCGTGATATTGTAGACAACCCAGATAGCCAGCCTAATAGCGCATGTCTCGCTAAAGTATTGACTTCGAGCGAGACGGGTAAGGATCGGCAGTCAGTTGATACCTGTGTTCTTCAGGGTTTGCCAAAGTTACGAGGATTAGAGAAACTTCATATGCCACGCTACTCACCAGGTCGTATTGGTGAATATCGTGTACATTCCAAGCGTCCCAATCCACCCACGCGTGAGTAATGATTGAAGACTGGACATAGAGCACCTGGGTAGCGCAGGGAAATCACACCGGGCCTGTGACCCTTATACCCCCGTCAACATCGAGCGCCGCACACTCAGAGCGGGTCGGGAGATATTTATACTGAACACGTCTAAAAGCGATGAGCATGGATTTTAGCGGGGTATAGATGTTAAAATTGGCTATCACGGGGTGTCCTATGCGACCGTGTACATCTGTACTGATAGTCGGTCTGGTGTAGCCGGCGCGCAGAAAGGCCTCTGAGATTAGCACTACGACGGTAGGTTTGTTAGGGAACGTTTTTCCCAGGGAGTGTAATTGCAAGTCGATATTAGCCAGTAGTCCTTGTTCTGTGAAGATAGTCGTTTTGTCATGTCTGGAATTAATTTGATGCGGTCACCATGGGAAGTTATACCACGGCGTGCTGGTTTTCAGAGTCGTTAAACCACACAGAACTATATCCCTGTCGTGGTGAAAGTTATGCACTGGTCCGGGTATGTAGTCTATTGTTCTCAGGGAAACTGTCGCAGGCACAACGTGACAGATTACATTTTAGCCGTCTAACAAATCAGCTCTGGCTTTCAGCAATTCAATCCATGCTCCGAGAGACTAGGACCCCAGCTCTGACAGAACTACAAGAGGTTTAAAGGAGTTTATGGGAGTTAGAGGAACACGGGGCGGCGATAGCTTACCTACAACTGCTTTAGCTAACTATGAAAGTACTCGAGTTCCGATCGCTTCCCAACCATGCACTCTTTCTAAAGAGGCCTACCGGCGACCCACGGATGAACTCCACTTAAGGGACGTTGAAACTCTCTCGCATCCTCGGGTACGCCACTAGAATACGTCCGCACCCTCCTAAGGTCGTCACTCATTGTGCAGGAAACGCAGATTGACGGGATTTGTACCACATCAATGAGCTTCATATTCGTGACGATTGCACGTGAAGTCACGGCTTGATAACATAGTTCGTACAGTAGGAGACCGGTTCCGGCTGGATTCATCCAGGGTCACGGGTCGACAGTGACGCTGTCAAAGCCTCGTAATATGTTAAAGTGTTCCGGAGTCAAACCAATCCGTACACCCGCAAGGTCGTCGTAGCATAATAATTAAGTCCGCCTAAGCAAGTCGAGTCTAGTAGTTGGGTACCTCTAGAACTCAAGCGATGATAATGCATCGTCTCGATCCCATAAACCCGCGAAGAACGCCTGGTAGGCCATTCGAGGGCAGATTGAATTCCCGTACTTGGGGTATGAGCTGAAAACGATGTGATCACACCGATTCACGCACCTTTGTAACTAAAGCGCAGCCGCTGAGTGTGGCTCCTCCTTAAGCCGGGATTGAGGAACATCTCTCCCAAAGATTAGTCCGTCCTAAGCCCCGGCTACGGGAGTCAATACGAAGCTAAGGCTCCCAATAGATAATAGCCGTCTTTACTCATTTACTCCTAAAATATCTCATTCGTATGGGCATAGAAAGTATATTGCTCCCGATGCCTTCCACATCCTGTACGTCAATCAATTAGCGTAACAACGACAACCTTCCGCGAATACTCCCACAAAGCATTCATCAATGCCTCATACATTGGGGCGCCACAAGGCTGCTTCTCCAAGGTTTTAAGTATCCCCCTCCGAGTCAACGAATAGAGACCTCTAACCGAACCAATAGGCACCAAGGCAGCATTCGAGATACTTGACTCAGACATTTTCAGAAAGGCCAACGACGATCGCGCTTTGCTGGGGCAGATGACTTACGACACGCTCACGAATTCCTGTCTCGGCATCATACACGTCAATAGCAGTTGAACCACTAAAGTTTAGGCAGGGGCGCTTACTATTGTATAGGATGGCCCGTTAAACCAAGACAGAAACATGCATCGAGGGCTGGACCAATTCGGCGGCTATAAACCTGGAAGGACGGCGAAGAGTTCAGGTGTTTCTCCGTTTCCGAAATTTCGTTTAAGGCTACTAGAACTGTATGGAACCATGGTGAATAGTGATGTATGAGTGGACCTCATCTATACGGCGCGGGGATTCCGCCGACCCAGAATCCGATCTAGGCAGCAAGCTCTTTACATTCTCGTGGATCGCGCTATCGACGGTGGGCCGGTGGGGTTAGCTATTACACGTAACATGTAACTCGTCACAGGGTAAATAGCTCCAGGCTATAGTGCGCCTGCTCTTTGCCTGCAGAGACGTTGATTAATGTGCTTTGGGGAATATTTGGTGTCACCAACACGAGCCTATAGCTGCAGTACCCGGCACCGGCGAACCAACCGAAATCTCTATCGGTCTGGAGAGAGGACGGGTCACGAGTAGGTGCAATCTACTGAAAACGTGTTTGGACTCGCAAGTAGGGCCCCTGACCTTCCTGAAACGTTGTAGTTTAGACTCACCCACATGCTTGAAAGACCGCCCGATGTTAGAGTCAACCACCTACATGTTGCCTTGGTGGTGAGCTCAGGTGCCCTCACCAGTGAAGGGGGAACCTATATCACTGTGCGCGTGGTCGTCCCTACATGCTTTAACCTAACGTGATGGCAGAATCGCGTAAGATAACCACTTCAGGGTCGCGAGATTCAACATTCGTAATAGCAATTAAGCCTTAGATCAGAACTCGCATTCTAATGTTTTCTTGCGCGGAGGAATATCTGTAGTTAGCCCGGGTTGGATGAGGAATAGA")
      val k = 100
      val sf = Source.fromFile("src/main/resources/compositionKmersExtraDatasetSolution.txt")
      val st = sf.getLines().mkString("\n")
      val expectedResult = DNAMotif.from(st).get.value
      compositionKmers(text, k) shouldBe expectedResult
    }

    scenario("interactive quiz") {
      val text = DNAString("ATATTAGACTATTATCCGACATTGTGACCCCCAATGTCTTTCCGAGGCCCCTTTCCGCACAGGCTTCCGCCCTATGATCTGGACGTCGTGAACGAATGGATGCTCTACACGCTCTCGTAGGGATGTGTGGGCGACACCCTGCTTTTATTAGCACACTGAAGTTATGGTGATTTAGCACTAAAGGGTTCCGTACTCAAAAATATACGCCATTGTTTGCGTAAATTTCCTGACCGACAACTCAATCAAGATAGTACCGTAAGTCAATGAGTGCACGAAGGTTGGGTATTCTCTTAGGGGCGGGGGCACCTAGCTCTTATCCAGCACTGCACAAATCATGTTGTATCCGCTAGTGCACATGCTCGGCGAGGTAGTCATCTTTTATCAGTCTCTAACTACCCCGATCCTTGTTGATAGTTGCGTGCGTTATCCTTGGTGCCCGCACTCTTATCTCGGGTTTGGCCGTTATTCGCATCTACCGTAGGTTGGGATTACCCGTCAACAGCGATAAGCAGAACTCCCGCATCGTTCAATGGTGCCCGGCTATGGAGCAAATCGAGAATGTCGGCGCCCCGCCCCTGGATTGTTGCGATTGCCGAATATGGAAGATTCGTACCAACATTAGGTCGTGCAGTGCCTTCACAGCGTCCATAGCCCGCGTTATTCGTCCAGAGTCCGGTGCAGCTACTGCTTCAAAAATAAGGACCCGACTCACGATAGCTTGCTCTATACCCGGACGGTTTCGTCTCTACAGCCTGCCCGTGTCCAGTTACGGGTCAAGATAATGCCCCCCCAAGACCATAAGCAGCTTGTCGCTGCAGTCACCGTGCCCGTGCTCAACATTTTTCTAGCTGTGTTAGAGCCCGGTGTCGTTATGGATGCGTGACCAAAAGTGCAACGTAAGTTTCTTGTGTTTCAGGACCAAAGCTCTTAACGAGATAACAGGCCGAGCTCTGGGCCATTAATGCTTCTCGGTTCTCTAGATACAGGGTGGCACACCAATAGAGTCCCACTATGGTTATAGCGTATATCTGAGTTAAGGGGATGCTTGGGACTACTCTCGCTAGAATTTGATGAATTTCCGTCTGTAATGTACTGTCTTGATGGCCAGTTACCTGCAGAGAGTTTCACGTTTGCGGCGATGCTGGAATCGTTCGTGACGGGGTTTCACTATGAATGCGACGGCCTCTAAAAAGGAGGAAAACGCCGGAAGCAGGAAGAGGGAAGTCAGCAGCTGGGTCTTGAAGCACTTTGAGCGCCGTTTAGTTCCAAAAGATACTACCTCCCACGCCCTGAAAACCCGCTGAAGTTTACGCTGTGAGCCTCTGTTATCAGTGTATGATTTTTGGGGCTAGGCATAGGGATGCTAACTTGGCCCCCCGACGCACACGACAGTATCCTATGCAGATACCTTGTAGACATGCCTAACCTGTCCACTTGACAGTCAATTGTTATACCCTTTGAGCGGTACAGACCGAGCTCACTCACGAGTCTACTACCCGTGACCCTGCAATCGGTGTTTCCACCTAGACAACCGCACGCATCAAGTGGCTAGTTAAAATGAACTCTTCCCTGGAGTCGGGTGTATCAACCTAAAGAAACCCTACGCTATCTCGGTTGGGTGGATACCGTAAACTGATACCATGTACTGTTCGTTGCAAAAGGGAATCGACGCACGCATCGGTAAGATCCTACTTCTGGCTTTCGTTATCAATAGAGGCGTTATTCAGAAGAGACTCCTGTAAAACAGCGGGTGTCCATAACCGCAAGTGCTGAGTCCGATATTACGGCTACCCTCTGGCTGTTTAAGGCGATCGACCACACACTCGGCCTCGGTAAAATATATTTGTCTACTGTCCACCCATACTGCGCCGCCTACAAATAACGGAGCGCAAAAAGTACGCCGTAACGGATCCATAATTATCATTACGTTTACCTATGGTAGTAGGCTGAGTTCCACACCACCGATTGTGCGGATGCGAAATCCAGCCATCATATGCAATGCAACGCTTCCGACAAGACTGACTTCAATAGGATGCCTATTGAGAGCAGTTGTATATCAGTACGCAGACTCGGGTAGAGGATGGATGCGCCGCTACTGATGAGGCATGTATAGCGCTACATTTGATACTGTGAAAGGGTCGACGATCAGGGCTAGCGGAGCCGCACATTTCCAGATACAACGCGCCGACGGTGAGCACCTACGTGATCAATAGACAGTACCCTGTAATATTTCACCTTAGTAGTTCCCTGACAATCCGTGCAGGACCGCAGGGAATTAGACTATGCGCGCGCAAGTCTACTAATATCACATCTGACTGTCGGGTACTGTAATTTTTAGCTCCAGGGATTAGGTAGACCGTATTGACCAATGTAAAGTTGTCTCGATACAAAGCCTTTCGCATAGATGCATAAGAGGCCGTTGCAAGCATCAACAAGGCACTTAATTGTTCAAGACTGTCCAGATGAACACCGCTAGCGTTAGCGGAGAAAGGTATTGGCCATGGTGTTTTAATTCGCCGGTGGTCTGTGTGGGTTGTGTTGGGCTCACGACGACGCCTCGGACCCCGCCGCACTGTGGAGAATGATCCATGTAAGTACGCGACCTTTAGAATGTGGTCTTCCCTACCTCGACCGTTATCGAGTCGACCCGCCAACCAAAAGCCCGACTGTATTCGGCATGGCTAACTTTGCTTAGGCTCCTGTATTCTGATGTAATTGTTTCTCTCTGACTCTACAGACCTTTTGGACTTGCTAGACGGCGAATAAAGGCTGCGCAATCGCATCCCATTCGTGGATTTGACAATCCTCACCTACAAACAGAACGAATAGCGGAGTGTGTAAATTGCACCAATCTTCACCGTCACGGTATCTAAGCCGGATTGACGACCTCAGCAGAACCCTAGCTAGTCAAGTCGTAGATACAGAGTGGTTAACGCATCTGTAAGCGTGTCGTATTCAAAGGGCGCAGGAAGCGGTGGGTACGTACGCCGCGCCAGCTGACCTAGCTTAGTAACTCTGCTCCGTCAGAGCGGACCAGCTGACGTCTAGGTTAACGAAGTATGGAGGCCCGTGCTAAATACTCAGTGATTGCTTTATTTGGACTCAGAATAGTAATTAGTTCGCATCCTTCCCTACTCCCAGGTGCGTCCCACTGAACCGCCGAGCGCCTCAACTCTCACAGGTCCACCCTGGTCAGGCACCGCTCCGGCCAATGCTTACGATCGAAGTCGTGGCAGTAAAAGCACAGTAACGCGACGTTATCTGCGTCTTGGATGCAAGGCTGTCGGAACTATACGGCGGACGCAACACATATTGAACACCGGAGAGTGTCCACCCGAGCATTTAGGTGTCATTGTCCATGTAGGCTCCCCTCTCGTGATAGCTCTCCAAGGATTTGCGGTGTTGCTTTACTCACCACTACTTTATCCGTAGGCAAGGCCGTTCTGGTTTCTCTATTTTTACCTATCTACGAAGAATGAATTAACTAGTGGTCGCTGCTATCTACTGTTAATCTGACATGAAAAGCGTCCTCGGACGTGCTGACATTCCAAGAGCCAGCTAGTTCTCATAGATTAATGCAAACGATGTCATTTAAGTACTGAATCTGCTGCATCCCTTAAGTCAACGGCATTCACCATGTCCACTTCTATCACGGAGATTCACATTGAGCAGAGACAGTGATACCCACTGAAACGGTTAAGCAAATGAGGTCCCTCTGAAAGATTTATTTGATGTGACGTTCATTCGTGCAGCGAAGTCTACCCCTGTATTCGAATCGGGACCTCTCGGATCTGGACAGAAAGCAAGCCCACAGAATTTCTTAGTTAGGCGCCATGGCTATCCTCATTGCCTACGTGACGGCTCGGTAGCCTCCGTAACTACGGTCTGTTCGACCCCTTCCCACACCCAGACGATTAGGCTTCGATGACGTGATCGTCCATTGCATACAGCTCCATCCTTTCCCTTGTCCGACTCCGTAGGTCCTTGATTCGGGGCCTGCTCCCGGCATCAATTGGCGTTAGTGAGTTGTGGCTCTCAGACTATGCGGCTGTGGAAGTTTCTTTTCTTGCTCGCGAGGCGGGCACTATGTTCGGATATGCTGTACTATATAGAGTTAAGGTCGCGTGTGAATCCCAAGTAACATGCGGTCTCACGGGGGGGCGCTTGGCCGGCTGGTGAGCTTGCTGTTGGGGCGCGGTTGTGCAACCATAGGGGTACTGCAACAGTGCGAAAAGCATTCGCTATCCTCCATTGGTCGGACCTATACAGAGATAGTCTAAGCCCCCAGGGCGTCAGCACAGGTCTTCAGAAATCTGCGGCACATGTTGATTGATCGGGTATTACCCATGCGGGCGTTTTGGACCAGTAGGCGGTAATCGTAGTCAGCTGCTGATTCTTAAGGCACTACCATCGTCGAAAACCAGGATCGGGGGCCCGAACAGCAAAGTGAAATACGCTCCTCTATGCATCCCCTAGGTTCCCCCTGCTGGAACGGCGGACTTTTTAGCAAATGTGCACTCCACCGCGCGCGTATGTAAATACCACCACTCGTCAAATATGCAACACCGAACAAGATACGACGGAAGCTTTTCGCCCTTCTAGACGATGATATAGACCCAGATAGTCTCGGACACTATTGTGATCTGGCCTCCCACGTACGTACACGTTTGGATACAGGGAGGATAAAGAGGATATACGATACAACGTACTTTACTGGTGGACTCACGATGCTACCAAACGGTAGACAGCAGTATTTAAGTCATCAACGTCGTTCCTAGACTCCACAATCTATTGGAGGTATTTTGGTTACTGCGCTCCTCAACTGTATCTTTCGGCCCATTCATATCGGATAGCACCGTGTCGAAACGCTACGCTTATTGTGATAGTACACATGTCCCACCCTGGCCATATACGGATAGGGTGCGTCAGAGAACGTTGGTGGCGAGCTTGACGAGTTCAGCCGCTGTCTTTCGGAGAGATGTGTCGCCTGATTCAACAAC")
      val k = 100
      val sf = Source.fromFile("src/main/resources/compositionKmersInteractiveQuizSolution.txt")
      val st = sf.getLines().mkString("\n")
      val expectedResult = DNAMotif.from(st).get.value
      compositionKmers(text, k) shouldBe expectedResult
    }
  }

  feature("stringSpelledByGenomePath") {
    scenario("example") {
      val path = DNAMotif("ACCGA\nCCGAA\nCGAAG\nGAAGC\nAAGCT")
      stringSpelledByGenomePath(path) shouldBe DNAString("ACCGAAGCT")
    }

    scenario("extra dataset") {
      val path = DNAMotif.from(Source.fromFile("src/main/resources/stringSpelledByGenomePathExtraDatasetInput.txt").getLines().mkString("\n")).get
      stringSpelledByGenomePath(path) shouldBe DNAString("AAGCTCGACAAGACGCACCACCGATTATTTGGTAAAACCCCGCTTGGTCACCAGGAGTGTTGTGGTCAAACTTTAACTGAGCCGAACGTCTGGGACGGAGGGACCCTACGCTTGAAACACCCTGCGGGGAGAAATAATGTTTCGAGTCTAGAGGTTCACAACCATAAAGGTAATTGTCACCTGAGATCGTTAAGCCGACTTATTAACTAAAGGACTATCCCCACGAAAAGAGGACAGATCTACACTATTCCTACCGCTGATACGTTGGCTCTACGATCATATATTATATGGAGCCGTCCGACGTCACTTCAAACCGGTTTAAATCGAACAACGGTCCATTCCCCGACTAGGGCGGCCTATGGAGCGACGGATAGACTATCGCCGCGCGGGACTAACCCGCCGTGAGTCCCGGCTAAGAACTCCATCCTTCTGATGACACTCTACCATAGCTGAACCGTGACTATCAAACTAGTTGACTCGTGGCAGGATTCACGATTGACTTGACTTACCCCACCGAACGCAGTCCGGATAATGTCGCCTAGAGGTAACTTGTACGAGCCATCTTCCAGCGGCAGTCCGCAGATACTTTTGCATTGCCGCACCCGACGCGGAAATTTGTGGGAGGGCGGTGACCACTCGTACACGAGTCCATGCACCTCCGTTACCACAGGCCTAAATCGGACCCGCACAATCCCGGTGACTCGGATTTGCGTACAACCCGACCTCGAGCTGATTAATGGGCGGTTATATGGTACTCCCGTAGATAGTGGACCTCCAGCCCAGGTGGAGGGCTAACCCCCGTATGGCAGCACTAAACATCAGTCCGATCCGCATGAGCAGTAGATGGTGTAAATTTCTAGGAACGCCGTATGGTAACGCCTCGGCGTGAAGGGGGCATACAGATGAGATACATCGACTCACGTAGATTCATCCTACTTAGAATGAAGGCGGTTATGGTTATTTTGTGAAGATTAGTGCGAATGCACTGCCCTTGTAGGGCTGGCTGTCATTTCCGTGGCGTAGATTGTAGCACAGTTTTGGTTGCAATGATCACTTGCCCCGATCCTCACTACCGGCGACCAAAGGGATCAAAACGAGCTCGATGTCTGTGCTATGATTAGTCCTCAACATCGAGTAGGTTCACAAGGATTGCTCATAGACAGGGCCCATTGGTACTTTGATAAATTGTATCCAAGCGGGGGCCTAAGTTGTATTAACTGATTGCGATAAGTATTGAGCTCTGATCGGTGAAATGACTCGGAGCGCATCTGCTCTTGAGTACGACCGCTGAGATCATTAGGTTGACCTCGATTAAGGCGTATTGTCTAAAACACTGTCCGGGGGTGGCCTGCGAGAAAATTCACTATAAGGCGAATGCAGTCCACGAGTTGTAAAGGGAGGAAGCTCAAACTCCCCCCTAGGCTTGAAGCGTTCTCACGAGCACCGATGCCATCGTTAAACGTCAGTTATTACGGCACACCAACGGCATTTTCTCCCCCCTTTCGCCCTCGACCATTTAGTATATGGACATAATCATGATGAAGAATTTCAACTATCTTAGTGACGAGCTGTATGATTAGAAATCACAGTAGTGAAGTCACAACTACCCGCACACTTGGATGTTCAATGCCGTGTTGCACAACCCAACAGTGGTACATTCTCGCACTGCAAGGTGGGATATCAGGGCCTTAGTGCCCTTGGGTAGGCAACTGTGTGGTGACTACTGCATCCAAGCTAGGTGTTGCTAAACCGGATACGTACCTGGATGGGGACAAACTTGGCGTCTCCGAGCAGTGCTAAAGGGATACATTACCACATGCACACCTCTCAATTCGGTCAGGGGACTCGATGCCTCAATGGGTAGCGAGGATGGGTAGACACAAAAATGAAATCACCCACGTCGCCTTTAAGGGACAGTTATTGCGTGTCCTACTTTAATGCTAAGCGTTGGGTTTAGAGGTCATCTCACGATTTACGTCAAGTCGGCGTAGAACTGCCCCAAATTCCATACGTTATTCGAATGGATTGTCGTAACCGCGGCTCCGGGCGAATAGAGATATGAACAGTATGCTGCTTAAGGGCTTCGGCTCTCTGCGTTCACATGATACATTAATCAGTAAAAAGTTGGGTCATGACGAATATCCCGCTAGGTTAAGTTTGCCCATCCGCGGCGTCTCGGACAAATCCGCCAAAGACTCGTGCAGTCAGAAAAATCTTAACCCAAGTCCAAGATGGGGATCTCGCCCAAAATGGTGAGGCCGCGTGCGGACGCGACTTTCGGCCTATTTGCGGATATCATCAGGACCTGCGTATCTGTCGCGAGCAATAGAACTACTGTCCCTTCGGACTGGAAGAGCCCCGCGTGAACGCAGATCCATAGCAGGCGGTGGTTGAAACGTATGTATGTGCGCTCATCATTTCGTGGCTAGCGCCCAAATGCTCGAGCGCCGGTCAGGTGCAGGGTGACACGACACACGGCAATCCTGCAATTCAGTGGATAGCTACAACACTTGCGATCGAGGTTGGCCTGCCTCGAGCACTGAGACAGTGCCTACCGCCGTTAAGACTCACAGGGCAGAAATCTTGGTTTCAGTAATAGTAGTTCTCGGAACCATCGCGAGACAGTCGATGCAGACGACTTAACAGTGCATGTCGCGTGGGGAGCCGCTGAACTTTTTCAGTAACGATTGACTATAAAAGCCTACATCCATTCACGGAACCCACCCTAGGACCCTGAAGCCGCCCGTGTGCACCGGGGTGTTCTAAAGAATCCCTCCTTACCATTTCGGTTCGTCGGTAGAACCTCAGAGATGTACAAATGAACAACTAAAGCTAGTGCGGGACTCCCGGAGCCCAGAAGGCGTGCCCGTCAAAGTGACGATCAAGCTCAAGTCACGGATAAGCACGTCGTGTCACGAGTGAGCCTGGCAATCAGGGGGTCTACTAGTAACCCGTCTGACTAGCTATATAGGTCTATCGCCTGTTTCATCCGCATAACATACGAGGAAAACACTGCTAGCATGGTGCGCTGAAGGGTTTGGTGCCAACCTCCGCAAGGTCAATCTGCATCTTTCTGCACTGCATAACCCGCCTTATCTTAAGGGGGTTGCCTCAGAATCGTGCTATAAACATGTGCAATATTGATGGCTCCTTAGCGCCCATTGCCAAAAAATATTTAGGCGAAGATTGTGGTCATTGAGCGTCTAATAATAGCATGAATGCCAGTGGTACCATTCTGTCTAGACAATATGTTGTACGCTGCTGCCGACGACTGTTAATCCGTATCTTTTCGAGAACAACGCTTCTAAAGAGCTTGGCCTGCCTGTCCTGACCGACACATTAGCACTATAGTTGACTCGGATGCGCTCATACCTGTGCAGAGGTAAACCCCTTGGAGACAGGTCCTAAGCATTCCGTGCTAACCATAGCTAATATTTTAGCCCCTTGGCTGTAAAGAATAGGATTGGGGTGTCGGTAATAGGCTAATCTTGTATCCCGCGGTCCAGGTGCGGCCTGGGAGGCAGCACGTACATTAATCGAGCTATGGAGATAATTGTTAGTACCTGTGGAACTGTTTGAGTGCAATATGGCTCCCTTGCCGTGTGGGGGCTGAGATAAGTTCGTGAACAGAGAAGCTGATGTCGATAATAGATATTGTTACAAGAGACTTTAGTTTTATAACAAACTCAGATGACCGAGAAGTGTACCGTTGTTACACTCTATGGATCGGCATTAGGCTACGCGCACGTCCCCCCACGGCTTGCGCAGTACCCTCCGATCTCTATATACACTAGCTGTCGATGCGCGAATCTGTTGCCGTCATCTTCTTGCGTTATTTCCCGTGTCTAGGTACTCGGGGTGAAACATTACGGCTTAAATAGACGTATCCAATGAAAAGATTGCACTCGCGAAGTGACTCGACCAGCTATGTCGCCCTCCTATCTGACTCCACATCCGACGTTCGTTTAAGGCCTCGCGGTACAACTGTAGTCGTGACCGTGCACATCACTGATGAGGTTCATTCATAGAATTGTATGAAAGCACGAAACAATAATTTTCCTGTGGAGCTTTGGGTATTTGGACTCGAGGGCGCGAGGTCGTGTCCCTGGCAGTAACAAAACAAGTCTCCTGACAACATTCCCTAGAGGTACGAAATGTCCTACCGACCCCTCAATAGTTCGAGATTGAGTCTGACTAACTAGAACCCGCCATACTATATGGTCGTGCTAGATATATTAGCAGAGTTTCTTCCTGTCAGACTTCTATACTGTCGCACATACTTTCTTTGAAAACGTGCATATTGAGTGTCCAACACATGAACACAAGACGATTAGCGCCTACTTTCAAACCTTCCCTTCGGACCACGATTGAGTTGTGACTATAAGGTAGCCTGGATACACGCAATTAGGTACAGATACTCTACCACTGTTTTGTTTGTTGCTGGACCAAAAAGCGGCTCTCTGACCAATCTCATTAGTCTGGTTGTAGCTCAGATACACCTAAAAAAGCACCACGTGAGTTACAATTTGGTATATCCGTCCGTTCTAACAATCTGTTAACTCGCCGCTAATCTTTAAAACCTCCATACGGTGCATTGTCTCGTCAACTAATCTGCGCACATTAGACCAGTGATCTATCGCATCAATTATAAATCATTTCGGATAGAAGTATAGTGAAAGTTCACCAATAAGGTTGACCGCGTGTCGTCTGACGTAGAAGGTATCCAGGCGCCGAGGATACTAAGATAACACTTGAGAGTCATGTATCAGCGTGTTGAAGATTCGACGCGTTTGTCTTAGCTAGGCGGCTGCAGAGAGCACTACCACGCATACGAAGGTGTTTATTCGGAATTCTGCAACTAGAGTGCAGGTCATCAGCACGAATCGATCTTTTACGTTCAACATTCCGAAAGCGGAACGCCAACTGTCATAGTCCTGACTTAGTAGAGGCCTGAACAACTCCTCAATAAGGATAGACACGCAAGCGGAGATGCACGTCAG")
    }

    scenario("interactive quiz") {
      val path = DNAMotif.from(Source.fromFile("src/main/resources/stringSpelledByGenomePathInterativeQuizInput.txt").getLines().mkString("\n")).get
      stringSpelledByGenomePath(path) shouldBe DNAString("GGTGTACAACACGACCCTCTAGGTGTGTCTGATCGCCTGAAGACCTGCCTCCACGTCGTTATCTCAGGACCACTGTCCGCGGACCGTGATCCGTTGCGGCTATGAACTTGGTGTAAATAAAGCGTTCGCGAGCGTAACATATAAAAGTACAAGCGTTTCTGCGCCGGACTGGGGGTACTCGGTGGGTCGTGACGTTTAGGAGAGCCAGTGATCCTACCAGATTCACGAACCTTCGAAGATAATACCTCGATGGGGACCCCTTATGCGTTCTATTACCCGCCCCACAGTCATCGGGTCCGTCCCAACATGGAGGAAGCCGGGCCTATGGTCATTTCACGCACTCAGGTATACTCAGGGATCGGAGAACCCCACGCGTCGCCCACCAGCACGAACTTGTTCTGAGCGCTTCAGGTCCTCCTGGGATCTCTCTTTTTCGCACGCGAAGTACTGTCTGTCATTTGGTAACCATTCTTCGGAGCGAGCATTGTCCCTTTGCCGAGAAGCGTCGGAATTGTTAGGCCGATTCCCACACACGCTAATAGGATGTCATTTGGTAGTCGGACGCAAGCCCTCGTCTATAATGATCTTCTAACTGAGTCGGATGTGCGTTAAGTTTGTAGTTGACACCGTGGACGAACCGCATCAAAAGATGAATATTGGTTGACGGATGTTTATGGATGTATAAAGAACAAGTGCGAAGCAACCAATAACTTCCCACGAGGACAAGACAACTCGGTGTGAGTCTAGGGACGGCCCCGCGAAGAGTTTTGCTTGTTACATCGGGGCTATTAGATGCTGCGTAACCGCTTACCTGAAGTGGGTAGCATGACTCGTTGTTTTCTTGCGTAGGATTCGGCCGTTGACGATTCTACGGGTTACTATAATCCACATTTTGATACCTCATGGAACCGCATACATAAACGTAAATCCTGTCCGTCGGGAAATTCGAATCGCCTAGATGGGGCCTGCGCGAGTGAATACCATAAAAGTGTGCTTGCTATTGTAATAAACCTATCTTCGCAAGGCTGTCACGCACCTCTGTTCATTCGTGAACACCCGCTTCGGCGAACCAATCTTGGTTTCTATGGAAAACCAAGCACTTAGAAGGTGGCCTCTCCTCTACAGGATGCTGATCTTTAGGATCTGGACCCGTCCCCCGCATCACTGAGGACATCTTAGACTGTCATAAAGAGGGTTTAACGTGTGTCGCACAACTTTATCACGGCCCAGTGTAAGAGCGTAGTGGCATAGACATCTCCGTGAATCGCTCAGTCGCCTTGCATAGTGCCTGAAAAACCTAAAGCTGGAACTGCTTAAATATTGGAACAATACAACGATTTTTTGCCTAACCCAGCGTGCAATCCACCTACTTACGAAGTGCGCAGTACGCCGAACTGTGCATTTCGCCGACGGGCGTACTTACCGCTACGCTGTCCGCCTGATGTATCGTAACATATCACTAGCCTATATTGCACCCCCTCTTAAGAAAATAAGCAACATACGTGCGGTCGATCAGGAATCCTAAAGATGGGTTGTCCGCGGCAAAGGTCTGCTTCCCACACCCGGCTTTGGGGCTTCTTGCCACAGAAGCGCAGACTGGTGTGAAGATTAATTGATTTCCTCCTCATCTCGTCTCCGATATGGTTGGTCAGTGCACCAGAACTCTGAAAGCGGGGACGGAGCTCACGATCTTGCCGATATGCACTAAACCCACGCAAAGTCATTCCAATATGCCCGTGCGGTATACCCATTCTGCTTACTGATCCTGCGGGTGCTGTACTTACACTGCGTGTTCCGTAGAGGGTGCAGCGTGTGGGCAGTTTGGCCCTACAGGTCTATTCTCATGAGTTTTCACCTACTTTTCAAATAGCATTCTCAATAATTAGCCAATAAAGCTCAACATATCTTAGCCAGTTAAGCCCACCGTGGTGCTATTTCTCAAAATCACATCTCTACTTATCTTATTTAATGCCTACCTTTGTACTCGGACGCCGCATTTCAGTTATTGGGCGCGAGCTTGGGAGATCAGAGACGGATAGCTCTTTTATTTAACCCAGTGCCGAATTGGGTGACTCTAGGTCCTGGCTAATTCCTATTGCGCTAACTTCGATCTATATTTTGCGACTCCAGTTAGGTGCTCGGGCTGTGGTCTCAACTCGGCACAGCTAATCAGAGGAGCCCTTAACTCTTGACTACGTCGCGTATATTATGAAAAATAACGGTCCAACAAAGGGTTACCCGGGGTTCCAATCTGCTTGAGGACTCTCCGAGCGCCCCAGCAGTGACCGTGACAGTCATGTTTGCTGTGGTAGAAGTCAGACCTCTTCAATGTTAGGCGGGGTAAGCCAGGGACTTATTGACAGGGTCAAGTCCTAACAGACCACACTTTACTATCCGAGGAGCATGAGATTTAAGGGAGTATATGGCGTACTGCCCCGCCACTTGATGTTCCCCAACTGGCGCACTTAAATCTTCGAAGAGAGTTATCACTACGATCATAAACGCTTAGTTGTTTCGATCTGTGGAAGTCCGCGCGAGCTCATGGGGTTGCTTCTGTACATGCTGGTGATCGACGCCATGGGTGTACAGAGATAACCTCCTCCAGCAGCTGGCACGCGATACTGGTTCGTTTTCTTCAAGGCTCCGGTCATAGTTATGATCATTTACGCGACTCCTCCATTCGCTAAATGTGTAGCTGGTGAAGAAGCTCGAAATGCGAGAGGGCGACACAACTAGTGAAAACAACGGCACGCGACAAGCGTTACGACAACTTAGTGCCGAGGTTGTTACCATTACCTTCGTGGCTTCCACAAAATTGGATCATAAAAAGCTTCTGGCAATGTCGTTGTACACTGTGGCTACTTGAGGCAAGAAGTCCGAACGGAGGCGTTCAATGTGGGTGTCCGCACAAGACACAGCTTGTCTTCAAGTACACCCGTTACCCGTTGTCTGATCATCAAGCAACTTACCAGAGGCAGTTTAGGTCTCGTCGACACGTGCTTAAGAAAAATGGGTCAGACGTCTATTGGGCAGATGAATCGATCCCCGTTGCGTGTGCGCACTGTACGCGTACTACTATCGTATCTTGATACTATAGTTATAAACAAAACGTGTGTTGGCCGGCCAGATAACCTCCGGCATGGTTCAGCCTTGTGCAGCACTACAAGACCAGCTAGTCGCTGCAACCTGTCAAGGGCGGGACCGCGGCTGATAGACCTCATACAAATTTCAGCTCTATCCCAAATCTCGCACACCTCTTCATTTCGATTGACTTGAAATTGGCCTATGGAACGACCGGAACCCCGCAAGCGATGGGAAACGTTTATCCGGGCGTAGGGTAACTAAGAGATTAATTGAAGAGCGGCCCGTCTCGCGTACATCGCGACCGTCAACGCCCTCTTATATCCGTCAAGTTTCGCTTCCCGATCTACACCATGAGCATTGTGGTGTACTGGGTTCGTTGAAAAGGCTAAACAGAAGTGAAGCATGCTGCAGGCCGCTCGACAGAGGGGAACCACGTGTTGTCAACTAGCTCGTGACATCGCTTTACTCGGGTGTCTACCACGCAGGGTTAGAGGGCGCCATGCCTCCGTCCAGTGTAGCTCGCGGTCAATCCATCCTTCAATGACGATGTGGGTAAAGGAGGCTTCAATACTCACATTTCGGTCGCGTACTCATGAGGGGTGCACCACGAAGTACTTCAACTATGTTGGGCTACACAACGACAGCGTGGGCGTCGAGGCTCTCGCTGGAGGCCTCCTGAACGGACCGAGACGCCCAATTCCATCGGGCTGGGTGGGAATTCATTCGATCTGAATACTCGTATTTTACTATTTTACACATCACTAAGGTTAGGGGCCACTTGGTCCACTTATTGGGCATACATGGGTGAGTACCTTACCCGGAGTGCTCTTCTGTGCCACTATTTACCATGACCTTAGCGCTGGGCGCATCCTAAACGAGCGTTATCTTCATTAACAATTGAGCTGAAACGAAGCAGTGGAATGGGATCACTAGTGCCCATCGAACAACGCAAAAAACATATCAGATCTTTAATGGGTGGTTGAGCTGCTGGCTGCTAGGGGATCTCGTGAGTACGGCATTCTAGCGCCATGACTTGCGATGTCACAAAAGTGGCGATCGGCTAGCTAGTTGAGTGTGTCAGCGCCGCAACAACTTTCGGATGTTGTTCTACTTGAAGGCAGGCCGTGCCCGATTAGCAATCCGCATTTAACTCACGATACTCCAATTGACGGCGCGAACTGCATAAAAATGTCGGTGGCGTGAAGTCACGCACTTGTGGATGACCCATACAAAGATGGTATGCTTCTGGGGCCCTTGCTACGCGGATAGGTGATTTAGATTGAAAGGATTTACGTGATATGACCTTCCCGCTTACGGTACTCGGGTGGGGTTTGGCCCGATGGTGACAACGCTTGACGTGTCCACCGTACCGATTACGACTGAAGGCAGCCATACCCACGGAGGGTTCGGTAGACAGTACCAGGTACTCCCAGAGACAGAGTTTAGGGGTTGCTTGGGAGAGAACTCCTATGTTCATGTATGGTAGTTGAGCCAATCAGTACTGTGGATTCGGCTTTAGTGCTTCCTGCAGAAGATCCATAATTAAGAACCACTCTTACTGCTGTCTAGTAATGTTGCTTACGACCCGAGCGTCCGTGGCTTATACAGTCTGTTCGTAGACACGATGACGCACCACGGTTGAATGGGGGTTCCGAGGTTAATGATAAGGTGCCAGATAGATGGTGAAAAGGACGTGTGTGTGTGGTTTGTCCCAGAAGTAACTTTCAGTCTAGGTGTTCACGTAACACGCTATAAAGAGGGGGAGGTCTTGCACGTCGATATTCCAATTGGCATGGACTTTGAATGATTGAGTGAAAGATGAAAAAAAGCCTAGGCATTAGGCTAGCCACTGTTTATAGCCCCCACTGTTCCATTGTGAAAATGGGAGCTAGGCGTCGGCACCACGCTGGAATAAG")
    }
  }

  feature("overlap") {
    scenario("example") {
      import weeks.DNAString.StringToDNAString
      val patterns = DNAMotif("ATGCG\nGCATG\nCATGC\nAGGCA\nGGCAT")
      val result: IndexedSeq[(DNAString, DNAString)] = IndexedSeq("AGGCA".toDNA -> "GGCAT".toDNA, "CATGC".toDNA -> "ATGCG".toDNA, "GCATG".toDNA -> "CATGC".toDNA, "GGCAT".toDNA -> "GCATG".toDNA)
      overlap(patterns) shouldBe result
    }

    val regex = """(?i)([ACGT]+) -> ([ACGT]+)""".r

    scenario("extra dataset") {
      import weeks.DNAString.StringToDNAString
      val patterns = DNAMotif.from(Source.fromFile("src/main/resources/overlapExtraDatasetInput.txt").getLines().mkString("\n")).get
      val expectedResult = Source.fromFile("src/main/resources/overlapExtraDatasetOutput.txt").getLines().map {
        case regex(left, right) ⇒
          left.toDNA -> right.toDNA
      }.toIndexedSeq
      overlap(patterns) shouldBe expectedResult
    }

    scenario("interactive quiz") {
      import weeks.DNAString.StringToDNAString
      val patterns = DNAMotif.from(Source.fromFile("src/main/resources/overlapInterativeQuizInput.txt").getLines().mkString("\n")).get
      val expectedResult = Source.fromFile("src/main/resources/overlapInterativeQuizOutput.txt").getLines().map {
        case regex(left, right) ⇒
          left.toDNA -> right.toDNA
      }.toIndexedSeq
      overlap(patterns) shouldBe expectedResult
    }
  }

  def loadGraph(fn: String): IndexedSeq[(DNAString, IndexedSeq[DNAString])] = {
    val regex = """(?i)([ACGT]+) -> ([ACGT,]+)""".r
    import weeks.DNAString.StringToDNAString
    Source.fromFile(fn).getLines().map {
      case regex(left, right) ⇒
        left.toDNA -> right.split(',').toIndexedSeq.map(_.toDNA)
    }.toIndexedSeq
  }

  feature("deBruijn") {
    scenario("example") {
      import weeks.DNAString.StringToDNAString
      val text = DNAString("AAGATTCTCTAAGA")
      val k = 4
      deBruijn(text, k) shouldBe IndexedSeq(
        "AAG".toDNA -> IndexedSeq("AGA".toDNA, "AGA".toDNA),
        "AGA".toDNA -> IndexedSeq("GAT".toDNA),
        "ATT".toDNA -> IndexedSeq("TTC".toDNA),
        "CTA".toDNA -> IndexedSeq("TAA".toDNA),
        "CTC".toDNA -> IndexedSeq("TCT".toDNA),
        "GAT".toDNA -> IndexedSeq("ATT".toDNA),
        "TAA".toDNA -> IndexedSeq("AAG".toDNA),
        "TCT".toDNA -> IndexedSeq("CTA".toDNA, "CTC".toDNA),
        "TTC".toDNA -> IndexedSeq("TCT".toDNA)
      )
    }

    scenario("extra dataset") {
      val text = DNAString("CTGAAGACCTCTCCACATTACTACGATATAAATCATTTCAGCCTCTAGATACGCCTTGGTGGGTGGGGTTGGCAATTTACGATATGTCCGAATGATTTGACACCAAATACCTTAGCTAGCCCCAAGGAAAATTCTGGGCTTTACGTTGGCCGAGCCACATTACTACAGTAAGGTTAAGCAACCAGCCAGTCGCTCATAAGGACTCCACGCCTCCCGTTACTGACTTCCAACAACAATGTGACAGTAGACTGGAACCTGGGAGGACATTATTGATTCGCCGCGAATCTTCTAAGGTATTTTACCCCCACTGGTCACCTTAACCATTAAGACCTCGAAGTGACACCTAGCCTCTTAACACCCAACTCCACCGACAATACCTATTCGCTGACAAGCGGGACATCCGATCGCCCCTGACTCGAGGTGTCTACCGTCCATCGATTGCTAAACTTTGTTAGGAGTCTAAGCGAACCATGGGAAGGGGGCGGCAGTCAACGTGCTCCTTTAGTGAGGTACCATATTCTTACAGCATGTGGAGCGCAGCAAACTAGCGACCGGGAGTACTCCCACAACCCTGGGTACGTACTGCACTTTTTTCAAGAGCCAGGGTCATTTAAATAGCATCTTTGCTCTTTCTGATAAGGGGGCGACCATCTCCGAATTGAGCCAAACGCTGGTATAAGACTCGTCTCATGACTCCCTAGCCATTTGTATGTTGTCATTTCTGATTTTAGCAGGTAAAACGTAAGGCCTGCTAAAGAATCACGCGGGGAGGCCTTAAATTTCGTCATGGAGCAATCGTCCTAGATTGCTGTGAAGGTTCGTACCAGTAGAGTCTAATGTGCGTAAATGTTAACTGGCCGTATATTCTCTGGTGAGCTGAAACAGAAAGCTGGCAGAAAGCCACTCTTGCTGTTTCGTGTGTACGGACATCGGGATAGTACCAAAAAGCATGTTCTTCATCTGGCGATGCTTGATGTCTACCGTAGACACCTTCATACGT")
      val k = 12
      val expectedResult = loadGraph("src/main/resources/deBruijnExtraDatasetSolution.txt")
      deBruijn(text, k) shouldBe expectedResult
    }

    scenario("interactive quiz") {
      val text = DNAString("TAAGGTCCTCTCTAGATGTCGGAGGGCGTCTTTTTTGATGTAGGCGTTATTTGCCGCGCTAGCTACGGACTACGCGGAGATCGCACATTGCTCCGAACAGGTTTCCGGAGCCTTCTGGCGCAACTTATTCGATCGTGGGCTGAGGACACATGCTGTCGTTAGTGCTTCGGGCGCATGAAATGGCATCAAGAACGCGACATTCTCGGCGATGTCCGGGTGTTCCAGATCTAGGACGAGTATCCCCAGAGTACTGACAGTAGGTGGGGCCGCCTCCCATTCGTTGTTAATTCTATATGCGTGAGCGGGCACGGTCTGGTTTGAAGTTCAAACCTGTAATCTCGTAAAAGCTCTGAAGCACTCGGAACAGCCCACTCAGCTCCTGGATCATCAAACAATCCTTATATGTAGTGTTTGGGAGGATCTTACGAATTTCCGCACAAATCTCATGCACAAATCCGAGTCTGAGCTACTGTAATTTCACTCTTGCGGGAAGCTTAGACCAAAAAGCCGACGCATTTGCGGCAATGAAGATACGTCCCATTCATACTGGATCTCGCATCCCCAATCTAGACCACAGCTCGATCGAACTAACTTCGGGACAGGGCCCTGGTAAATTAGCCGTCTTTAATCACCCAGGTATCCCTCTAACGTACATTGTCTGCACACGATAAATGAGGGAGCCGAACGAAGGAGGCGAACAACGCGACAGGTTTCCCAGTTAAGAAGAGGGCCGTGCGGTGAAGCTTGCAGATACGATTCCTGCTGAGCACTCCCAGAAAACTCGTACCTCCCCCTAACGGCTCCCGGACGAGGGCTCACCACCACTGATGTAAACAGCCGGTACAAGGTAACTTGCATAAGCCGCAACTACCTACTAATAAACGGAAATGGCGCGCCAAACTCAGGTTGACACTAAGAAAGGGAAAAACTATCTTGTAAGGAAGGATAATGTAAATTCATGGGAAGTTGGATTGGTGGTCATCTTGTGCGCAAGAGCGGGGCCATCGTTAGTCTCCTAGAAGGCACGATCCGAACAACTGTTTGATATCACAGTTGTATTGGCCACGCCGCGCCTGTCGGCGACACGGGCATAACGCTCTCAGGCCACGCTACGGGTAGTCCTTTCGAAGTAGGTTGGTAACAAACACAGACTGATTGAGTCAGCCCCCAGGTCTACCATAGTTACCGGTCACGGTCGCCAGAGTCGGTGGGGCAGCTCTTCTATTGCATAGCCCTTCGCCGCAAACGACTTGTGCAGCGGGTTGACATCGTGTGGGACGCTCAGGCACTACGTTCGGCTATACCCGTGAGCGGTTCCTTTCATCCTCTATCGATGATCCGGGTAATCTTCCGATTCGGAATAGTTACGCTGCAGGGCGCGACAGAAAGATCTTATACCTTCATACCTGACCGCTCCGAAGTACCTCCCCCTAGACGAAATTCTTGTAATTTACGCCAGTTCTACGTGGCCCGATGACCAGGCCTCGGTTTTAGTCATGGTCTAGGGCCCGACTGAACGGGACGGTTGCCAGGTGCCTATTGTCATCCTACGCCCCAGCGTCAGGTTTCTACCTTTCTTTATCCCAGCACTGTCCATTACGTTTGAAACATATGATCCATCCCCTTGTTACACGGCGGAAGGTGCAGGAGTTTTATGTGACTTTTTACAGTAGGGGTAGAAGTCTCGGTCTTACAGTTAATCCGCGGCTCTTGTACAGACGGCTGGAGAGTGTCAGAATCTCACGCTAAGCTGGTCCCCCGGTGCACAGGTCGCAATATCGAGGTGGAAACCCCTTAGGTTGAACCGCCATGGATGGCACATTAGACCGCAACCTAACCTATCGGGGTTAAGTCCACTGATGCTCCCGAGTTAGATACCTAGCGATCTTCAGAGAGCTTTCTTCGGATACATTCGAAGGTTATGAGCACGGTGGCATCAAGGTCCGAACCATCGTATGTGAAGTGGTCCTGTGGACCTCTCCATCGAACTAGTTTAGCGG")
      val k = 12
      val expectedResult = loadGraph("src/main/resources/deBruijnInteractiveQuizSolution.txt")
//      deBruijn(text, k) foreach { case (k, v) =>
//          println(k + " -> " + v.mkString(","))
//      }
      val result = deBruijn(text, k)
      println(result.length)
      result shouldBe expectedResult
    }
  }

  feature("deBruijnFromKmers") {
    scenario("example") {
      import weeks.DNAString.StringToDNAString
      val kMers = DNAMotif("GAGG\nCAGG\nGGGG\nGGGA\nCAGG\nAGGG\nGGAG")
      deBruijnFromKmers(kMers) shouldBe IndexedSeq(
        "AGG".toDNA -> IndexedSeq("GGG".toDNA),
        "CAG".toDNA -> IndexedSeq("AGG".toDNA, "AGG".toDNA),
        "GAG".toDNA -> IndexedSeq("AGG".toDNA),
        "GGA".toDNA -> IndexedSeq("GAG".toDNA),
        "GGG".toDNA -> IndexedSeq("GGA".toDNA, "GGG".toDNA)
      )
    }

    scenario("extra dataset") {
      val kMers = DNAMotif.from(Source.fromFile("src/main/resources/deBruijnFromKmersExtraDatasetInput.txt").getLines().mkString("\n")).get
      val expectedResult = loadGraph("src/main/resources/deBruijnFromKmersExtraDatasetSolution.txt")
      val result = deBruijnFromKmers(kMers)
      result shouldBe expectedResult
    }

    scenario("interactive quiz") {
      val kMers = DNAMotif.from(Source.fromFile("src/main/resources/deBruijnFromKmersInteractiveQuizInput.txt").getLines().mkString("\n")).get
      val expectedResult = loadGraph("src/main/resources/deBruijnFromKmersInteractiveQuizSolution.txt")
      val result = deBruijnFromKmers(kMers, sort = true)
      result shouldBe expectedResult
    }
  }
}
