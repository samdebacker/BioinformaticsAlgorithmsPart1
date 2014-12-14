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
}
