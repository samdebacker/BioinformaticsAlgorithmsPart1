package specs

import breeze.linalg.DenseMatrix
import org.scalatest.Matchers._
import org.scalatest._
import weeks.Week3._

class Week3Spec extends FeatureSpec {
  feature("expectedNumberOfOccurences") {
    scenario("interactive quiz") {
      expectedNumberOfOccurences(9, 1000, 500) shouldBe (1.8921 +- 0.0001)
    }
  }

  feature("motifEnumeration") {
    scenario("example") {
      val dna = Set("ATTTGGC", "TGCCTTA", "CGGTATC", "GAAAATT")
      val k = 3
      val d = 1
      motifEnumeration(dna, k, d) shouldBe Set("ATA", "ATT", "GTT", "TTT")
    }

    scenario("extra dataset") {
      val dna = Set("TCTGAGCTTGCGTTATTTTTAGACC", "GTTTGACGGGAACCCGACGCCTATA", "TTTTAGATTTCCTCAGTCCACTATA", "CTTACAATTTCGTTATTTATCTAAT", "CAGTAGGAATAGCCACTTTGTTGTA", "AAATCCATTAAGGAAAGACGACCGT")
      val k = 5
      val d = 2
      val expectedResult = "AAACT AAATC AACAC AACAT AACCT AACTA AACTC AACTG AACTT AAGAA AAGCT AAGGT AAGTC AATAC AATAT AATCC AATCT AATGC AATTC AATTG ACAAC ACACA ACACC ACACG ACACT ACAGA ACAGC ACATC ACATG ACCAT ACCCT ACCGT ACCTA ACCTC ACCTG ACCTT ACGAC ACGAG ACGAT ACGCT ACGGT ACGTC ACGTT ACTAA ACTAG ACTAT ACTCA ACTCC ACTCG ACTCT ACTGA ACTGC ACTGT ACTTA ACTTC ACTTT AGAAA AGAAC AGAAG AGAAT AGACA AGACT AGATA AGATC AGCAT AGCCA AGCGT AGCTA AGCTC AGCTG AGCTT AGGAT AGGTA AGGTC AGTAA AGTAC AGTAT AGTCC AGTCG AGTCT AGTGA AGTTG ATAAA ATAAC ATACA ATACC ATAGA ATATA ATATC ATATG ATATT ATCAG ATCCC ATCCG ATCCT ATCGA ATCGC ATCTA ATCTC ATCTG ATGAC ATGAT ATGCA ATGCC ATGGA ATGGC ATGTA ATGTC ATTAA ATTAC ATTAG ATTAT ATTCA ATTCC ATTCG ATTGA ATTGC ATTGG ATTGT ATTTA ATTTC ATTTG ATTTT CAAAG CAACC CAACT CAAGA CAAGC CAATA CAATT CACAC CACAG CACCT CACGT CACTA CACTT CAGAA CAGAC CAGAT CAGGT CAGTA CAGTC CATAA CATAC CATAG CATAT CATCC CATCT CATGA CATGT CATTA CATTG CATTT CCAAG CCATA CCATG CCATT CCCGT CCCTA CCCTT CCGAA CCGAC CCGAT CCGCT CCGGT CCGTA CCGTC CCGTG CCGTT CCTAC CCTAT CCTCA CCTCC CCTTA CCTTC CCTTG CCTTT CGAAA CGAAG CGACA CGACT CGAGT CGATA CGATG CGATT CGCAA CGCAT CGCCA CGCGA CGCTA CGCTC CGCTT CGGAC CGGAT CGGCA CGGTA CGGTC CGGTT CGTAA CGTAC CGTCA CGTCG CGTCT CGTTA CGTTT CTAAC CTAAG CTAAT CTACA CTACC CTACG CTACT CTAGA CTAGC CTAGG CTAGT CTATA CTATC CTATG CTATT CTCAT CTCCG CTCGT CTCTA CTCTT CTGAA CTGAG CTGCA CTGCC CTGTA CTGTT CTTAA CTTAC CTTAG CTTAT CTTCA CTTGA CTTTA CTTTC CTTTG CTTTT GAAAT GAACA GAACT GAAGT GAATG GAATT GACAC GACAT GACCA GACCT GACGT GACTT GAGAA GAGAT GAGCT GATAA GATAC GATAG GATAT GATCA GATCC GATCG GATCT GATGT GATTA GATTC GATTG GATTT GCAAT GCACT GCATC GCATT GCCAT GCCGT GCCTA GCCTT GCGAT GCGGT GCGTC GCGTT GCTAA GCTAC GCTAG GCTAT GCTGA GCTGT GCTTA GCTTT GGAAT GGACA GGATA GGATC GGATT GGCTA GGGAT GGTAC GGTAG GGTAT GGTCA GGTCG GGTTA GTAAA GTAAG GTACA GTACC GTACG GTAGA GTATA GTATC GTATG GTATT GTCAA GTCAG GTCCG GTCCT GTCGA GTCGC GTCGT GTCTA GTCTG GTGAA GTGAG GTGCA GTGCG GTTAA GTTAC GTTAG GTTAT GTTCA GTTCC GTTCG GTTGA GTTTA TAAAC TAAAG TAACA TAACC TAACT TAAGA TAAGC TAATA TAATC TACAC TACAG TACCC TACCG TACCT TACGA TACGC TACGT TACTA TACTC TACTG TAGAA TAGAC TAGAG TAGAT TAGCC TAGCG TAGGA TAGTC TATAA TATAC TATAT TATCA TATCC TATCG TATGA TATGC TATGG TATGT TATTA TATTG TCAAC TCAAT TCACC TCACG TCACT TCAGA TCATA TCATG TCCAA TCCAC TCCAG TCCAT TCCCA TCCCT TCCGA TCCGC TCCGT TCCTA TCCTG TCCTT TCGAA TCGAC TCGAT TCGCC TCGCT TCGGA TCGGC TCGGG TCGGT TCGTC TCTAC TCTAG TCTAT TCTCC TCTCT TCTGG TCTGT TCTTA TCTTT TGAAA TGAAC TGAAT TGACA TGACC TGACT TGAGA TGAGC TGAGT TGATA TGATC TGATG TGATT TGCAA TGCAC TGCAG TGCAT TGCCA TGCCG TGCCT TGCGA TGCGT TGCTT TGGAA TGGAT TGGTA TGTAA TGTAG TGTAT TGTCC TGTCG TGTGG TGTTA TTAAA TTAAC TTAAG TTAAT TTACA TTACC TTACG TTACT TTAGA TTAGC TTAGG TTAGT TTATA TTATC TTATG TTATT TTCAA TTCAC TTCAT TTCCA TTCCC TTCCT TTCGA TTCGG TTCGT TTCTA TTCTG TTGAA TTGAC TTGAG TTGAT TTGCA TTGCG TTGGA TTGGG TTGTG TTTAA TTTAC TTTAG TTTAT TTTCA TTTCC TTTCG TTTGA TTTGG TTTTA TTTTG".split(" ").toSet
      motifEnumeration(dna, k, d) shouldBe expectedResult
    }

    scenario("interactive quiz") {
      val dna =
        """GGGGGACACATGACAAGCGCAGGAA
          |CAGAGGAGGCCAAATTGAGACTAAG
          |TGAGAACGGAGTACTGGGGTCACAT
          |TGAAAGAATCTTATTTTCAAAGGGG
          |TGAAACCTAGATAACTCGACTTTGC
          |CGTAGTGATAAGCTATGAAGCCGTC""".stripMargin.split("\\s").toSet[DNA]
      val k = 5
      val d = 1
      val result = motifEnumeration(dna, k, d)
      //info("Result " + result.mkString(" "))
      val expectedResult = Set("TGAAA", "TGATA", "TGAGA", "TGACA", "GAGAA")
      result shouldBe expectedResult
    }
  }

  feature("count profile consensus entropy") {
    val NFkBmotifs =
      """TCGGGGgTTTtt
        |cCGGtGAcTTaC
        |aCGGGGATTTtC
        |TtGGGGAcTTtt
        |aaGGGGAcTTCC
        |TtGGGGAcTTCC
        |TCGGGGATTcat
        |TCGGGGATTcCt
        |TaGGGGAacTaC
        |TCGGGtATaaCC""".stripMargin.toUpperCase.split("\\s").toIndexedSeq
    val expectedResult: DenseMatrix[Double] = DenseMatrix(
      (2.0d,  2.0d,  0.0d,  0.0d,  0.0d,  0.0d,  9.0d,  1.0d,  1.0d,  1.0d,  3.0d,  0.0d),
      (1.0d,  6.0d,  0.0d,  0.0d,  0.0d,  0.0d,  0.0d,  4.0d,  1.0d,  2.0d,  4.0d,  6.0d),
      (0.0d,  0.0d, 10.0d, 10.0d,  9.0d,  9.0d,  1.0d,  0.0d,  0.0d,  0.0d,  0.0d,  0.0d),
      (7.0d,  2.0d,  0.0d,  0.0d,  1.0d,  1.0d,  0.0d,  5.0d,  8.0d,  7.0d,  3.0d,  4.0d)
    )

    scenario("count") {
      count(NFkBmotifs) shouldBe expectedResult
    }
    scenario("profile") {
      profile(NFkBmotifs) shouldBe (expectedResult :/ NFkBmotifs.size.toDouble)
    }
    scenario("consensus") {
      consensus(NFkBmotifs) shouldBe "TCGGGGATTTCC"
    }
    scenario("entropy") {
      entropy(NFkBmotifs) shouldBe (9.91629 +- 0.00001)
    }
  }
}