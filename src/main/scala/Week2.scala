import scala.annotation.tailrec
import scala.collection.mutable

object Week2
  extends App
{
  val geneticCode = Map(
    "AAA" -> "K",
    "AAC" -> "N",
    "AAG" -> "K",
    "AAU" -> "N",
    "ACA" -> "T",
    "ACC" -> "T",
    "ACG" -> "T",
    "ACU" -> "T",
    "AGA" -> "R",
    "AGC" -> "S",
    "AGG" -> "R",
    "AGU" -> "S",
    "AUA" -> "I",
    "AUC" -> "I",
    "AUG" -> "M",
    "AUU" -> "I",
    "CAA" -> "Q",
    "CAC" -> "H",
    "CAG" -> "Q",
    "CAU" -> "H",
    "CCA" -> "P",
    "CCC" -> "P",
    "CCG" -> "P",
    "CCU" -> "P",
    "CGA" -> "R",
    "CGC" -> "R",
    "CGG" -> "R",
    "CGU" -> "R",
    "CUA" -> "L",
    "CUC" -> "L",
    "CUG" -> "L",
    "CUU" -> "L",
    "GAA" -> "E",
    "GAC" -> "D",
    "GAG" -> "E",
    "GAU" -> "D",
    "GCA" -> "A",
    "GCC" -> "A",
    "GCG" -> "A",
    "GCU" -> "A",
    "GGA" -> "G",
    "GGC" -> "G",
    "GGG" -> "G",
    "GGU" -> "G",
    "GUA" -> "V",
    "GUC" -> "V",
    "GUG" -> "V",
    "GUU" -> "V",
    "UAA" -> "",
    "UAC" -> "Y",
    "UAG" -> "",
    "UAU" -> "Y",
    "UCA" -> "S",
    "UCC" -> "S",
    "UCG" -> "S",
    "UCU" -> "S",
    "UGA" -> "",
    "UGC" -> "C",
    "UGG" -> "W",
    "UGU" -> "C",
    "UUA" -> "L",
    "UUC" -> "F",
    "UUG" -> "L",
    "UUU" -> "F"
  )

  val integerMass = Map(
    'G' -> 57,
    'A' -> 71,
    'S' -> 87,
    'P' -> 97,
    'V' -> 99,
    'T' -> 101,
    'C' -> 103,
    'I' -> 113,
    'L' -> 113,
    'N' -> 114,
    'D' -> 115,
    'K' -> 128,
    'Q' -> 128,
    'E' -> 129,
    'M' -> 131,
    'H' -> 137,
    'F' -> 147,
    'R' -> 156,
    'Y' -> 163,
    'W' -> 186
  )

  val masses: Seq[Int] = integerMass.map(_._2).toSet.toSeq.sorted

  val reverseGenericCode = geneticCode.groupBy(_._2).filterKeys(!_.isEmpty).mapValues(_.keySet)

  def peptideFor(rnaPattern: String) = {
    @tailrec def peptideForImpl(rnaPattern: String, result: String): String = {
      if (rnaPattern.length < 3)
        result
      else
        peptideForImpl(rnaPattern.substring(3), result + geneticCode(rnaPattern.substring(0, 3)))
    }
    peptideForImpl(rnaPattern, "")
  }

  def fastPeptideFor(rnaPattern: String) = {
    var i = 0
    val N = rnaPattern.length - 3 + 1
    val r = new StringBuilder(rnaPattern.length / 3)
    while (i < N) {
      r.append(geneticCode(rnaPattern.substring(i, i + 3)))
      i += 3
    }
    r.toString()
  }

  def countPossiblePatternsTranslatingToPeptide(peptide: String) = {
    @tailrec def countPossiblePatternsTranslatingToPeptideImpl(peptide: String, result: Long): Long = {
      if (peptide.isEmpty) result
      else countPossiblePatternsTranslatingToPeptideImpl(peptide.substring(1), result * reverseGenericCode(peptide.substring(0, 1)).size)
    }
    countPossiblePatternsTranslatingToPeptideImpl(peptide, 1)
  }

  def dnaToRnaPatterns(dnaPattern: String): (String, String) = {
    def transcribe(dnaPattern: String): String = {
      def transcribe(c: Char): Char = if (c == 'T') 'U' else c
      @tailrec def transcribeImpl(dnaPattern: String, result: String): String =
        if (dnaPattern.isEmpty) result
        else transcribeImpl(dnaPattern.tail, result + transcribe(dnaPattern.head))
      transcribeImpl(dnaPattern, "")
    }
    (transcribe(dnaPattern), transcribe(Week1.reverseComplement(dnaPattern)))
  }

  def dnaEncodesPeptide(dnaPattern: String, peptide: String) = {
    val peptideLength = peptide.length * 3
    def transcribe(dnaPattern: String): String = {
      def transcribe(c: Char) = if (c == 'T') 'U' else c
      @tailrec def transcribeImpl(dnaPattern: String, result: String): String =
        if (dnaPattern.isEmpty) result
        else transcribeImpl(dnaPattern.tail, result + transcribe(dnaPattern.head))
      transcribeImpl(dnaPattern, "")
    }
    @tailrec def dnaEncodesPeptideImpl(dnaPattern: String, result: Seq[String], c: Int): Seq[String] = {
      if (dnaPattern.length < peptideLength) result
      else {
        if (c % 10000 == 0) println(c)
        val dnaPart = dnaPattern.substring(0, peptideLength)
        dnaEncodesPeptideImpl(dnaPattern.tail, if (peptide == fastPeptideFor(transcribe(dnaPart)) || peptide == fastPeptideFor(transcribe(Week1.reverseComplement(dnaPart)))) {
          println(dnaPart); result :+ dnaPart
        } else result, c + 1)
      }
    }
    dnaEncodesPeptideImpl(dnaPattern, Seq.empty[String], 0)
  }

  def fastDnaEncodesPeptide(dnaPattern: String, peptide: String): Seq[String] = {
    def transcribe(dnaPattern: String) = {
      var i = 0
      val N = dnaPattern.length
      val r = new StringBuilder(dnaPattern.length)
      while (i < N) {
        val c = dnaPattern.charAt(i)
        r.append(if (c == 'T') 'U' else c)
        i += 1
      }
      r.toString()
    }
    def reverseComplementTranscribe(dnaPattern: String): String = {
      var i = dnaPattern.length - 1
      val r = new StringBuilder(dnaPattern.length)
      while (i >= 0) {
        val c = dnaPattern.charAt(i) match {
          case 'A' => 'U'
          case 'T' => 'A'
          case 'G' => 'C'
          case 'C' => 'G'
        }
        r.append(c)
        i -= 1
      }
      r.toString()
    }
    var i = 0
    val L = 3 * peptide.length
    val N = dnaPattern.length - L + 1
    val ls = mutable.MutableList.empty[String]
    while (i < N) {
      if (i % 10000 == 0) println(i)
      val dnaPart = dnaPattern.substring(i, i + L)
      //      println(dnaPart + " " + fastPetideFor(transcribe(dnaPart) ))
      if (peptide == fastPeptideFor(transcribe(dnaPart)) || peptide == fastPeptideFor(reverseComplementTranscribe(dnaPart)))
        ls += dnaPart
      i += 1
    }
    ls.toSeq
  }

  def massOf(peptide: String): Int =
    peptide.map(integerMass(_)).reduce(_ + _)

  def theoreticalSpectrum(cyclicPeptide: String): Seq[(String, Int)] = {
    val maxLength = cyclicPeptide.length
    val helper = cyclicPeptide + cyclicPeptide
    var result = Seq(("", 0), (cyclicPeptide, massOf(cyclicPeptide)))
    var forLength = 1
    while (forLength < maxLength) {
      var i = 0
      while (i < maxLength) {
        val subPeptide = helper.substring(i, i + forLength)
        result = result :+(subPeptide, massOf(subPeptide))
        i += 1
      }
      forLength += 1
    }
    result.sortBy(_._2)
  }

  type Peptide = Seq[Int]
  type Spectrum = Seq[Int]

  def cyclicSpectrum(peptide: Peptide): Spectrum = {
    spectra(peptide)._1
  }

  def linearSpectrum(peptide: Peptide): Spectrum = {
    spectra(peptide)._2.sorted
  }

  /**
   * @return (cyclicSpectrum, linearSpectrum) with cyclicSpectrum sorted and linearSpectrum unsorted
   */
  private final def spectra(peptide: Peptide): (Spectrum, Spectrum) = {
    var peptideMass = 0
    val prefixMass = (0 +: peptide).map { e =>
      peptideMass += e
      peptideMass
    }.toArray
    var linearSpectrum = Seq(0)
    var cyclicSpectrum = Seq(0)
    val peptideLength = peptide.length
    for (i <- 0 until peptideLength) {
      for (j <- (i + 1) to peptideLength) {
        val currentVal = prefixMass(j) - prefixMass(i)
        linearSpectrum = currentVal +: linearSpectrum
        cyclicSpectrum = currentVal +: cyclicSpectrum
        if ((i > 0) && (j < peptideLength)) cyclicSpectrum = (peptideMass - currentVal) +: cyclicSpectrum
      }
    }
    (cyclicSpectrum.sorted, linearSpectrum)
  }

  private final def isConsistentWith(peptide: Peptide, spectrum: Spectrum): Boolean = {
    peptide.diff(spectrum).isEmpty
  }

  private final def expand(peptides: Seq[Peptide], massesToUse: Seq[Int] = masses): Seq[Peptide] = {
    for {
      peptide <- peptides
      mass <- massesToUse
    } yield mass +: peptide // (we prepend but in the end it doesn't matter if the peptide is constructed in reverse, since the reverse will also be in the result
  }

  /* uses massive memory, start sbt with:
      sbt -J-Xmx12G
     well not anymore since we now correctly check the LSP is consistent with the SP, and not my own peptide is consistent with elements in spectrum
   */
  def cyclopeptideSequencingBranchAndBound(spectrum: Spectrum): Set[Peptide] = {
    var results = Set.empty[Seq[Int]]
    val basicAminoAcidsInSpectrum: Seq[Int] = masses.intersect(spectrum)
    var peptides = Seq(Seq.empty[Int]) // DO NOT .par, due to some side-effect things go wrong
    //println("Peptides = " + peptides.size)
    while (peptides.nonEmpty) {
      peptides = expand(peptides, basicAminoAcidsInSpectrum)
      //println("Peptides expanded = " + peptides.size)

      peptides = peptides.filter { peptide =>
        val (cyclicSpectrumSorted, linearSpectrum) = spectra(peptide)
        if (cyclicSpectrumSorted == spectrum) {
          results += peptide
          //print(peptide.mkString("-") + " ")
          false
        } else isConsistentWith(linearSpectrum, spectrum)
      }
    }
    results
  }

  def peptideNumeric(peptide: String): Peptide = {
    peptide.map { c =>
      integerMass(c)
    }
  }

  def score(left: Spectrum, right: Spectrum): Int = {
    @tailrec def scoreImpl(left: Spectrum, right: Spectrum, result: Int): Int = {
      if (left.isEmpty || right.isEmpty) result
      else {
        val l = left.head
        val r = right.head
        if (l == r)
          scoreImpl(left.tail, right.tail, result + 1)
        else if (l < r)
          scoreImpl(left.tail, right, result)
        else // (l > r)
          scoreImpl(left, right.tail, result)
      }
    }
    scoreImpl(left,right,0)
  }

  def spectralConvolution(spectrum: Spectrum): Seq[Int] = {
    // TODO optimize now we just have Seq!
    var result = Seq.empty[Int]
    for (i <- 0 until spectrum.length) {
      for (j <- (i + 1) until spectrum.length) {
        val diff = spectrum(j) - spectrum(i)
        if (diff > 0) {
          result = result :+ diff
        }
      }
    }
    result.sorted

    /*
    var sortedSpectrum = spectrum.sorted
    var i = 0
    var result = Seq.empty[Int];
    while (i < spectrum.length) {
      var j = 0
      var current = Array.fill(spectrum.length - i - 1)(0)
      while (j < spectrum.length - i - 1) {
        current(j) = previous(j + 1) - previous(0)
        if (current(j) > 0) {
          //print(current(j) + " ")
          result = result :+ current(j)
        }
        //print(previous(j+1) + " - " + previous(0) + " = " + current(j) + ", ")
        j+= 1
      }
      previous = current
      //println
      i += 1
    }
    result.sorted
    */
  }

//  val pattern = "AUGAAGACAUCGUUGGGGACUCUCGUUUGGGGCGUAAGGAUAAGCCAACGUCUUAUGUCAAUCAGAUACGUUAUCGAGCACCUAGCCAAACGGAAGACCCUGGUGACACGAUUGUUAUCGCUAGAUCAGCUUAACGACGAGGAAGCUACGCGCUUGGAGGGUACACAGACCAACCCAGGGACAGACCUAACUAGAACUAUUCGCUUUGGAAAACGCUAUAACUCUGCGUCGGCACAUCAAGCGCUGGCCGAAACCUAUUGCAUAACUCACGGACUCCUCGCUCCUAUACAGCUUGCUACCGUGAGACACGGCACAGACCUUAUUCUGGGUGGUCGCAACUGUAGAGCGAGAGGACGCCGCAGAGCCCCCUGUCAAGGGGAGCAAAGCGUGUCGGAAUUGAAUCCUAAACGGGGGCCUAGAGCGUCAAGAGGGAAAGCCUACGCAUCGCCGCAUCACCUACCAUUCUUGACAAGUUCGCCUUUAAUCCGUAAGCCUCCUCGUACAUAUUGCACACCGGUAACUGAUUUGAACUACAUCCGACCCGGAUGUUCGGGACAACUAUACAAGCGGUUGAGUUACCUAGGUAGCGUACUAGGCUCACUCAGUAAGAGCCGGCCACGGCCGAGUAUUCGCAUACUUGUUCUCUCCCUCCAUCUCAGUAUCUGCAUGCCGCGCGAUGCGACGGGGCGGUAUAAUCACACUGGGGGGAGGCCGUCACCUCUGUGCACGUUAGCUCGCAAUCGGCCUGCGGCGUAUCGAAUUGGUCGCGCGGCAGUCUUUUCGCCGGUGCCCUCACGUCGUUUCACCGGCCUAACAUCUUACUUCAGCCACAUAUUGGAGGGGAUGUUUCUCCCGAAUGCCCACCCUUGUAGUAAUAACUGGGUUGGACCGAAACGGACCUCGUGGUUCGACGUAUACUUUGCGGCUAUGAGUAUUCAACGUGGAAUCAGCGCCUCAUCUCUGCUUUUCGUGCAUCAGCGAAGCGCUGGUGUCGCUUGGAGGAAUCAUUAUGCAAGAGGAGGCACGAUUACAAAUGCUGAAGAUCAUAGAUCUCCUGGUCACCUAACGUCCUGGAGAAUCGUACAGUUUGUCCACCUGUGCAAGCAAGCGGGGAGACCUUCCUUCCAUCCAGCUGAGCGACACGGCAUCAAGCGCAAUAGAAGCAACGUGACGCGCGUUACCUCGACCAUCCCCAGUUUUAGUAACUUUGUCUGGUGCGGUAUAACCGUAGAGAAUGGAUCUAGCCUUGUAUCUGCAAUAGUAUCGCGAACGAUGCCACUCAGCGUUAGCGCCCUAGGGCCUAGCUCAACCGGAAGCUACCUACUUACAACAGUCAUUGUCUACUCUGGCAAGGCGGAUGUGGAGGGAAGAUUAUACGACGUGUCAAAUGUUUAUAAGAAACACGUCGUUGUGACCUUAGCCUCCGUUUUACAUGUAUUCCUCAUUUUGCAGAUUUUCCACUUCGCUAUACUGAGACUCGUGCAAGACAGGAAUUACCCCCCGUUCCGAUUUACAACUCGAAUAAAUGAUCCUAAUACGACGUUAGCGAUAUCGAGAUGUGUUAGAGCAGGGACAUCGCAUGGCAUCGGUAGCCAAAAGUUCGGUACUCAACGUCUUAAUACGUUCCUAGAAGGCUGUGAAAGUACCUACACCCACACUGACAUUGGUCACUCGGGCAGGGGUUUUGGAAAGGAGGUACAAAUGGAUGCACAGAGCGCUUCUCGACCCUUGCUUCCAAGGCCCACGUACGGUGCGGGUGCUUCAGUGAUAGCAGACUAUUCACCAGUCAACUCUGUGGUCCUUAAGUAUGGUACUGGAGAGCCUGGCAUGCUACGUCAACCGAUUCAGCUGCACGCCACUGCAUACGCUGACGCACAUAGAGCCUCUAUUACAAGUGUGAUGAACACAUAUUGCUGUAGCUUUAUGAUAUCCUUAAAAUGGUCUCUUGACUGUCGCGAUUUAAUACGCAUCGUGUUGUCCUUUCGGAAGAUCGUUCUCUGGUCCCGACUCUCGGCAGUCCUAGAGUUUGGAAGCAAGUUACGUGAGCCAUUGAUGCGUAGAUUACAAGGCCCGGCGCCUUUUUUCGGACAUUACUCGUAUGUCAGGGGAUAUUGCUACGAGCCAGACCACCAGAAAUCAAUUAUCUUCUUCCGUAACUACAUGGAUUCCCGGAAUAGGUUUGAGAACACAGCACGUCCACGUGCGUGGCAUGUUGGAGGGUCGCAUGUCGCCGUCGAGCCGUCAUUUGCUGUAUGUAAAGUGACACCACAGUGGUUCGUACGCCCCUAUCUGGCGGCGAAAUCUACAUUCAAGCAGACAACGUACUACGGGCGCGGGCAUUUUCCGUCAAGUCUAAACUUUCACCCAUUUUCUAACUUACCUCGGAGCAACGCGCCAGGGGUGGUCCCCAUAACUCCCGACGCGCGGGCGCCGGUCCUUGGGUUUCCUCCAUGCCGGCGGUCAAACCCAAGGCGUACUCGACUCUUGUGGGUGAUGGAGCUCGGUAAAUUCUUUACUCAUCUGAGCUUUCUUUAUGAACGAACCUGUUUAGCGUAUCGUCCCUACUUGUUUUGGGCUUUUAGAGUCAUACUCGUAGUACACAACAUUUCUUAUAGCUUGUCCGGGGGGAUCUUCAGUGUUGCCAAGCCGUGGUUCUACGUAGACCCAUAUAUUCAUCUGGCGACCCAGAUGUUCUCAACGGAUGGAUCGGCCCUCUAUUUGGAUCGCCUGUUGACCUUUGGGAAGCUACGAGGUCGUUAUCAGCGCUACGUCGUUAUCAGCACGUGGGUACGGAAAAUAAUUGUUGUAGUCCUGCAAUACAGGUGCGUGUUAUUCCCCGGAGCAGAGAACCAUGGGAACGAUCAGCCUGUAGCUCCUCUGAGAGGUAGCCGCCAGAGGGAAAUUUGGCGGGAAGCAAGCUCACAAUUGGUGGGCGUUAGCCGGAAGCAUACACCCAACUGUGGAGUAUCACAGUUAGCGACGUUAAGUGAAAACGUGGAAGAUCUACAUUGCUGGGUACCUGCGGAGCCGCUAACCGUGCCGGUAGGCCCCAUACGUCAAGCUACUCGGAUCAUAAACCAUAGAAAUACGCAGAUCCGAGUCCAACAAAUACGCUCCCCAAAGUACUCGUAUAUUGGGCAACUUUCGUGUCUCAUGCGUAUGUGUGAGCCAGAUGUUACGGGUCCUUUCAUACCACUAAAUGAGCUAAAAUUCAUUGAUCCGCCAGGAUUCCGUAAAACUCUUAAACCUGUAACCUUCGCAAUAGGGAGGGGCUUAGAGUUCCGCGGCUCCAUGUACCUCGCGGUCGUCCCGCGUCAACAAAUCAUUCUCAGUGCAAAUGCCGGAGGAGGGACAGCUAACCCCAGAGAUGAGCUAAUCUCUGUCGAGGUCUCUCGCCGCUCGCACAUAUCGUGCCUAAGUACGGUACAUUUGGUGGGAACCAUAUACCCGUCUUGCAGCCCGCGCAUAGCAUUGACAUACGUCCAGCCAGUUCGGCGGUGUAGAUAUGGGAGGGGGGUUUUUGUAUGUACGCGCGAAAAAUGGCCCUGGGUCAGCGUUCCGGAUUUAAAGAACUCGAAUCCCACACAACGCGCGCGAACCGCCCUUCAUGAAGACUACAGUUUAGCGUCACGCUCGUACAAUAUGGAGCGGACGUUAGUUACUGGUCUAUCCGACUACUCGGCGUCAAAUGACCGGGGCGCCGGCACCCGCCGUACCUUUGUAAGUGACAUCCUUCUAACUAGUUACGUGAACUCACUCUUUCCUAUAUUUGGCACGCCGUUCGAUUAUCAUUCGCCCAAGUGUACAAGGGUGGGUCAUCAUGGAAGUAUUAUGAACAAUUUUGAGUCGUUUGUGCCAUCGUCGCUCAUGCCUAGAAAUUCAAUAGAAAGAAGAGUAUACGGAAGCUGCUGCACUCCCGACGCCCAUCCGAACUGCGACCAUGGCACAAUGGCUAUACUCGCAGUCAUACCAAGCAUAGCAGUAGUGCCGGGCGGACCCUCUUCCAAUACCAAAAGUAUGGGGCGGACAGCGCGACCGAUCCUUACGAGGCUAAAAGCCGCGCGGCAUUCUGAGGAGUUCGUCUCGCUGCCCUUACCCCUGACGGACAGGAAACACGCAGAAUCGCACAGGGCUGCAUUUCUUUGUUCAGUAUCCUCUGGUUGGGCUUGGAGUUCGAUCCUCUGGUUGCAUCAAGGAUGCCGUAACGCUGUGCAUGCUAAGAUUACGCUCGAUCGAGGGUCCCUGCAUUACUCGCUGACAGUGUCGACCAUAAUGUUUUAUAUUCCUAAGCUUGAGUCUAGACUCUCUAGGAAAUGGCCACCAAUCUGUAAGCCGUUGUGGUUAGGCCGGUCACUCCUACCGAUCGGAGCGCCACCCACUAGCGAUAGCCCCUGCUUCGCCUCCCAUCGCGCAGACCGGAGUGCGUACUUCCCCCAUGGUCACCGGGGUUCCAUCCGACGGGUGGCUAAAACUGAACGGCUACACAGAUGCAUACAACGUGGCGGUAAGGCAUCGCUGGGCAACUUGUUAGGUGAGUCUUCUGAUAGGGAAGCUACAACGGUGGUCACCGCUGUAAUUGCUCCACGUGUCUCUCUGAAGCGAGCCUCCAGUUUUAAACAUAGCGUGACAGUUCCGUGGCGAAGAUGUUUUUGUUUGAGUACCGAGUACAGUUGUCUGCCAACGCGCAGUAUUUAUACCCGCCUGUACUGUCAUCCCGACACGGGUAUUAUUGUCGGUAGUCGAUCCGAGUACGAACUGUAUUGCUGCCGCAUCGUAAGUUUGGUGUGUACGCCAAACGAGUACAUGAAGGGCAGGGAGAGUGCCCGCAUGGCGGGUUGGGUCUGGGGACGCCGCCGCACACUUCUUACACGAGUAUGUCAUGGCUCGAUAUUCUUUGGGCAUGGGUUCACACUACAGCAUAUAGUUGUAGCGACUAGUUGGCUAGAGCGGUAUCCCCAAUAUGUUAUUCAUGGGGACCGGCCUCUACUCAACGCUGCACCACGUUGUGCAGCUUAUAAUGAUUACUCUGAGUCUCGCAGCAUGCGGUCUCGCUUAUUGCGGCCCCUAAAAAAUAUGAUUCCGGCCACCCUACUGAUAUCGCUAAUUUUGGUAUGCAUUCUGUCAAUCAAUUCUCUUACACGAUGCUACUGCCGGUCUCUUGGCGUCGGACGUAGGAAUCGUGCCCAUAAAAGCACCAUCCAGGUACAGGUAACCGCACUAGUGGAUUGUCUCGUUUACUGGGUUCCACAGAUAUACGUCCAAGCUAGUCAGGUUACGCAAUCUGAGUUCAAAGCUAGCGACGUGUUGCAUGGCACCGGCACCGGAACUGUCGUGUUGGCUGAUAACGCUACGAUACGUUGCACACUCUUUCCGGACUGUUCAGCACACCCCACGUCCUCCCGAAAUAAUACCCGCCAAUCGCUUCCCACGGUUAGGUGUGAUGUUAAGAUACUAGUGGCAAUCGGACCUGCAACGUUGGCAUGUGUUCUGGCAGUCCAUGAUGGCCUGUUCACCGUUGGGCGCCCCACACCGCGCAACUUACCCGGAAUAGAAGCUGGGCUUUUCUGGCGAGGUACGCUCGUUAUAAUAUUUCCCUCACGCCACUGUACUAAACGCCUCCCCUCUUCGUCUCGCGAUACCAAAGUCGAGUGGCGUGCCGAGCAGAAUGGCUGUUUACGGAGCACCCGCCGAGACCCAUCUCUGUCACUUGUUGGCGUCAUAGGCGCGGCAACCUGGCAGUUACGGUCAGCGAUGACGCACUUCCACAUCCGCCCCCCUAUUAAACACUGGCCGGUUAGUAGACGAUUCGGACGCACUCUUUUUCCUUCCGGUUACAUUGGGGACGAAGAAUACUGGAAAGUCGUGCUAUUUAUUCAACUUAAAAAGACACCGACGAACUGCACUGAGCUGGGAUUUAAAUCAGAUGGAAGCCGCUGGUUUGCGCCAUCGAGCAUUGACAAUGCGAUUGCGUUAGAAGUCUUGAUCUGCCAUGUCAACGUCUCUGUCACCAACCUACCUCCAGAGCUUAGAUCUGCAAACGUUGCGUUAAUCUGUGCGCUCUGGGGAGAACUGUAUACCCUGUGUUUCAUUUGGCACAGGACGCCAAUCCGGGGACUUAUUCUUAGCUAUUUCCCGGACUGGAAGCUCCGGGGUCCGCACUGUCCUAAAUCUACGCCCCUUUGGGCUGAGCGAAGGUCGAACGCCUCUCUCACGCUCAACAGCUCCGAUCAACGCUCGGGACGUAAAUCGAUACCUCUGUUCGGACAGUAUAAAACCCUACAUGAUGGGCACGUGUCAACUAUACUCAGAACCCUAUCCAAGAAUUAUAGCACCGCAGUGAGACAGAAACACUCCUCGUUUAGGUACUAUGAGCUUGGGCCCUUUAUUUACUGCUACCAGACAAGCGCAUUGUGUGUGACACCAACGCGUCUUAAUGAGUGGAUCGUCACCGGCACGGAACCGCUGACCCGCCUUCGUUCACAUCCCAAAUGUACAAGUCUGCAUUGGUUGGCUUCGACUUCUCUCGGUUUUAUCUUCUCCGCACUAGCGAAUCCGCCAAGGAGGCCCGUUGAGUCUGGAGAUAGGGGACUGUUGACGGGUCAAGGCAAAGUUUUGGGAUUGCCCCAGGAAACAGGUCCUGAGAAAUUCUUAUUGGGUCGACUUAAUAUCAACAACAAGUCUAACAAAUUAAUAAUCUAUAACUUACUAGAUACGCCUUGUUGGCGGAUCGGGUCAAGCAUCGUGACCUUUUCGAGCACGAUUCAGGUUAUUCUCUGGAAAGCCAAUGGCCCGUGCAUUUACGUCAGGCUUAUGGCUCUUCACGCUGACGCCUAUUGCAACGGAUUAAACCUCCAAGUGAUCGCUAAAACCCAUUAUUCAUGCGUUGCCUUUCCACAUUUUUGCAUAGUGUUAUUUAGAGCUCAAGGUACGUUUGAGCAUACCGACUGUUUUCCGAUUGCUUGCUUAAUCGAUGAAACAGCGCGGGCUUUGCUAGGAAUAGGGAGGCACCGAAUCUUUAUGCUGCGGCUCGCCGUUUUGUCCCAUUCCGAUAAUCGUAGUUCGCUACUACUGGUUCGAAUGUUGAAACUUCAUCUGUGGGCCGUGACGCUCGAGAAGGCGGAUCCUACAAGUCACUGCGGAAGUCCGACGACUCAGGAUGAGGAUCACUGCAUUGCUCUAUAUGGUUGUUUGGUCGCACAUUCUACUACAGUAAAGGCACAAUUAAGCUUCGAUGCGAACGCAGAGUCCAUUCUAUACGGAACCAGUCAGAUUAACCAGUUGGUUCAAGGGCCCUUUUAUCAAUCGAGCGGGCCAACCUUACUAGAUCUUUUCUGUAGUAUUCCUGACUUGGAUCUUGUCUAUGGACAGGGGCACGGCUCGCUGGAAACUUGGAGAACCGAAAUACGAGAAGCUGAUGAUAUACGAGUGGGGCAAACGCACCAACCUCAUUGUAAGGGUGGGACUCAGGACGAUAGAGUACAAAUGCUAGUCAACACCCCAUCUCAUCCUAACUUCUGGGUGGUCAUUUCUAAGGUUACGAAGGUCAAGAAACCCUUUAAAUCAUGUAGCUCACUGCUGCGGUCCCUAGCGCCACAAGAGUCUCGGAUGUUCUCGAUCUUCGUGCACUUCAACCAGCGUUUUGGCUCCGCACACAUACGAUGCGGAACAGGAACCACAAGUCGCCGUGGCCCUUACAGCUUGCCGGGACAGCUACGCGAUGUGCCCACACGGGGAAGGAACGCUUUAACUAUUCCGUACUGUAACCUUGCGCUUAGGCAAAUCACUCUCGUCACUAGACGCUACGAUGCUGGCUCUAAAUAUGACUGCAAGGCACCGGAUCCCAACCAAGCACCCGUGCUUCUGUUUGAGUUAGCGCAAAUUCCAGGCGCGCGCAGAGCUUUGGUCACGGGCAUACUCUUUUAUGCGGCGUACCAGGUAUCAUUAGGAUGGAACAAAUCUUCAGAAGUUGCCGGGGAGCGUCUAUCCGCCAAGUUCUCCCUGCAACAUCUCGCGAACUUCACGCGAAACUGCGUGAUGCAUAGCGACGAUCCCAAAACGUCUUUGCUCUUACUAAUGCGUGAGGAAGUGCUCUAG"
//  println(s"peptideFor($pattern) = " + peptideFor(pattern))
//
//  println(reverseGenericCode.mkString(","))
//  val pattern = "CCUCGUACUGAUAUUAAU"
//  println(s"peptideFor($pattern) = " + peptideFor(pattern))

  val tyrocidineB1 = "VKLFPWFNQY"
////  val peptide = tyrocidineB1
////  println(s"countPossiblePatternsTranslatingToPeptide($peptide) = " + countPossiblePatternsTranslatingToPeptide(peptide))
//  val peptide = "SYNGE"
//  println(s"countPossiblePatternsTranslatingToPeptide($peptide) = " + countPossiblePatternsTranslatingToPeptide(peptide))
//
//  //val dnaPattern = "ATGGCCATGGCCCCCAGAACTGAGATCAATAGTACCCGTATTAACGGGTGA"
//  val dnaPattern = Source.fromFile("src/main/resources/BacillusBrevis.txt").getLines().mkString
//  //val peptide = "MA"
//  println(dnaPattern.length)
//  val peptide = tyrocidineB1
////  println(s"dnaEncodesPeptide($dnaPattern,$peptide) = \n" + dnaEncodesPeptide(dnaPattern, peptide).mkString("\n"))
//  println(s"fastDnaEncodesPeptide($dnaPattern,$peptide) = \n" + fastDnaEncodesPeptide(dnaPattern, peptide).mkString("\n"))

//  val peptide = tyrocidineB1
////  println(s"massOf($peptide) = " + massOf(peptide))
//  val spectrum = theoreticalSpectrum(peptide)
//  println(s"theoreticalSpectrum($peptide) = " + spectrum.mkString(","))
////  println(spectrum.map(_._2).mkString(" "))
//  val fastspectrum = fastCyclicSpectrum(peptide)
//  println(s"fastCyclicSpectrum($peptide) = " + fastspectrum.mkString(","))
//  println(fastspectrum.map(_._2).mkString(" "))

//  val peptideN = Seq(113,128,186)
//  println(s"fastCyclicSpectrumNumeric(${peptideN.mkString("-")}) = " + fastCyclicSpectrumNumeric(peptideN).mkString(" "))

//    var peptide = "ALTM"
//  println(s"fastCyclicSpectrum($peptide)" + fastCyclicSpectrum(peptide).map(_._2).mkString(" "))
//  peptide = "IAMT"
//  println(s"fastCyclicSpectrum($peptide)" + fastCyclicSpectrum(peptide).map(_._2).mkString(" "))
//  peptide = "MAIT"
//  println(s"fastCyclicSpectrum($peptide)" + fastCyclicSpectrum(peptide).map(_._2).mkString(" "))
//  peptide = "TMIA"
//  println(s"fastCyclicSpectrum($peptide)" + fastCyclicSpectrum(peptide).map(_._2).mkString(" "))
//  peptide = "MIAT"
//  println(s"fastCyclicSpectrum($peptide)" + fastCyclicSpectrum(peptide).map(_._2).mkString(" "))
//  peptide = "TMLA"
//  println(s"fastCyclicSpectrum($peptide)" + fastCyclicSpectrum(peptide).map(_._2).mkString(" "))

//  val spectrum =  "0 71 97 99 103 113 113 114 115 131 137 196 200 202 208 214 226 227 228 240 245 299 311 311 316 327 337 339 340 341 358 408 414 424 429 436 440 442 453 455 471 507 527 537 539 542 551 554 556 566 586 622 638 640 651 653 657 664 669 679 685 735 752 753 754 756 766 777 782 782 794 848 853 865 866 867 879 885 891 893 897 956 962 978 979 980 980 990 994 996 1022 1093".split(" ").map(_.toInt)
//  val spectrum = "0 87 87 99 101 103 128 129 129 131 137 156 174 216 216 227 230 231 234 236 238 285 287 303 303 317 330 337 362 364 367 372 390 404 416 432 454 459 461 465 466 467 503 518 519 533 541 553 568 588 590 594 598 606 617 640 647 670 681 689 693 697 699 719 734 746 754 768 769 784 820 821 822 826 828 833 855 871 883 897 915 920 923 925 950 957 970 984 984 1000 1002 1049 1051 1053 1056 1057 1060 1071 1071 1113 1131 1150 1156 1158 1158 1159 1184 1186 1188 1200 1200 1287".split(" ").map(_.toInt)
//  val spectrum =  "0 97 97 99 101 103 196 198 198 200 202 295 297 299 299 301 394 396 398 400 400 497".split(" ").map(_.toInt)
//  val spectrum =  "0 101 103 113 113 113 115 128 128 131 137 163 204 216 226 228 228 244 256 259 264 265 300 317 331 341 341 357 367 372 387 393 401 428 432 444 454 472 480 485 500 504 524 529 545 556 557 585 595 600 613 617 632 637 657 658 687 688 708 713 728 732 745 750 760 788 789 800 816 821 841 845 860 865 873 891 901 913 917 944 952 958 973 978 988 1004 1004 1014 1028 1045 1080 1081 1086 1089 1101 1117 1117 1119 1129 1141 1182 1208 1214 1217 1217 1230 1232 1232 1232 1242 1244 1345".split(" ").map(_.toInt)
//  val spectrum =  "0 57 57 114 114 171 171 228".split(" ").map(_.toInt)
//  val spectrum = "0 113 128 186 241 299 314 427".split(" ").map(_.toInt)
//  val spectrum = "0 97 101 101 103 113 128 128 156 163 198 216 225 229 241 257 264 266 284 326 338 344 354 367 379 385 385 392 439 441 480 482 482 486 495 507 542 548 583 595 604 608 608 610 649 651 698 705 705 711 723 736 746 752 764 806 824 826 833 849 861 865 874 892 927 934 962 962 977 987 989 989 993 1090".split(" ").map(_.toInt)
//  val spectrum = "0 71 87 99 113 137 137 147 156 170 186 200 227 250 257 284 284 293 299 326 337 364 370 397 413 421 436 440 463 484 507 511 526 534 550 577 583 610 621 648 654 663 663 690 697 720 747 761 777 791 800 810 810 834 848 860 876 947".split(" ").map(_.toInt)
//  println(s"cyclopeptideSequencingBranchAndBound({" + spectrum.mkString(",") + "}) = " + cyclopeptideSequencingBranchAndBound(spectrum).map(_.mkString("-")).mkString(" "))

//  val spectrum = "0 71 99 101 103 128 129 199 200 204 227 230 231 298 303 328 330 332 333".split(" ").map(_.toInt)
//  var peptide = "TCE"
//  println(s"consistent(fastLinearSpectrum($peptide), $spectrum) = " + consistent(fastLinearSpectrum(peptide).map(_._2), spectrum))
//  peptide = "CTQ"
//  println(s"consistent(fastLinearSpectrum($peptide), $spectrum) = " + consistent(fastLinearSpectrum(peptide).map(_._2), spectrum))
//  peptide = "ETC"
//  println(s"consistent(fastLinearSpectrum($peptide), $spectrum) = " + consistent(fastLinearSpectrum(peptide).map(_._2), spectrum))
//  peptide = "AQV"
//  println(s"consistent(fastLinearSpectrum($peptide), $spectrum) = " + consistent(fastLinearSpectrum(peptide).map(_._2), spectrum))
//  peptide = "TCQ"
//  println(s"consistent(fastLinearSpectrum($peptide), $spectrum) = " + consistent(fastLinearSpectrum(peptide).map(_._2), spectrum))
//  peptide = "AVQ"
//  println(s"consistent(fastLinearSpectrum($peptide), $spectrum) = " + consistent(fastLinearSpectrum(peptide).map(_._2), spectrum))

//  val peptide = "THSQGPWPTASIKGHMYGCGGMELLSQIAWWGSHPSGPVPVEKFK"
//  val spectrum = "0 57 57 57 57 71 71 87 87 87 87 87 97 97 97 97 97 99 101 101 103 113 113 114 114 115 128 128 128 128 128 129 129 129 131 131 131 137 137 137 137 144 147 147 154 158 163 172 185 186 186 186 186 188 190 194 194 194 196 198 200 201 202 215 215 218 224 224 228 228 229 234 238 240 241 241 242 243 245 247 251 257 257 259 260 260 265 268 269 271 275 275 275 283 283 291 293 294 294 298 300 315 317 317 322 325 325 325 327 327 328 330 338 338 344 346 348 352 352 356 356 357 362 362 366 372 372 373 374 374 374 376 380 384 384 385 387 390 399 403 404 404 414 420 422 423 425 430 431 433 435 435 441 441 443 443 444 447 449 453 453 453 453 455 456 469 471 477 480 481 481 486 487 488 488 489 500 503 503 504 505 513 517 519 522 524 528 532 532 534 537 538 542 545 548 548 550 552 554 557 557 562 566 568 572 573 575 576 578 581 585 586 590 590 590 593 597 600 600 602 611 616 616 618 619 619 627 631 632 633 634 635 637 639 641 644 645 647 653 654 655 661 663 665 668 668 675 682 687 689 689 691 694 697 703 704 704 710 714 718 718 719 724 728 728 728 729 731 732 732 734 746 747 747 748 748 752 760 762 766 770 771 772 774 776 779 781 783 790 791 791 791 794 797 799 805 805 805 813 815 816 817 817 818 825 825 829 832 833 840 847 850 851 856 857 860 861 862 862 865 868 869 869 873 876 876 880 884 887 889 894 900 903 904 904 907 910 919 919 920 921 922 922 926 928 928 933 936 937 938 946 947 949 956 957 962 964 965 966 970 970 971 975 976 977 978 979 985 988 989 990 990 991 993 993 1001 1006 1017 1017 1018 1023 1025 1028 1031 1031 1033 1034 1035 1041 1041 1041 1046 1053 1054 1056 1062 1063 1065 1066 1071 1074 1074 1076 1077 1080 1084 1085 1090 1093 1098 1103 1104 1104 1107 1108 1112 1118 1118 1119 1121 1121 1122 1122 1125 1128 1133 1142 1143 1145 1147 1147 1148 1150 1151 1156 1159 1160 1161 1162 1164 1164 1167 1169 1175 1178 1178 1181 1190 1199 1202 1204 1205 1211 1216 1217 1219 1221 1221 1222 1224 1229 1232 1232 1234 1234 1235 1240 1241 1243 1245 1246 1246 1247 1248 1248 1250 1258 1261 1265 1270 1270 1271 1272 1276 1276 1278 1289 1290 1293 1293 1295 1303 1304 1304 1305 1306 1318 1318 1319 1321 1333 1339 1342 1342 1345 1346 1347 1350 1350 1350 1357 1361 1362 1363 1363 1363 1368 1369 1369 1371 1371 1373 1375 1379 1380 1387 1390 1391 1392 1393 1400 1402 1402 1405 1407 1407 1407 1407 1415 1415 1417 1418 1418 1426 1426 1432 1437 1448 1450 1458 1459 1464 1466 1470 1470 1470 1474 1474 1476 1476 1478 1481 1487 1493 1494 1494 1494 1497 1499 1500 1502 1504 1504 1505 1505 1507 1508 1512 1512 1518 1519 1521 1521 1527 1531 1536 1544 1545 1547 1551 1557 1561 1563 1563 1565 1565 1565 1565 1569 1584 1588 1589 1590 1591 1594 1598 1601 1601 1602 1603 1603 1604 1608 1608 1609 1617 1618 1622 1622 1628 1631 1631 1631 1632 1633 1634 1636 1644 1645 1650 1655 1655 1656 1664 1666 1667 1674 1675 1678 1678 1681 1685 1688 1690 1693 1693 1693 1694 1694 1698 1698 1699 1702 1712 1715 1718 1719 1720 1726 1734 1735 1737 1742 1745 1745 1745 1745 1747 1749 1751 1753 1759 1759 1761 1764 1765 1765 1773 1775 1776 1780 1781 1783 1791 1792 1794 1795 1795 1795 1796 1799 1802 1806 1812 1821 1822 1822 1822 1822 1830 1832 1832 1836 1840 1844 1846 1848 1850 1851 1852 1856 1857 1860 1868 1870 1873 1873 1874 1874 1878 1878 1879 1879 1892 1893 1893 1895 1896 1896 1905 1909 1909 1911 1912 1923 1930 1931 1935 1935 1937 1939 1939 1941 1944 1947 1949 1950 1950 1950 1953 1957 1958 1960 1961 1964 1967 1969 1969 1973 1975 1977 1980 1980 1983 1988 1992 1993 1993 2002 2002 2005 2007 2008 2010 2018 2018 2024 2024 2031 2032 2034 2036 2037 2037 2038 2040 2042 2050 2051 2054 2059 2064 2064 2065 2067 2070 2070 2072 2075 2080 2086 2089 2089 2094 2097 2097 2097 2097 2098 2101 2108 2111 2111 2115 2117 2117 2121 2121 2129 2133 2137 2137 2137 2138 2139 2139 2146 2151 2152 2155 2158 2162 2162 2167 2168 2169 2174 2179 2182 2184 2186 2188 2194 2195 2196 2198 2198 2198 2204 2208 2208 2210 2212 2212 2217 2218 2218 2220 2225 2225 2226 2230 2231 2233 2240 2252 2252 2255 2264 2265 2266 2266 2267 2268 2269 2271 2275 2275 2276 2283 2283 2283 2283 2295 2295 2295 2297 2299 2299 2299 2309 2311 2312 2315 2317 2321 2325 2326 2327 2332 2332 2335 2346 2348 2349 2352 2354 2356 2358 2361 2362 2363 2364 2367 2368 2377 2380 2380 2380 2380 2382 2392 2392 2394 2396 2397 2397 2398 2402 2403 2403 2408 2412 2412 2413 2415 2422 2426 2427 2436 2445 2446 2448 2449 2449 2454 2454 2460 2463 2463 2464 2467 2469 2469 2477 2483 2484 2489 2489 2490 2491 2492 2493 2493 2495 2495 2506 2510 2511 2511 2511 2512 2523 2525 2528 2535 2535 2536 2539 2540 2540 2541 2543 2546 2550 2550 2550 2550 2554 2566 2566 2566 2566 2578 2582 2582 2582 2582 2586 2589 2591 2592 2592 2593 2596 2597 2597 2604 2607 2609 2620 2621 2621 2621 2622 2626 2637 2637 2639 2639 2640 2641 2642 2643 2643 2648 2649 2655 2663 2663 2665 2668 2669 2669 2672 2678 2678 2683 2683 2684 2686 2687 2696 2705 2706 2710 2717 2719 2720 2720 2724 2729 2729 2730 2734 2735 2735 2736 2738 2740 2740 2750 2752 2752 2752 2752 2755 2764 2765 2768 2769 2770 2771 2774 2776 2778 2780 2783 2784 2786 2797 2800 2800 2805 2806 2807 2811 2815 2817 2820 2821 2823 2833 2833 2833 2835 2837 2837 2837 2849 2849 2849 2849 2856 2857 2857 2861 2863 2864 2865 2866 2866 2867 2868 2877 2880 2880 2892 2899 2901 2902 2906 2907 2907 2912 2914 2914 2915 2920 2920 2922 2924 2924 2928 2934 2934 2934 2936 2937 2938 2944 2946 2948 2950 2953 2958 2963 2964 2965 2970 2970 2974 2977 2980 2981 2986 2993 2993 2994 2995 2995 2995 2999 3003 3011 3011 3015 3015 3017 3021 3021 3024 3031 3034 3035 3035 3035 3035 3038 3043 3043 3046 3052 3057 3060 3062 3062 3065 3067 3068 3068 3073 3078 3081 3082 3090 3092 3094 3095 3095 3096 3098 3100 3101 3108 3108 3114 3114 3122 3124 3125 3127 3130 3130 3139 3139 3140 3144 3149 3152 3152 3155 3157 3159 3163 3163 3165 3168 3171 3172 3174 3175 3179 3182 3182 3182 3183 3185 3188 3191 3193 3193 3195 3197 3197 3201 3202 3209 3220 3221 3223 3223 3227 3236 3236 3237 3239 3239 3240 3253 3253 3254 3254 3258 3258 3259 3259 3262 3264 3272 3275 3276 3280 3281 3282 3284 3286 3288 3292 3296 3300 3300 3302 3310 3310 3310 3310 3311 3320 3326 3330 3333 3336 3337 3337 3337 3338 3340 3341 3349 3351 3352 3356 3357 3359 3367 3367 3368 3371 3373 3373 3379 3381 3383 3385 3387 3387 3387 3387 3390 3395 3397 3398 3406 3412 3413 3414 3417 3420 3430 3433 3434 3434 3438 3438 3439 3439 3439 3442 3444 3447 3451 3454 3454 3457 3458 3465 3466 3468 3476 3477 3477 3482 3487 3488 3496 3498 3499 3500 3501 3501 3501 3504 3510 3510 3514 3515 3523 3524 3524 3528 3529 3529 3530 3531 3531 3534 3538 3541 3542 3543 3544 3548 3563 3567 3567 3567 3567 3569 3569 3571 3575 3581 3585 3587 3588 3596 3601 3605 3611 3611 3613 3614 3620 3620 3624 3625 3627 3627 3628 3628 3630 3632 3633 3635 3638 3638 3638 3639 3645 3651 3654 3656 3656 3658 3658 3662 3662 3662 3666 3668 3673 3674 3682 3684 3695 3700 3706 3706 3714 3714 3715 3717 3717 3725 3725 3725 3725 3727 3730 3730 3732 3739 3740 3741 3742 3745 3752 3753 3757 3759 3761 3761 3763 3763 3764 3769 3769 3769 3770 3771 3775 3782 3782 3782 3785 3786 3787 3790 3790 3793 3799 3811 3813 3814 3814 3826 3827 3828 3828 3829 3837 3839 3839 3842 3843 3854 3856 3856 3860 3861 3862 3862 3867 3871 3874 3882 3884 3884 3885 3886 3886 3887 3889 3891 3892 3897 3898 3898 3900 3900 3903 3908 3910 3911 3911 3913 3915 3916 3921 3927 3928 3930 3933 3942 3951 3954 3954 3957 3963 3965 3968 3968 3970 3971 3972 3973 3976 3981 3982 3984 3985 3985 3987 3989 3990 3999 4004 4007 4010 4010 4011 4011 4013 4014 4014 4020 4024 4025 4028 4028 4029 4034 4039 4042 4047 4048 4052 4055 4056 4058 4058 4061 4066 4067 4069 4070 4076 4078 4079 4086 4091 4091 4091 4097 4098 4099 4101 4101 4104 4107 4109 4114 4115 4115 4126 4131 4139 4139 4141 4142 4142 4143 4144 4147 4153 4154 4155 4156 4157 4161 4162 4162 4166 4167 4168 4170 4175 4176 4183 4185 4186 4194 4195 4196 4199 4204 4204 4206 4210 4210 4211 4212 4213 4213 4222 4225 4228 4228 4229 4232 4238 4243 4245 4248 4252 4256 4256 4259 4263 4263 4264 4267 4270 4270 4271 4272 4275 4276 4281 4282 4285 4292 4299 4300 4303 4307 4307 4314 4315 4315 4316 4317 4319 4327 4327 4327 4333 4335 4338 4341 4341 4341 4342 4349 4351 4353 4356 4358 4360 4361 4362 4366 4370 4372 4380 4384 4384 4385 4385 4386 4398 4400 4400 4401 4403 4404 4404 4404 4408 4413 4414 4414 4418 4422 4428 4428 4429 4435 4438 4441 4443 4443 4445 4450 4457 4464 4464 4467 4469 4471 4477 4478 4479 4485 4487 4488 4491 4493 4495 4497 4498 4499 4500 4501 4505 4513 4513 4514 4516 4516 4521 4530 4532 4532 4535 4539 4542 4542 4542 4546 4547 4551 4554 4556 4557 4559 4560 4564 4566 4570 4575 4575 4578 4580 4582 4584 4584 4587 4590 4594 4595 4598 4600 4600 4604 4608 4610 4613 4615 4619 4627 4628 4629 4629 4632 4643 4644 4644 4645 4646 4651 4651 4652 4655 4661 4663 4676 4677 4679 4679 4679 4679 4683 4685 4688 4689 4689 4691 4691 4697 4697 4699 4701 4702 4707 4709 4710 4712 4718 4728 4728 4729 4733 4742 4745 4747 4748 4748 4752 4756 4758 4758 4758 4759 4760 4760 4766 4770 4770 4775 4776 4776 4780 4780 4784 4786 4788 4794 4794 4802 4804 4805 4805 4807 4807 4807 4810 4815 4815 4817 4832 4834 4838 4838 4839 4841 4849 4849 4857 4857 4857 4861 4863 4864 4867 4872 4872 4873 4875 4875 4881 4885 4887 4889 4890 4891 4891 4892 4894 4898 4903 4904 4904 4908 4908 4914 4917 4917 4930 4931 4932 4934 4936 4938 4938 4938 4942 4944 4946 4946 4946 4946 4947 4960 4969 4974 4978 4985 4985 4988 4995 4995 4995 4995 5001 5001 5001 5003 5003 5003 5004 5004 5004 5004 5004 5017 5018 5018 5019 5019 5029 5031 5031 5033 5035 5035 5035 5035 5035 5045 5045 5045 5045 5045 5061 5061 5075 5075 5075 5075 5132".split(" ").map(_.toInt)
//  val peptide = "MAMA"
//  val spectrum = "0 71 71 71 131 131 131 156 198 199 199 202 202 202 333 333 333 404 404".split(" ").map(_.toInt)
//  println(s"scoreCyclic($peptide), " + spectrum.mkString(" ") + ") = " + score(cyclicSpectrum(peptideNumeric(peptide)), spectrum))
  val peptide ="IDLNPQVWSLLYMWQAFIFTTTIKWVHIGVCFHQPE"
  val spectrum = "0 57 71 71 71 87 97 97 99 99 99 101 101 103 113 113 113 113 113 113 114 115 128 128 128 128 128 128 128 129 131 137 137 147 147 156 163 168 186 186 199 199 200 200 202 204 208 211 212 214 225 226 227 227 227 228 236 241 241 242 242 243 250 260 265 265 273 275 276 284 284 285 294 296 305 307 313 313 314 315 317 324 328 336 339 339 340 342 349 351 354 355 356 357 363 364 364 372 374 385 386 389 389 407 412 412 413 418 420 431 433 438 441 443 445 452 453 454 455 460 463 464 467 470 476 477 477 480 483 485 492 492 499 500 502 510 511 514 516 520 540 546 548 554 556 563 565 565 566 567 568 576 580 582 588 590 591 593 593 597 598 605 607 608 611 613 624 627 639 639 639 645 655 659 661 662 664 678 679 681 691 691 693 694 695 696 704 704 706 708 710 710 711 718 721 726 738 752 758 758 761 762 765 767 767 774 776 792 792 792 793 793 794 806 806 807 807 808 817 819 823 824 832 834 838 839 851 863 864 867 875 877 878 889 892 893 895 904 905 905 905 918 919 920 921 921 921 935 935 937 945 952 954 966 975 978 979 979 980 986 988 992 1003 1006 1006 1008 1018 1019 1020 1023 1034 1034 1042 1046 1048 1065 1067 1067 1068 1072 1074 1078 1079 1092 1100 1107 1109 1113 1116 1116 1117 1119 1121 1122 1134 1147 1147 1147 1155 1170 1171 1171 1178 1180 1180 1181 1187 1192 1193 1206 1206 1210 1218 1226 1228 1229 1231 1234 1244 1247 1250 1269 1272 1277 1283 1284 1284 1284 1293 1294 1294 1303 1307 1315 1320 1323 1330 1334 1343 1346 1355 1355 1357 1359 1373 1381 1382 1385 1397 1397 1397 1405 1406 1412 1412 1417 1420 1426 1428 1431 1431 1433 1451 1456 1458 1474 1483 1483 1484 1486 1486 1498 1502 1510 1511 1519 1525 1529 1532 1534 1534 1544 1545 1545 1554 1559 1564 1567 1582 1585 1587 1599 1599 1611 1614 1616 1626 1630 1631 1639 1647 1657 1660 1662 1663 1666 1669 1670 1673 1681 1686 1710 1710 1712 1727 1727 1727 1733 1740 1744 1744 1745 1769 1771 1773 1786 1788 1794 1794 1797 1799 1800 1809 1826 1830 1838 1840 1840 1857 1858 1859 1870 1871 1872 1873 1880 1891 1896 1897 1901 1922 1923 1927 1928 1939 1953 1963 1971 1972 1977 1985 1985 1986 1988 1993 1994 1994 1998 2005 2024 2027 2034 2040 2051 2052 2076 2084 2090 2091 2095 2099 2101 2101 2108 2113 2122 2122 2122 2133 2139 2147 2155 2171 2189 2190 2193 2196 2204 2209 2212 2212 2214 2219 2226 2236 2248 2250 2260 2276 2290 2302 2303 2308 2309 2310 2317 2318 2321 2325 2325 2337 2340 2347 2351 2361 2404 2416 2418 2423 2424 2431 2436 2437 2438 2439 2452 2453 2462 2464 2465 2503 2507 2533 2544 2550 2551 2551 2552 2553 2561 2565 2567 2578 2590 2602 2604 2631 2632 2638 2649 2664 2666 2666 2679 2689 2689 2689 2691 2715 2730 2735 2760 2763 2778 2779 2786 2788 2792 2794 2801 2817 2817 2828 2843 2857 2891 2898 2900 2907 2907 2914 2914 2915 2916 2916 2956 2964 2971 3006 3011 3013 3020 3027 3028 3028 3043 3044 3063 3099 3101 3114 3119 3124 3127 3141 3143 3156 3191 3200 3211 3214 3229 3255 3255 3256 3271 3288 3300 3327 3328 3328 3370 3383 3384 3397 3399 3402 3425 3456 3483 3496 3498 3527 3530 3539 3553 3611 3624 3624 3645 3667 3667 3721 3738 3758 3782 3795 3835 3866 3895 3910 3963 3981 4023 4078 4094 4191".split(" ").map(_.toInt)
  println(s"scoreLinear($peptide), " + spectrum.mkString(" ") + ") = " + score(linearSpectrum(peptideNumeric(peptide)), spectrum))

////  val spectrum = "0 113 114 128 129 227 242 242 257 355 356 370 371 484".split(" ").map(_.toInt)
////  val spectrum = "0 99 113 114 128 227 257 299 355 356 370 371 484".split(" ").map(_.toInt)
////  val spectrum = "131 457 931 832 572 946 331 131 844 215 588 344 372 200 0 960 600 230 244 113 444 359 129 257 715 471 340 574 930 471 487 241 702 946 829 244 260 602 671 928 459 802 328 799 815 584 572 113 99 487 731 687 859 815 615 475 928 700 588 128 1059 358 388 485 931 972 357 701 87 818 728 227 128 719".split(" ").map(_.toInt)
//  val spectrum = "0 86 160 234 308 320 382".split(" ").map(_.toInt)
//  //val r: Map[Int, Int] = spectralConvolution(spectrum).groupBy(k => k).map(kv => (kv._1, kv._2.length))
////  println(s"spectralConvolution($spectrum) = " + spectralConvolution(spectrum).mkString(" "))
//  println(s"spectralConvolution($spectrum) = " + spectralConvolution(spectrum).groupBy(k => k).map(kv => (kv._1,kv._2.length)).mkString(" "))
}
