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
import org.scalatest.{FeatureSpec, Matchers}

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
}
