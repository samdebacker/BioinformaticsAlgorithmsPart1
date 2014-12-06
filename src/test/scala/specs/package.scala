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

import breeze.linalg.DenseMatrix
import org.scalatest.Tag
import weeks.Week3._

package object specs {
  // Run with
  //  testOnly FullSpec -- -l SlowTest
  // to exclude tests that are marked as slow
  object SlowTest extends Tag("SlowTest")

  implicit class DnaHelper(val sc: StringContext) extends AnyVal {
    def dna(args: Any*): IndexedSeq[DNA] = {
      sc.parts.mkString.stripMargin.toUpperCase.split("\\s+").toIndexedSeq
    }
  }

  implicit class ProfileHelper(val sc: StringContext) extends AnyVal {
    def profile(args: Any*): Profile = {
      val m = sc.parts.mkString.stripMargin.split("\\s+").map(_.toDouble)
      new DenseMatrix(m.length / 4, 4, m).t
    }
  }
}
