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

package io.ireact.bioinformatics.part1

import breeze.linalg.DenseMatrix
import io.ireact.bioinformatics.part1.Chapter3._

package object support {

  implicit class DnaHelper(val sc: StringContext) extends AnyVal {
    @deprecated("use DNAString and DNAMotif", "0.1")
    def dna(args: Any*): IndexedSeq[DNA] = {
      sc.parts.mkString.trim.toUpperCase.split("\\W+").toIndexedSeq
    }
  }

  implicit class ProfileHelper(val sc: StringContext) extends AnyVal {
    def profile(args: Any*): Profile = {
      val m = sc.parts.mkString.trim.split("[^\\d\\.]+").map(_.toDouble)
      new DenseMatrix(m.length / 4, 4, m).t
    }
  }
}
