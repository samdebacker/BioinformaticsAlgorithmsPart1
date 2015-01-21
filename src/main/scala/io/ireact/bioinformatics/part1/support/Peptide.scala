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

package io.ireact.bioinformatics.part1.support

import scala.util.matching.Regex

object Peptide {
  @inline def from(value: String): Option[Peptide] = {
    if (isValid(value)) Some(unsafeFrom(value)) else None
  }
  @inline def from(value: Seq[Int]): Peptide = new Peptide(value)

  @inline def unsafeFrom(value: String): Peptide = new Peptide(value.split(' ').map(_.toInt))

  private[this] val peptideString: Regex = """(\d)+( (\d)+)*""".r
  private[support] def isValid(value: String): Boolean = value.trim match {
    case peptideString(_*) ⇒ true
    case _                 ⇒ false
  }

  import scala.language.experimental.macros
  import scala.language.implicitConversions
  implicit def apply(value: String): Peptide = macro PeptideMacro.applyMacro
}

final class Peptide private[support] (val value: Seq[Int]) extends AnyVal {
  override def toString: String = value.mkString("-")
}