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

import scala.reflect.macros.whitebox.Context

object DNAMotifMacro {
  def applyMacro(c: Context)(value: c.Expr[String]): c.Tree = {
    import c.universe._
    value.tree match {
      case Literal(stringConst) ⇒
        val literalValue = stringConst.value.toString
        DNAMotif.isValid(literalValue) match {
          case Some(dnaStrings) ⇒
            val args = dnaStrings.map(dnaString ⇒ q"DNAString(${dnaString.toString})")
            q"DNAMotif.unsafeFrom(IndexedSeq(..$args))"
          case _ ⇒
            c.abort(c.enclosingPosition, "DNAMotif must be formed of DNAStrings all of the same length, and can only contain nucleotides A, C, G or T")
        }
      case _ ⇒
        c.abort(c.enclosingPosition, "DNAMotif macro only works on String Literals, use DNAMotif.form(String) instead.")
    }
  }
}
