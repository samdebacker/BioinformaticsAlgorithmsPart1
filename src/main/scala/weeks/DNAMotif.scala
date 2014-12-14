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

package weeks

import scala.reflect.macros.whitebox.Context

object DNAMotif {
  @inline def from(value: String): Option[DNAMotif] = isValid(value) match {
    case Some(valid) ⇒ Some(new DNAMotif(valid))
    case _           ⇒ None
  }
  private def isValid(value: String): Option[IndexedSeq[DNAString]] = {
    val dnaStrings: Array[String] = value.trim.split("""\W+""")
    if (dnaStrings.forall { s ⇒ s.length == dnaStrings.head.length && DNAString.isValid(s) }) {
      Some(dnaStrings.map(s ⇒ new DNAString(s.toUpperCase)).toIndexedSeq)
    } else {
      None
    }
  }

  import scala.language.experimental.macros
  import scala.language.implicitConversions
  implicit def apply(value: String): DNAMotif = macro applyMacro
  def applyMacro(c: Context)(value: c.Expr[String]) = {
    import c.universe._
    value.tree match {
      case Literal(stringConst) ⇒
        val literalValue = stringConst.value.toString
        isValid(literalValue) match {
          case Some(dnaStrings) ⇒
          case _                ⇒ c.abort(c.enclosingPosition, "DNAMotif must be formed of DNAStrings all of the same length, and can only contain nucleotides A, C, G or T")
        }
      case _ ⇒
        c.abort(c.enclosingPosition, "DNAMotif macro only works on String Literals, use DNAMotif.form(String) instead.")
    }
    // Pitty we cannot access the private constructor here
    q"DNAMotif.from($value).get"
  }
}

final class DNAMotif private[weeks] (val value: IndexedSeq[DNAString]) extends AnyVal {
  override def toString: String = value.toString
  def k: Int = value.head.value.length
}
