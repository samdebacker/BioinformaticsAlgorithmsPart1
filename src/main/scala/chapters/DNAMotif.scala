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

package chapters

object DNAMotif {
  @inline def from(value: String): Option[DNAMotif] = isValid(value).map(new DNAMotif(_))

  /**
   * /!\ For macro use, do not call this method directly /!\
   * This way of construction is unsafe since it allows DNAStrings of different length, the macro checks this.
   */
  @inline def unsafeFrom(value: IndexedSeq[DNAString]): DNAMotif = new DNAMotif(value)

  private[chapters] def isValid(value: String): Option[IndexedSeq[DNAString]] = {
    val dnaStrings: Array[String] = value.trim.split("""\W+""")
    if (dnaStrings.forall { s ⇒ s.length == dnaStrings.head.length && DNAString.isValid(s) }) {
      Some(dnaStrings.map(s ⇒ new DNAString(s.toUpperCase)).toIndexedSeq)
    } else {
      None
    }
  }

  import scala.language.experimental.macros
  import scala.language.implicitConversions
  implicit def apply(value: String): DNAMotif = macro DNAMotifMacro.applyMacro
}

final class DNAMotif private[chapters] (val value: IndexedSeq[DNAString]) extends AnyVal {
  override def toString: String = value.toString
  def k: Int = value.head.value.length
}
