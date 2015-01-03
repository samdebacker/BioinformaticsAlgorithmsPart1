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

import scala.util.matching.Regex

object DNAString {
  @inline def from(value: String): Option[DNAString] = {
    if (isValid(value)) Some(new DNAString(value.trim.toUpperCase)) else None
  }

  // We could hide it in an inner factory class so working around it makes it even more difficult,
  // but no guarantees, so maybe better clearly mark it as unsafe
  //class UnsafeFrom {
  /**
   * /!\ For macro use, do not call this method directly /!\
   */
  @inline /*protected*/ def unsafeFrom(value: String): DNAString = new DNAString(value.trim.toUpperCase)
  //}

  private[this] val dnaString: Regex = """(?i)[ACGT]+""".r
  private[weeks] def isValid(value: String): Boolean = value.trim match {
    case dnaString(_*) ⇒ true
    case _             ⇒ false
  }

  import scala.language.experimental.macros
  import scala.language.implicitConversions
  implicit def apply(value: String): DNAString = macro DNAStringMacro.applyMacro

  implicit val dnaStringOrdering = Ordering.by { (dnaString: DNAString) ⇒ dnaString.value }
}

final class DNAString private[weeks] (val value: String) extends AnyVal {
  override def toString: String = value.toString
}
