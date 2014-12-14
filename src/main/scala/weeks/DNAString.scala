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

object DNAString {
  @inline def from(value: String): Option[DNAString] = {
    if (isValid(value)) Some(new DNAString(value.trim.toUpperCase)) else None
  }
  private[this] val dnaString = """(?i)[ACGT]+""".r
  private[weeks] def isValid(value: String): Boolean = value.trim match {
    case dnaString(_*) ⇒ true
    case _             ⇒ false
  }

  import scala.language.experimental.macros
  import scala.language.implicitConversions
  implicit def apply(value: String): DNAString = macro applyMacro
  def applyMacro(c: Context)(value: c.Expr[String]): c.Tree = {
    import c.universe._
    value.tree match {
      case Literal(stringConst) ⇒
        val literalValue = stringConst.value.toString
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, "DNAString can only contain nucleotides A, C, G or T")
      case _ ⇒
        c.abort(c.enclosingPosition, "DNAString macro only works on String Literals, use DNAString.form(String) instead.")
    }
    // Pitty we cannot access the private constructor here
    q"DNAString.from(${value}).get"
  }

  implicit class StringToDNAString(val s: String) extends AnyVal {
    def toDNA: DNAString = from(s).get
  }
}

final class DNAString private[weeks] (val value: String) extends AnyVal {
  override def toString: String = value.toString
}
