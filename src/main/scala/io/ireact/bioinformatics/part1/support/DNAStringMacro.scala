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

object DNAStringMacro {
  def applyMacro(c: Context)(value: c.Expr[String]): c.Expr[DNAString] = {
    import c.universe._
    value.tree match {
      case Literal(stringConst) ⇒
        val literalValue = stringConst.value.toString
        if (!DNAString.isValid(literalValue))
          c.abort(c.enclosingPosition, "DNAString can only contain nucleotides A, C, G or T")
      case _ ⇒
        c.abort(c.enclosingPosition, "DNAString macro only works on String Literals, use DNAString.from(String) instead.")
    }
    // As state by Eugene Burmako in http://stackoverflow.com/questions/19170137/scala-using-private-constructor-in-a-macro
    // we cannot call a private method, so we do it via an unattractive method, called unsafe.
    // Didn't use the vampire method approach, that would loose the AnyVal benefit!
    reify {
      DNAString.unsafeFrom(value.splice)
    }
    // If using UnsafeFrom this is a way to make it happen:
    // q"""
    //     class UnsafeI extends chapters.DNAString.UnsafeFrom {
    //       def unsafeFromI(value: String): DNAString = unsafeFrom(value)
    //     }
    //     (new UnsafeI).unsafeFromI($value)
    // """
  }

  def dnaMacro(c: Context)(args: c.Tree*): c.Expr[DNAString] = {
    import c.universe._
    println("VALUE: " + args)
    //val value = sc.parts.mkString // CANNOT GET THE StringContext sc here /!\
    reify {
      // FIXME Can we fix this?
      DNAString.from("ACGT").get
    }
  }
}
