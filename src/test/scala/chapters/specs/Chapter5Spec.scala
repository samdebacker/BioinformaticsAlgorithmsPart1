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

class Chapter5Spec extends FeatureSpec with Matchers{
  feature("dpChange") {
    scenario("example") {
      dpChange(40, Set(50,25,20,10,5,1)) shouldBe 2
    }
    scenario("extra dataset") {
      dpChange(8074, Set(24,13,12,7,5,3,1)) shouldBe 338
    }
    scenario("interactive quiz") {
      dpChange(17907, Set(24,21,20,18,13,12,5,3,1)) shouldBe 747
    }
  }
}
