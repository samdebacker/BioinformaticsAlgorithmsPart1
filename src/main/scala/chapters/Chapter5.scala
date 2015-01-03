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

package chapters

import scala.collection.immutable.ListMap

object Chapter5 {
  def dpChange(money: Int, coins: Set[Int]):Int = {
    val l = (0 to money).map { m ⇒
      (m → (if (m == 0) 0 else Int.MaxValue))
    }
    val minNumCoins = ListMap(l: _*)
    val result = minNumCoins.tail.foldLeft(minNumCoins) { case (minNumCoins, (m, _)) ⇒
      coins.foldLeft(minNumCoins) { (minNumCoins, c) ⇒
        if (m >= c && (minNumCoins(m - c) + 1 < minNumCoins(m)))
          minNumCoins.updated(m, minNumCoins(m - c) + 1)
        else
          minNumCoins
      }
    }
    result(money)
  }
}
