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

package io.ireact.bioinformatics.part1.specs

import io.ireact.bioinformatics.part1.Chapter6._
import org.scalatest.{ FeatureSpec, Matchers }

import scala.io.Source

class Chapter6Spec extends FeatureSpec with Matchers {
  feature("greedySorting") {
    scenario("example") {
      val p = IndexedSeq(-3, +4, +1, +5, -2)
      val expectedResult = Seq(
        IndexedSeq(-1, -4, +3, +5, -2),
        IndexedSeq(+1, -4, +3, +5, -2),
        IndexedSeq(+1, +2, -5, -3, +4),
        IndexedSeq(+1, +2, +3, +5, +4),
        IndexedSeq(+1, +2, +3, -4, -5),
        IndexedSeq(+1, +2, +3, +4, -5),
        IndexedSeq(+1, +2, +3, +4, +5)
      )
      greedySorting(p) shouldBe expectedResult
    }

    def loadPermutations(fn: String): Seq[Permutation] = {
      val regex =
        """\(([+\-\d ]+)\)""".r
      Source.fromFile(fn).getLines().map {
        case regex(content) ⇒
          content.split(' ').toIndexedSeq.map(_.toInt)
      }.toSeq
    }

    scenario("extra dataset") {
      val p = "-340 -324 +233 -345 +21 +120 -366 -419 +105 -25 +356 -148 +279 +59 -110 +231 -375 +330 +199 +48 +122 -42 +236 -365 -26 +275 -60 -227 +259 -338 +183 -126 -223 -354 +93 -288 -221 -332 +219 +400 -350 +147 -286 +143 +161 -213 -234 -152 -17 -109 -116 -176 +131 -55 +323 +267 +285 +82 +287 -61 -207 -83 -1 -7 -188 +242 -78 -396 -392 +274 -258 -371 -309 +69 +45 +57 +314 +228 +373 -90 +159 +348 -239 -257 -136 -63 -144 +162 +104 +278 +406 +182 -170 -158 -169 +265 +399 +226 -201 +33 -79 +357 -118 +212 -343 +196 -154 +326 -89 +187 -291 -269 -240 +115 +405 -130 +195 +386 +310 +281 +420 +85 +172 +128 -16 +49 +404 -304 +376 +331 +168 +73 +282 +124 +302 -232 +121 +398 +355 -74 +146 -247 +165 -75 +362 +5 -395 -253 +41 -316 -205 -56 -24 -37 -262 +150 -250 -409 +283 -290 -64 +10 +378 -211 -43 -51 -65 -370 +235 -35 -377 -84 -353 -40 -34 -103 +125 +28 -422 +209 -335 +108 +294 +328 +6 +417 +38 -29 +36 -284 +206 +296 +300 -100 -312 -237 +166 -76 -384 -244 -208 +245 +397 -140 -401 +119 +261 +299 +292 +347 +194 +80 -62 +117 +9 +238 +214 +382 +321 +71 +307 +252 -218 +190 -308 -3 -98 +30 +361 +171 +341 -106 +295 +298 +352 +418 -13 -413 -387 -129 -351 -254 -319 -58 -318 -94 +315 +177 -87 -317 +391 +181 -180 +416 -132 +402 -163 +151 -408 +97 +142 -77 +46 +67 -27 -342 -364 -173 -210 +134 -388 -349 -359 +368 -276 +91 -339 +92 -264 +255 +193 -11 +385 +263 +230 -191 +241 -225 +268 -320 +272 -149 -81 -31 -139 -337 +164 -204 +19 -403 -123 -222 +289 +360 -306 -367 +248 +23 +39 +380 -260 +135 -224 -22 -107 -383 -246 -12 -369 +297 -344 -99 -305 -280 +44 -412 +113 -167 -156 +145 -336 -325 -101 +203 +53 +47 +50 +389 +293 +8 -137 +410 +346 -220 +186 -256 +249 +215 +153 +311 +141 -127 -72 -138 +411 -421 -390 -303 -358 -216 +192 -266 +333 +313 -277 -270 -32 -102 -189 -2 -68 -414 +329 -334 -271 -198 -114 -174 +393 -155 -327 -175 -273 +70 +95 +217 -301 -4 +52 -88 -18 +229 -54 -415 +184 +200 -363 -243 +202 -112 -251 -379 +178 -179 -157 +197 -160 +185 -381 -96 +372 -374 -15 +133 -111 -394 +20 -407 +14 +322 -86 +66"
        .split(' ').toIndexedSeq.map(_.toInt)
      val expectedResult = loadPermutations("src/main/resources/greedySortingExtraDatasetOutput.txt")
      greedySorting(p) shouldBe expectedResult
    }

    scenario("interactive quiz") {
      val p = "+33 +86 +102 -91 -26 +119 +46 -122 -65 -68 +1 -106 -113 +71 +84 -111 -18 -13 -53 +101 +21 -19 +83 -104 -110 +36 -72 +11 +97 +7 -23 -38 +95 -81 +89 -125 -51 -109 -115 +52 -120 +48 +16 -74 +67 -25 +2 +6 +79 +12 +60 +70 -10 -100 +50 +15 +29 -90 -39 +75 +58 -105 +66 -73 -118 +76 +17 +42 -123 +43 -34 +5 +37 -32 +64 -121 +8 +49 -87 +78 +54 -114 +117 -80 +35 +116 +40 -63 -28 -9 -45 +56 -61 -88 -99 +47 -22 -41 +92 +103 -3 -112 +4 +82 +96 -31 +57 +98 -124 -107 +24 +14 -44 -108 +20 +77 -69 -93 -55 -62 -30 -94 -85 +59 -27"
        .split(' ').toIndexedSeq.map(_.toInt)
      val expectedResult = loadPermutations("src/main/resources/greedySortingInteractiveQuizOutput.txt")
      greedySorting(p) shouldBe expectedResult
    }
  }
}
