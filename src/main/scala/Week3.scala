import scala.math._

object Week3 {
  def expectedNumberOfOccurences(k: Int, L: Int, n: Int): Double = (L - k + 1) * pow(.25, k) * n
}
