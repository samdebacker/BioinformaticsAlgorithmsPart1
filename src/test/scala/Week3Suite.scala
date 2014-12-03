import org.scalatest.Matchers._
import org.scalatest._

class Week3Suite extends FunSuite {
  import Week3._

  test("Expected number of occurences") {
    expectedNumberOfOccurences(9, 1000, 500) should equal (1.8921 +- 0.0001)
  }
}
