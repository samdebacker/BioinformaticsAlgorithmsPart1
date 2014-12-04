import org.scalatest.Matchers._
import org.scalatest._

class Week1Spec extends FeatureSpec {
  import Week1._

  feature("neighbours") {
    scenario("example") {
      val pattern = "GAGTTGAAGCA"
      val d =  3
      val expectedResult = 4984
      neighbours(pattern, d).size shouldBe expectedResult
    }
  }
}