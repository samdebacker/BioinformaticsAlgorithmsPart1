import org.scalatest._
import specs._

import scala.collection.immutable.IndexedSeq

@DoNotDiscover
class FullSpec extends Spec {
  override def nestedSuites = IndexedSeq(
    new Week1Spec,
    new Week2Spec,
    new Week3Spec,
    new Week4Spec
  )
}
