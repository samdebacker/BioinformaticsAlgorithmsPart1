import org.scalatest._
import specs._

import scala.collection.immutable.IndexedSeq

class FullSpec extends Spec {
  override def nestedSuites = IndexedSeq(
    new Week1Spec,
    new Week2Spec,
    new Week3Spec
  )
}
