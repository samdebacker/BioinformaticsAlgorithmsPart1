package specs

import org.scalatest._

import scala.collection.immutable.IndexedSeq

@DoNotDiscover
class FullSpec extends Spec {
  override def nestedSuites = IndexedSeq(
    new Chapter1Spec,
    new Chapter2Spec,
    new Chapter3Spec,
    new Chapter4Spec,
    new Chapter5Spec
  )
}
