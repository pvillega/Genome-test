
package default

package object genome {

  type Traversable[+A] = scala.collection.immutable.Traversable[A]
  type Iterable[+A]    = scala.collection.immutable.Iterable[A]
  type Seq[+A]         = scala.collection.immutable.Seq[A]
  type IndexedSeq[+A]  = scala.collection.immutable.IndexedSeq[A]
}
