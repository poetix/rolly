package rolly

case class Part[T, V](name: String = "") {
  def of(value: V) = this -> value
  def from(partsList: PartsList[T]): V = partsList.parts(this).asInstanceOf[V]
}

class PartsList[T](val parts: Map[Part[T, _], Any]) {
  def isEmpty = parts.isEmpty

  def +[V](part: (Part[T, V], V)) = new PartsList[T](parts + part)

  override def toString() = parts.toString
}

object PartsList {
  def apply[T](parts: Map[Part[T, _], Any]) = new PartsList[T](parts)
  def empty[T]() = new PartsList[T](Map.empty[Part[T, _], Any])
}