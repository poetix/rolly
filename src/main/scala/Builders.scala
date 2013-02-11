package rolly

import scalaz.Scalaz._
import scalaz._

trait Reifiable[V] {
  def reify(requisites: Requisites): V
}

trait Builder[T] {
  def build(parts: PartsList[T]): T
}

case class Concrete[V](value: V) extends Reifiable[V] {
  def reify(requisites: Requisites) = value
}

case class Reifiables[T](reifiables: Map[Part[T, _], Reifiable[_]] = Map.empty[Part[T, _], Reifiable[_]]){

  def reifiableFor[V](part: Part[T, V]) = reifiables(part).asInstanceOf[Reifiable[V]]

  def toPartsList(requisites: Requisites) = {
    val reified = reifiables.map { case (part, reifiable) =>
      (part -> reifiable.reify(requisites))
    }.toMap[Part[T, _], Any]
    PartsList(reified)
  }

  def +[V](partToReifiable: (Part[T, V], Reifiable[V])) = Reifiables(reifiables + partToReifiable)
}

sealed case class Requisites(requisites: Map[BuildTarget[_], Reifiables[_]] = Map.empty[BuildTarget[_], Reifiables[_]]) {

  def partsListFor[T](target: BuildTarget[T]) =
    requisites(target).asInstanceOf[Reifiables[T]].toPartsList(this)

  def reifiablesFor[T](target: BuildTarget[T]) = requisites(target).asInstanceOf[Reifiables[T]]

  def add[T, V](target: BuildTarget[T], part:Part[T, V], reifyable: Reifiable[V]) = {
    val updated = reifiablesFor(target) + (part -> reifyable)
    Requisites(requisites.updated(target, updated))
  }

  def reifiableFor[T, V](target: BuildTarget[T], part: Part[T, V]): Reifiable[V] =
    reifiablesFor(target).reifiableFor(part)

  def valueFor[T, V](target: BuildTarget[T], part: Part[T, V]) = reifiableFor(target, part).reify(this)

  def withTarget[T](target: BuildTarget[T]) = new Requisites(requisites + (target -> Reifiables()))
}

trait Accessor[V] {
  def :=(value: V): State[Requisites, V]
  def :=(reifiable: Reifiable[V]): State[Requisites, Reifiable[V]]
}

sealed case class BuildTarget[T](builder: Builder[T]) extends Reifiable[T] {
  val target = this
  def /[V](part: Part[T, V]) = new Accessor[V] {
    override def :=(value: V)  = state[Requisites, V] { requisites =>
      (requisites.add(target, part, new Concrete[V](value)), value)
    }
    override def :=(reifiable: Reifiable[V]) = state[Requisites, Reifiable[V]] { requisites =>
      (requisites.add(target, part, reifiable), reifiable)
    }
  }

  override def reify(requisites: Requisites) = {
    val partsList = requisites.partsListFor(this)
    builder.build(partsList)
  }
}

object Builders {

  def builder[T](implicit builder: Builder[T]) = state[Requisites, BuildTarget[T]] { requisites =>
    val target = BuildTarget[T](builder)
    (requisites.withTarget(target), target)
  }

  def buildFrom[T](state: State[Requisites, BuildTarget[T]]) = {
    val (requisites, target) = state(Requisites())
    target.reify(requisites)
  }
}