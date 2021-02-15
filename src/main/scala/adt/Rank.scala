package adt

sealed trait Rank

object Rank {
  final case object Two extends Rank
  final case object Three extends Rank
  final case object Four extends Rank
  final case object Five extends Rank
  final case object Six extends Rank
  final case object Seven extends Rank
  final case object Eight extends Rank
  final case object Nine extends Rank
  final case object Ten extends Rank
  final case object Jack extends Rank
  final case object Queen extends Rank
  final case object King extends Rank
  final case object Ace extends Rank
}
