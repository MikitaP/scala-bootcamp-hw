package adt

sealed trait Combination

object Combination {
  final object StraightFlush extends Combination
  final object FourOfAKind extends Combination
  final object FullHouse extends Combination
  final object Flush extends Combination
  final object Straight extends Combination
  final object ThreeOfAKind extends Combination
  final object TwoPair extends Combination
  final object Pair extends Combination
  final object HighCard extends Combination
}
