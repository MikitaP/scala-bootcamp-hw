package adt

sealed trait Suit

object Suit {
  final case object Hearts extends Suit
  final case object Spades extends Suit
  final case object Clubs extends Suit
  final case object Diamonds extends Suit
}
