package adt

sealed trait Hand

object Hand {
  final case class TexasHand(card1: Card, card2: Card) extends Hand
  final case class OmahaHand(card1: Card, card2: Card, card3: Card, card4: Card)
      extends Hand
}
