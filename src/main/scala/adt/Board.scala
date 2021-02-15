package adt

final case class Board(cards: Set[Card])

object Board {
  def create(cards: Set[Card]): Option[Board] = {
    if (cards.size == 5) Some(Board(cards))
    else None
  }
}