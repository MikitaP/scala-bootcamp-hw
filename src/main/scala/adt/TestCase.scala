package adt

import adt.Hand.{OmahaHand, TexasHand}

sealed trait TestCase
object TestCase {
  final class TexasCase(board: Board, hands: Set[TexasHand]) extends TestCase
  final class OmahaCase(board: Board, hands: Set[OmahaHand]) extends TestCase
}
