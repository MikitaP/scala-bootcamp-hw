package adt

sealed trait TestCase
object TestCase {
  final class TexasCase(board: Board, hands: Set[Hand]) extends TestCase
  final class OmahaCase(board: Board, hands: Set[Hand]) extends TestCase
}
