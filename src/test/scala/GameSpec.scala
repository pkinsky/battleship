package com.pkinsky.battleship

import org.scalatest._

class GameSpec extends FlatSpec with Matchers {


  val startingBoard1 =
    Board.default
      .place(ShipType.Carrier, Point(0,0), Point(4,0))

  val startingBoard2 =
    Board.default
      .place(ShipType.Carrier, Point(0,0), Point(4,0))
      .place(ShipType.Carrier, Point(0,1), Point(4,1))

  "A Game" should "detect misses" in {
    val game = GameState(Map(A -> startingBoard1, B -> startingBoard1), A)

    val (moveRes, game1) = GameState.playTurn(game)(Point(1,1))

    moveRes should be (Miss)
  }

  "A Game" should "detect hits" in {
    val game = GameState(Map(A -> startingBoard1, B -> startingBoard1), A)

    val (moveRes, game1) = GameState.playTurn(game)(Point(0,0))

    moveRes should be (Hit)
  }

  "A Game" should "detect sinking a ship" in {
    val game = GameState(Map(A -> startingBoard2, B -> startingBoard2), A)

    val (moveRes, _) =
      (0 to 4).foldLeft[(MoveResult, GameState)]((Miss, game)){ (state, n) =>
        val playerA =
          GameState //first player A moves
            .playTurn(state._2)(Point(n, 0))

        GameState //then player B
          .playTurn(playerA._2)(Point(n, 0))
      }

    moveRes should be (Sunk)
  }

  "A Game" should "detect sinking a player's last ship" in {
    val game = GameState(Map(A -> startingBoard2, B -> startingBoard1), A)

    val (moveRes, _) =
      (0 to 4).foldLeft[(MoveResult, GameState)]((Miss, game)){ (state, n) =>
        val playerA =
          GameState //first player A moves
           .playTurn(state._2)(Point(n, 0))

        GameState //then player B
          .playTurn(playerA._2)(Point(n, 0))
      }

    moveRes should be (Sunk)
  }
}


