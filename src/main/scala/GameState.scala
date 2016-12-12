package com.pkinsky.battleship


case class GameState(
  boards: Map[Player, Board],
  turn: Player = A
)

object GameState{
  def playTurn(state: GameState)(target: Point): (MoveResult, GameState) = {
    val targetPlayer = if (state.turn == A) B else A
    val targetBoard = state.boards.getOrElse(targetPlayer, throw new Exception(s"board for $targetPlayer not found"))
    val targetSquare = targetBoard.state.getOrElse(target, throw new Exception(s"square at $target not found"))

    if (targetSquare.hit){//already hit, invalid
      (AlreadyTaken, state) //state does not change
    } else {
      val updatedSquare = targetSquare.copy(hit = true)
      val updatedBoard = targetBoard.copy(state = targetBoard.state.updated(target, updatedSquare))
      val updatedState = state.copy(turn = targetPlayer, boards = state.boards.updated(targetPlayer, updatedBoard))

      targetSquare.ship match {
        case Some(ship) if updatedBoard.allShipsDead => (Win, updatedState)
        case Some(ship) if ship.isDead(updatedBoard) => (Sunk, updatedState)
        case Some(ship) => (Hit, updatedState)
        case None => (Miss, updatedState)
      }
    }
  }
}
