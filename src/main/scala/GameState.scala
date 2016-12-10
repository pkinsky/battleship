package com.pkinsky.battleship

sealed trait Player
case object A extends Player
case object B extends Player

sealed trait MoveResult
case object Hit extends MoveResult
case object Miss extends MoveResult
case object AlreadyTaken extends MoveResult
case object Sunk extends MoveResult
case object Win extends MoveResult

case class PlayingGameState(
  boards: Map[Player, Board],
  turn: Player = A //player A goes first
)

object PlayingGameState{
  //player is player making move, opposite of target board
  def playTurn(state: PlayingGameState)(player: Player, target: Point): (MoveResult, PlayingGameState) = {
    val targetPlayer = if (player == A) B else A
    val targetBoard = state.boards.get(targetPlayer).getOrElse(throw new Exception("player not found"))
    val targetSquare = targetBoard.state.get(target).getOrElse(throw new Exception("square not found"))

    if (targetSquare.hit){//already hit, invalid
      (AlreadyTaken, state) //state does not change
    } else targetSquare.ship match {
      case None =>
        val updatedSquare = targetSquare.copy(hit = true)
        val updatedBoard = targetBoard.copy(state = targetBoard.state.updated(target, updatedSquare))
        val updatedState = state.copy(turn = targetPlayer, boards = state.boards.updated(targetPlayer, updatedBoard))
        (Miss, updatedState)

      case Some(ship) =>
        val updatedSquare = targetSquare.copy(hit = true)
        val updatedBoard = targetBoard.copy(state = targetBoard.state.updated(target, updatedSquare))
        val updatedState = state.copy(turn = targetPlayer, boards = state.boards.updated(targetPlayer, updatedBoard))

        if (updatedBoard.allShipsDead) (Win, updatedState)
        else if (ship.isDead(updatedBoard)) (Sunk, updatedState)
        else (Hit, updatedState)
    }
  }
}
