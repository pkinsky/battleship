sealed trait Player
case object A extends Player
case object B extends Player

sealed trait ShipType {
  val size: Int
}
case object ShipType {
  val startingTypes = Set(Carrier, Battleship, Cruiser, Submarine, Destroyer)
}

case object Carrier extends ShipType { val size = 5 }
case object Battleship extends ShipType { val size = 4 }
case object Cruiser extends ShipType { val size = 3 }
case object Submarine extends ShipType { val size = 3 }
case object Destroyer extends ShipType { val size = 2 }

case class Point(x: Int, y: Int)
case class Square(point: Point, ship: Option[Ship], hit: Boolean = false)

case class Ship(points: Set[Point], shipType: ShipType){
  assert(points.size == shipType.size, "ship should occupy number of points equal to size")

  //return true if all points on ship have been hit on a given board
  def isDead(board: Board): Boolean = points.forall{ p =>
    board.state.get(p).map{ s => s.hit }.getOrElse(true)
  }
}


case class Board(state: Map[Point, Square], ships: Set[Ship]){
  def allShipsDead: Boolean = ships.forall(_.isDead(this))
}

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


//PROBLEM: two phases of game, need to represent both
//PROBLEM: also I'm supposed to be able to finish this in an hour so I'm not sure...
//you know what? I got different idea from meeting today,
//I think, today I got more of a 'just build your version of the game battleship and show it to me, not do it on collabedit. should clarify that'
