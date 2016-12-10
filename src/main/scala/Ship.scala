package com.pkinsky.battleship


case class Ship(points: Seq[Point], shipType: ShipType){
  assert(points.size == shipType.size, s"ship should occupy number of points equal to size,\n\t$this")

  //return true if all points on ship have been hit on a given board
  def isDead(board: Board): Boolean = points.forall{ p =>
    board.state.get(p).map{ s => s.hit }.getOrElse(true)
  }
}

sealed trait ShipType {
  val size: Int
}
case object ShipType {
  val startingTypes = Set(Carrier, Battleship, Cruiser, Submarine, Destroyer)

  case object Carrier extends ShipType { val size = 5 }
  case object Battleship extends ShipType { val size = 4 }
  case object Cruiser extends ShipType { val size = 3 }
  case object Submarine extends ShipType { val size = 3 }
  case object Destroyer extends ShipType { val size = 2 }
}

