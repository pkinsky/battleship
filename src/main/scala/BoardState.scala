package com.pkinsky.battleship

/*
Data structures representing battleship boards and ships
*/

object Board{
  def create(x: Int, y: Int): Board = {
    val state = (0 until x).flatMap{ x =>
      (0 until y).map { y =>
        val p = Point(x,y)
        (p, Square(p, None, false))
      }
    }.toMap

    Board(state, Set.empty)
  }


  val default: Board = create(8,8)
}

case class Board(state: Map[Point, Square], ships: Set[Ship]){
  def allShipsDead: Boolean = ships.forall(_.isDead(this))

  def getSquare(point: Point): Square = 
    state.get(point).getOrElse(throw new Exception(s"invalid point $point for board $this"))

  def place(s: ShipType, start: Point, end: Point): Board = {
    if (start.x == end.x && Math.abs(start.y - end.y) + 1 == s.size){
      val points = (start.y to end.y).map{ y =>
          Point(start.x, y)
        }

      val ship = Ship(points, s)

      val newState = points.foldLeft(state)( (acc, point) =>
        acc.updated(point, getSquare(point).copy(ship = Some(ship)))
      )

      points.find( p => getSquare(p).ship.isDefined ).foreach{ ship =>
        throw new Exception(s"invalid ship placement for ($s size = ${s.size}, $start -> $end),\n\tship overlaps with ship $ship")
      }

      Board(newState, ships ++ Set(ship))
    } else if (start.y == end.y && Math.abs(start.x - end.x) + 1 == s.size){
      val points = (start.x to end.x).map{ x =>
          Point(x, start.y)
        }

      val ship = Ship(points, s)

      val newState = points.foldLeft(state)( (acc, point) =>
        acc.updated(point, getSquare(point).copy(ship = Some(ship)))
      )

      points.find( p => getSquare(p).ship.isDefined ).foreach{ ship =>
        throw new Exception(s"invalid ship placement for ($s size = ${s.size}, $start -> $end),\n\tship overlaps with ship $ship")
      }

      Board(newState, ships ++ Set(ship))
    } else {
      throw new Exception(s"invalid ship placement for ($s size = ${s.size}, $start -> $end)")
    }
  }
}

case class Square(point: Point, ship: Option[Ship], hit: Boolean = false)
case class Point(x: Int, y: Int)

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

