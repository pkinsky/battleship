package com.pkinsky.battleship

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
    state.getOrElse(point, throw new Exception(s"invalid point $point for board $this"))

  private def range(a: Int, b: Int): Seq[Int] = {
    if (a < b) a to b else b to a
  }

  def place(s: ShipType, start: Point, end: Point): Board = {
    if (start.x == end.x && Math.abs(start.y - end.y) + 1 == s.size){
      val points = range(start.y, end.y).map{ y =>
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
      val points = range(start.x, end.x).map{ x =>
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
