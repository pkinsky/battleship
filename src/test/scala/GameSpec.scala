package com.pkinsky.battleship

import org.scalatest._

class BoardSpec extends FlatSpec with Matchers {

  "A Board" should "allow ships to be added vertically and horizontally" in {
    Board.default
      .place(ShipType.Carrier, Point(0,0), Point(4,0))
      .place(ShipType.Battleship, Point(0,1), Point(3,1))
      .place(ShipType.Submarine, Point(0,2), Point(2,2))
      .place(ShipType.Cruiser, Point(0,3), Point(2,3))
      .place(ShipType.Destroyer, Point(7,6), Point(7,7)) //note: if # in end smaller than # in start, fails

  }

  it should "throw Exception if placed in overlapping configuration" in {
    val board =
      Board.default
        .place(ShipType.Carrier, Point(0,0), Point(4,0))
    a [Exception] should be thrownBy {
       board.place(ShipType.Destroyer, Point(0,0), Point(1,0))
    }
  }
}