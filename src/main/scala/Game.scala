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

