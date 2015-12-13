package chess

import Util._

trait Piece {
  val x: Int
  val y: Int
  val token: Char
  def attacks(p: Piece) : Boolean
  def cloneWithLoc(x:Int, y:Int): Piece
}

case class King(x: Int, y: Int) extends Piece {
  val token = 'K'
  override def attacks(p: Piece) = {
    difference(x, p.x) <= 1 && difference(y,p.y) <= 1
  }
  override def cloneWithLoc(x:Int, y:Int): Piece = this.copy(x,y)
}

case class Queen(x: Int, y: Int) extends Piece {
  val token = 'Q'
  override def attacks(p: Piece) = {
    (difference(x, p.x) == 0 || difference(y,p.y) == 0) || difference(x, p.x) == difference(y,p.y)
  }
  override def cloneWithLoc(x:Int, y:Int): Piece = this.copy(x,y)
}

case class Rook(x: Int, y: Int) extends Piece {
  val token = 'R'
  override def attacks(p: Piece) = {
    difference(x, p.x) == 0 || difference(y,p.y) == 0
  }
  override def cloneWithLoc(x:Int, y:Int): Piece = this.copy(x,y)
}

case class Knight(x: Int, y: Int) extends Piece {
  val token = 'N'
  override def attacks(p: Piece) = {
    (difference(x, p.x) == 2 && difference(y,p.y) == 1) ||  (difference(x, p.x) == 1 && difference(y,p.y) == 2)
  }
  override def cloneWithLoc(x:Int, y:Int): Piece = this.copy(x,y)
}

case class Bishop(x: Int, y: Int) extends Piece {
  val token = 'B'
  override def attacks(p: Piece) = {
    difference(x, p.x) == difference(y,p.y)
  }
  override def cloneWithLoc(x:Int, y:Int): Piece = this.copy(x,y)
}
case class Empty(x: Int, y: Int) extends Piece {
  val token = '*'
  override def attacks(p: Piece) = false
  override def cloneWithLoc(x:Int, y:Int): Piece = this.copy(x,y)
}