package chess

import scala.collection.mutable

/**
 * Created by saheb on 12/10/15.
 */

object Util {

  def getUniquePermutations(tokens: List[Char]): List[List[Char]] = {
    val permutations = mutable.Set.empty[List[Char]]
    tokens.permutations.foreach(items => {
      permutations.add(items)
    })
    tokens.permutations.foreach(entry =>
      if (permutations.contains(entry.reverse) && entry != entry.reverse) permutations.remove(entry))
    permutations.toList
  }

  def getBoard(p: Piece , board: List[List[Char]]): List[List[Char]] = {
    val row = board(p.x)
    val newRow = row.splitAt(p.y)._1 ++ List(p.token) ++ row.splitAt(p.y+1)._2
    val newBoard = board.splitAt(p.x)._1 ++ List(newRow) ++ board.splitAt(p.x+1)._2
    newBoard
  }


 def createPiece(x:Int, y:Int, token:Char): Piece = {
    token match {
      case 'K' => King(x,y)
      case 'Q' => Queen(x,y)
      case 'R' => Rook(x,y)
      case 'B' => Bishop(x,y)
      case 'N' => Knight(x,y)
      case '*' => Empty(x,y)
    }
  }

  def time[R](block: => Int): Result = {
    val t0 = System.nanoTime()
    val solutions = block
    val t1 = System.nanoTime()
    Result(t1 - t0, solutions)
  }

  def printBoard(board: List[List[Char]], solutionNum: Int) = {
    println(s"Solution #$solutionNum")
    println(board map (_ mkString " | ") mkString "\n")
    println()
  }

  def printBoardReflection(board: List[List[Char]], solutionNum: Int) = {
    println(s"Solution #$solutionNum")
    println(board reverseMap (_ mkString " | ") mkString "\n")
    println()
  }

  def difference(a: Int, b: Int)  = math.abs(a-b)

  def inc(a: Int) = a + 1

  def dec(a: Int) = a - 1
}

