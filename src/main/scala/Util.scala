package main
/**
 * Created by saheb on 12/10/15.
 */

object Util {

  def getBoard(p: Piece , board: List[List[Char]]): List[List[Char]] = {
    val row = board(p.x)
    val newRow = row.splitAt(p.y)._1 ++ List(p.token) ++ row.splitAt(p.y+1)._2
    val newBoard = board.splitAt(p.x)._1 ++ List(newRow) ++ board.splitAt(p.x+1)._2
    newBoard
  }

  def time[R](block: => Unit): Long = {
    val t0 = System.nanoTime()
    block
    val t1 = System.nanoTime()
    t1 - t0
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

