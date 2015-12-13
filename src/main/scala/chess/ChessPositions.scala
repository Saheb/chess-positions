package chess

/**
 * Created by saheb on 11/17/15.
 */

import java.io._

import Util._

import scala.annotation.tailrec

case class LastPlaced(board: List[List[Char]], x: Int, y: Int, placedPieces: List[Piece])
case class LastDisplaced(board: List[List[Char]], x: Int, y: Int, index: Int, placedPieces: List[Piece])
case class Result(time: Long, solutions: Int)

class ChessPositions(val m: Int, val n: Int, val tokens: List[Char]) {

  private def isSafe(p: Piece, tokens: List[Piece]) : Boolean = {
    val attacking = tokens exists(piece => p.attacks(piece))
    val beingAttacked = tokens exists(piece => piece.attacks(p))
    !(attacking || beingAttacked) // Safe = Neither attacking nor being attacked.
  }

  @tailrec
  private def place(p: Piece , board: List[List[Char]], placedPieces: List[Piece]): LastPlaced = {
    if(isSafe(p,placedPieces))
    {
      LastPlaced(getBoard(p,board),p.x,p.y, placedPieces :+ p)
    }
    else
    {
      if(p.y+1 < n)
        place(p.cloneWithLoc(p.x,p.y+1),board,placedPieces)
      else if(p.x+1 < m)
        place(p.cloneWithLoc(p.x+1,0),board,placedPieces)
      else
        LastPlaced(List.empty[List[Char]],-1,-1,placedPieces)
    }
  }

  @tailrec
  private def displace(index: Int, tokens: List[Char],board: List[List[Char]],
                       placedPieces: List[Piece]): LastDisplaced = {
    if(placedPieces.isEmpty)
      LastDisplaced(board,m,n,index,placedPieces)
    else
    {
      val entry = placedPieces.last
      val displacedBoard = getBoard(Empty(entry.x,entry.y),board)
      if(entry.y + 1 >= n && entry.x + 1 >= m)
        displace(index-1,tokens,displacedBoard,placedPieces.dropRight(1))
      else
        LastDisplaced(displacedBoard,entry.x,entry.y,index,placedPieces.dropRight(1))
    }
  }

  @tailrec
  private def solve(i: Int, j: Int,index: Int, tokens: List[Char], board: List[List[Char]],
                     placedPieces: List[Piece], numOfSolutions: Int): Int = {
    val LastPlaced(newBoard, lastX, lastY, newlyPlaced) = place(createPiece(i,j,tokens(index)),board,placedPieces)
    if(newBoard.nonEmpty)
    {
        if(newlyPlaced.size == tokens.size)
        {
          printBoard(newBoard,numOfSolutions+1)
          val isNotReflection = tokens != tokens.reverse
          if(isNotReflection)
          {
            printBoardReflection(newBoard,numOfSolutions+2)
          }
          // backtrack
          val nextSolution = if(isNotReflection) numOfSolutions+2 else numOfSolutions+1
          val LastDisplaced(displacedBoard,dX,dY,dIndex, newlyDisplaced) = displace(index,tokens,board,newlyPlaced)
          if(dY+1 < n)
            solve(dX,dY+1,dIndex,tokens,displacedBoard,newlyDisplaced,nextSolution)
          else if(dX+1 < m)
            solve(dX+1,0,dIndex,tokens,displacedBoard,newlyDisplaced,nextSolution)
          else
            nextSolution
        }
        else
        {
          if(lastY +1 < n)
            solve(lastX,lastY+1,index+1,tokens,newBoard,newlyPlaced,numOfSolutions)
          else if(lastX+1 < m)
            solve(lastX+1,0,index+1,tokens,newBoard,newlyPlaced,numOfSolutions)
          else // backtrack
          {
            val LastDisplaced(displacedBoard,dX,dY,dIndex,newlyDisplaced) = displace(index,tokens,board,newlyPlaced)
            if(dY+1 < n)
              solve(dX,dY+1,dIndex,tokens,displacedBoard,newlyDisplaced,numOfSolutions)
            else if(dX+1 < m)
              solve(dX+1,0,dIndex,tokens,displacedBoard, newlyDisplaced,numOfSolutions)
            else
              numOfSolutions
          }
        }
    }
    else // backtrack
    {
      val LastDisplaced(displacedBoard,dX,dY,dIndex,newlyDisplaced) = displace(index-1,tokens,board,newlyPlaced)
      if(dY+1 < n)
        solve(dX,dY+1,dIndex,tokens,displacedBoard, newlyDisplaced,numOfSolutions)
      else if(dX+1 < m)
        solve(dX+1,0,dIndex,tokens,displacedBoard, newlyDisplaced,numOfSolutions)
      else
        numOfSolutions
    }
  }

  def run(writeToFile: Boolean = false): Int = {
    val result = time {
      val permutations = getUniquePermutations(tokens)
      val solutions = permutations.par.map(
        items => {
          solve(0, 0, 0, items, List.fill(m, n)('*'), List.empty[Piece], 0)
        }
      )
      solutions.sum
    }

    val totalSolutions = result.solutions

    val seconds: Double = result.time / 1000000000
    val minutes: Double = seconds / 60

    val numOfCores = Runtime.getRuntime.availableProcessors()

    if (writeToFile) {
      val pw = new PrintWriter(new File("Stats.txt"))
      pw.write(s"Stats are gathered by running the program on Intellij Idea 14.1.4\n")
      pw.write(s"Number of Cores used to perform this task = $numOfCores\n")
      pw.write(s"Dimensions of the board are $m rows and $n columns\n")
      pw.write(s"Tokens for the problem are $tokens\n")
      pw.write(s"Number of Solutions: $totalSolutions\n")
      pw.write(s"Time taken in seconds: $seconds\n")
      pw.write(s"Time taken in minutes: ${math.round(minutes)}")
      pw.close()
    }

    println(s"Number of Cores used to perform this task = $numOfCores")
    println(s"Dimensions of the board are $m rows and $n columns")
    println(s"Tokens for the problem are $tokens")
    println(s"Number of Solutions: $totalSolutions")
    println(s"Time taken in seconds: $seconds")
    println(s"Time taken in minutes: ${math.round(minutes)}")
    totalSolutions
  }
}

object ChessPositions extends App {
  val in = new java.util.Scanner(System.in)
  println("Dimensions of board M N?")
  val M = in.nextInt()
  val N = in.nextInt()
  println("Number of Kings(K)?")
  val k = in.nextInt()
  println("Number of Queens(Q)?")
  val q = in.nextInt()
  println("Number of Knights(N)?")
  val n = in.nextInt()
  println("Number of Bishops(B)?")
  val b = in.nextInt()
  println("Number of Rooks(R)?")
  val r = in.nextInt()
  val tokens = Seq.fill(k)('K') ++ Seq.fill(q)('Q') ++ Seq.fill(n)('N') ++ Seq.fill(b)('B') ++ Seq.fill(r)('R')
  val runner = new ChessPositions(M,N,tokens.toList)
  runner.run()//  runner.run(true) to write results to Stats.txt
}
