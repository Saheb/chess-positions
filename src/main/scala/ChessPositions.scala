package main

/**
 * Created by saheb on 11/17/15.
 */

import java.io._

import scala.annotation.tailrec
import scala.collection.mutable.{Stack,Set}

import main.Util._

case class Piece(x: Int, y: Int, token: Char)
case class LastPlaced(board: List[List[Char]], x: Int, y: Int)
case class LastDisplaced(board: List[List[Char]], x: Int, y: Int, index: Int)

class ChessPositions(val m: Int, val n: Int, val tokens: List[Char]) {

  private var solutions = 0
  private var counter = Array.fill(m,n)(0)
  private val tokenLoc = new Stack[Piece]()

  private def kingCtr(i: Int, j: Int, f: Int => Int) = {
    if(i + 1 < m) counter(i+1)(j) = f(counter(i+1)(j))
    if(j + 1 < n) counter(i)(j+1) = f(counter(i)(j+1))
    if(i-1 >=0) counter(i-1)(j) = f(counter(i-1)(j))
    if(j-1 >=0) counter(i)(j-1) = f(counter(i)(j-1))
    if(i+1 < m && j+1 < n) counter(i+1)(j+1) = f(counter(i+1)(j+1))
    if(i-1 >=0 && j-1 >= 0) counter(i-1)(j-1) = f(counter(i-1)(j-1))
    if(i+1 < m && j-1 >= 0) counter(i+1)(j-1) = f(counter(i+1)(j-1))
    if(i-1 >= 0 && j+1 < n) counter(i-1)(j+1) = f(counter(i-1)(j+1))
  }

  private def knightCtr(i: Int, j: Int, f: Int => Int) = {
    if(i + 2 < m && j + 1 < n)
      counter(i+2)(j+1) = f(counter(i+2)(j+1))
    if(i + 2 < m && j - 1 >= 0)
      counter(i+2)(j-1) = f(counter(i+2)(j-1))
    if(i-2 >= 0 && j-1 >=0)
      counter(i-2)(j-1) = f(counter(i-2)(j-1))
    if(i-2 >=0 && j+1 < n)
      counter(i-2)(j+1) = f(counter(i-2)(j+1))
    if(i + 1 < m && j + 2 < n)
      counter(i+1)(j+2) = f(counter(i+1)(j+2))
    if(i + 1 < m && j - 2 >= 0)
      counter(i+1)(j-2) = f(counter(i+1)(j-2))
    if(i-1 >= 0 && j-2 >=0)
      counter(i-1)(j-2) = f(counter(i-1)(j-2))
    if(i-1 >=0 && j+2 < n)
      counter(i-1)(j+2) = f(counter(i-1)(j+2))
  }


  private def bishopCtr(i: Int, j: Int, f: Int => Int)= {
    for(a<-1 until math.min(m,n))
      {
        if(i+a < m && j+a < n)
        {
          counter(i+a)(j+a) = f(counter(i+a)(j+a))
        }
        
        if(i+a< m && j-a >= 0)
        {
          counter(i+a)(j-a) = f(counter(i+a)(j-a))
        }

        if(i-a >= 0 && j+a < n)
        {
          counter(i-a)(j+a) = f(counter(i-a)(j+a))
        }
        if(i-a >= 0 && j-a >= 0)
        {
          counter(i-a)(j-a) = f(counter(i-a)(j-a))
        }
      }
  }

  private def rookCtr(i: Int, j: Int, f: Int => Int) = {
    for(x <- 0 until j)
      counter(i)(x) = f(counter(i)(x))
    for(x <- j+1 until n)
      counter(i)(x) = f(counter(i)(x))
    for(y <- 0 until i)
      counter(y)(j) = f(counter(y)(j))
    for(y <- i+1 until m)
      counter(y)(j) = f(counter(y)(j))
  }

  private def notAttacking(p: Piece) : Boolean = {

    def kingAttackFn(entry: Piece): Boolean = {
      difference(entry.x, p.x) <= 1 && difference(entry.y,p.y) <= 1
    }

    def rookAttackFn(entry: Piece): Boolean = {
      difference(entry.x, p.x) == 0 || difference(entry.y,p.y) == 0
    }

    def knightAttackFn(entry: Piece): Boolean = {
      (difference(entry.x, p.x) == 2 && difference(entry.y,p.y) == 1) ||  (difference(entry.x, p.x) == 1 && difference(entry.y,p.y) == 2)
    }

    def bishopAttachFn(entry: Piece): Boolean = {
      difference(entry.x, p.x) == difference(entry.y,p.y)
    }

    p.token match {
      case 'K' =>
        !(tokenLoc exists kingAttackFn)
      case 'Q' =>
        !((tokenLoc exists rookAttackFn) || (tokenLoc exists bishopAttachFn))
      case 'R' =>
        !(tokenLoc exists rookAttackFn)
      case 'N' =>
        !(tokenLoc exists knightAttackFn)
      case 'B' =>
        !(tokenLoc exists bishopAttachFn)
      case  _ =>
        println("This should never be printed!")
        false
    }
  }

  private def updateCtr(p: Piece)(f: (Int) => Int) = {
    p.token match {
      case 'K' =>
        kingCtr(p.x,p.y,f)
      case 'Q' =>
        bishopCtr(p.x,p.y,f)
        rookCtr(p.x,p.y,f)
      case 'R' =>
        rookCtr(p.x,p.y,f)
      case 'B' =>
        bishopCtr(p.x,p.y,f)
      case 'N' =>
        knightCtr(p.x,p.y,f)
      case  _ => println("This should never be printed!")
    }
  }

  @tailrec
  private def place(p: Piece , board: List[List[Char]]): LastPlaced = {
    if(counter(p.x)(p.y) == 0 && notAttacking(p))
    {
      tokenLoc.push(p)
      updateCtr(p)(inc)
      LastPlaced(getBoard(p,board),p.x,p.y)
    }
    else
    {
      if(p.y+1 < n)
        place(Piece(p.x,p.y+1,p.token),board)
      else if(p.x+1 < m)
        place(Piece(p.x+1,0,p.token),board)
      else
        LastPlaced(List.empty[List[Char]],-1,-1)
    }
  }

  @tailrec
  private def displace(index: Int, tokens: List[Char],board: List[List[Char]]): LastDisplaced = {
    if(tokenLoc.isEmpty)
      LastDisplaced(board,m,n,index)
    else
    {
      val entry = tokenLoc.pop()
      updateCtr(entry)(dec)
      val displacedBoard = getBoard(Piece(entry.x,entry.y,'*'),board)
      if(entry.y + 1 >= n && entry.x + 1 >= m)
        displace(index-1,tokens,displacedBoard)
      else
        LastDisplaced(displacedBoard,entry.x,entry.y,index)
    }
  }

  @tailrec
  private def solve(i: Int, j: Int,index: Int, tokens: List[Char], board: List[List[Char]]): Int = {
    val LastPlaced(newBoard, lastX, lastY) = place(Piece(i,j,tokens(index)),board)
    if(newBoard.nonEmpty)
    {
        if(tokenLoc.size == tokens.size)
        {
          solutions += 1
          printBoard(newBoard,solutions)
          if(tokens != tokens.reverse)
          {
            solutions += 1
            printBoardReflection(newBoard,solutions)
          }
          val LastDisplaced(displacedBoard,dX,dY,dIndex) = displace(index,tokens,board) // backtrack
          if(dY+1 < n)
            solve(dX,dY+1,dIndex,tokens,displacedBoard)
          else if(dX+1 < m)
            solve(dX+1,0,dIndex,tokens,displacedBoard)
          else
            -1
        }
        else
        {
          if(lastY +1 < n)
            solve(lastX,lastY+1,index+1,tokens,newBoard)
          else if(lastX+1 < m)
            solve(lastX+1,0,index+1,tokens,newBoard)
          else // backtrack
          {
            val LastDisplaced(displacedBoard,dX,dY,dIndex) = displace(index,tokens,board) // backtrack
            if(dY+1 < n)
              solve(dX,dY+1,dIndex,tokens,displacedBoard)
            else if(dX+1 < m)
              solve(dX+1,0,dIndex,tokens,displacedBoard)
            else
              -1
          }
        }
    }
    else // backtrack
    {
      val LastDisplaced(displacedBoard,dX,dY,dIndex) = displace(index-1,tokens,board)
      if(dY+1 < n)
        solve(dX,dY+1,dIndex,tokens,displacedBoard)
      else if(dX+1 < m)
        solve(dX+1,0,dIndex,tokens,displacedBoard)
      else
        -1
    }
  }

  def run(writeToFile: Boolean = false): Int = {
    val permutations = Set.empty[List[Char]]
    val timeTaken = time {
      tokens.permutations.foreach(items => {
        permutations.add(items)
      })
      tokens.permutations.foreach(entry =>
        if(permutations.contains(entry.reverse) && entry != entry.reverse) permutations.remove(entry))

      permutations.foreach(
        items => {
          counter = Array.fill(m,n)(0)
          solve(0,0,0,items,List.fill(m,n)('*'))
        }
      )
    }

    val seconds: Double = timeTaken/1000000000
    val minutes: Double = seconds/60

    if(writeToFile)
    {
      val pw = new PrintWriter(new File("Stats.txt" ))
      pw.write(s"Stats are gathered by running the program on Intellij Idea 14.1.4\n")
      pw.write(s"Dimensions of the board are $m rows and $n columns\n")
      pw.write(s"Tokens for the problem are $tokens\n")
      pw.write(s"Number of Solutions: $solutions\n")
      pw.write(s"Time taken in seconds: $seconds\n")
      pw.write(s"Time taken in minutes: ${math.round(minutes)}")
      pw.close()
    }

    println(s"Dimensions of the board are $m rows and $n columns")
    println(s"Tokens for the problem are $tokens")
    println(s"Number of Solutions: $solutions")
    println(s"Time taken in seconds: $seconds")
    println(s"Time taken in minutes: ${math.round(minutes)}")
    solutions
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
