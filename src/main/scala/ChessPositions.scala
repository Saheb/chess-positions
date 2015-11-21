package main

/**
 * Created by saheb on 11/17/15.
 */

import java.io._

import scala.collection.mutable.{Stack,Map}

class ChessPositions(val m : Int, val n : Int, val tokens : List[Char]) {

  private var board = Array.fill(m,n)('*')
  private var counter = Array.fill(m,n)(0)
  private val tokenLoc = new Stack[((Int,Int),Char)]()
  private var solutions = 0

  private def time[R](block: => Unit): Long = {
    val t0 = System.nanoTime()
    block
    val t1 = System.nanoTime()
    t1 - t0
  }

  private def printBoard() = {
    board.foreach(row => {
      print(row.mkString(" | "))
      println()
    })
  }

  private def printBoardReflection() = {
    for(row <- 0 until  m)
      {
        print(board(m - 1 - row).mkString(" | "))
        println()
      }
  }

  private def remove(index : Int, list: List[Char]) = list diff List(list(index))

  private def shift(list: List[Char])  = remove(0, list) :+ list.head

  private def difference(a : Int, b : Int)  = math.abs(a-b)

  private def inc(a : Int) = a + 1

  private def dec(a : Int) = a - 1

  private def kingCtr(i : Int, j : Int, f : Int => Int) = {
    if(i + 1 < m) counter(i+1)(j) = f(counter(i+1)(j))
    if(j + 1 < n) counter(i)(j+1) = f(counter(i)(j+1))
    if(i-1 >=0) counter(i-1)(j) = f(counter(i-1)(j))
    if(j-1 >=0) counter(i)(j-1) = f(counter(i)(j-1))
    if(i+1 < m && j+1 < n) counter(i+1)(j+1) = f(counter(i+1)(j+1))
    if(i-1 >=0 && j-1 >= 0) counter(i-1)(j-1) = f(counter(i-1)(j-1))
    if(i+1 < m && j-1 >= 0) counter(i+1)(j-1) = f(counter(i+1)(j-1))
    if(i-1 >= 0 && j+1 < n) counter(i-1)(j+1) = f(counter(i-1)(j+1))
  }

  private def knightCtr(i : Int, j : Int, f : Int => Int) = {
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

  private def bishopCtr(i : Int, j : Int, f : Int => Int) = {
    for(x <- 0 until m)
      for(y <- 0 until n)
      {
        if(difference(i,x) == difference(j,y) )
          if(i == x && j == y)
            counter(y)(x)
          else
            counter(x)(y) = f(counter(x)(y))
      }
  }

  private def rookCtr(i : Int, j : Int, f : Int => Int) = {
    for(x <- 0 until j)
      counter(i)(x) = f(counter(i)(x))
    for(x <- j+1 until n)
      counter(i)(x) = f(counter(i)(x))
    for(y <- 0 until i)
      counter(y)(j) = f(counter(y)(j))
    for(y <- i+1 until m)
      counter(y)(j) = f(counter(y)(j))
  }

  private def notAttacking(i : Int, j :Int, token : Char)  : Boolean = {

    def kingAttackFn(entry : ((Int,Int),Char)) = {
      difference(entry._1._1, i) <= 1 && difference(entry._1._2,j) <= 1
    }

    def rookAttackFn(entry : ((Int,Int),Char)) = {
      difference(entry._1._1, i) == 0 || difference(entry._1._2,j) == 0
    }

    def knightAttackFn(entry : ((Int,Int),Char)) = {
      (difference(entry._1._1, i) == 2 && difference(entry._1._2,j) == 1) ||  (difference(entry._1._1, i) == 1 && difference(entry._1._2,j) == 2)
    }

    def bishopAttachFn(entry : ((Int,Int),Char)) = {
      difference(entry._1._1, i) == difference(entry._1._2,j)
    }

    token match {
      case 'K' =>
        tokenLoc filter kingAttackFn isEmpty
      case 'Q' =>
        (tokenLoc filter rookAttackFn isEmpty) && (tokenLoc filter bishopAttachFn isEmpty)
      case 'R' =>
        tokenLoc filter rookAttackFn isEmpty
      case 'N' =>
        tokenLoc filter knightAttackFn isEmpty
      case 'B' =>
        tokenLoc filter bishopAttachFn isEmpty
      case  _ =>
        println("This should never be printed!")
        false
    }
  }

  private def place(i : Int, j : Int, token : Char) : (Int,Int) = {
    if(counter(i)(j) == 0 && notAttacking(i,j,token))
    {
      board(i)(j) = token
      tokenLoc.push(((i,j),token))
      incCtr(i,j,token)
      (i,j)
    }
    else
    {
      if(j+1 < n)
        place(i,j+1,token)
      else if(i+1 < m)
        place(i+1,0,token)
      else
        (-1,-1)
    }
  }

  private def incCtr(i : Int, j : Int, token : Char) = {
    token match {
      case 'K' =>
        kingCtr(i,j,inc)
      case 'Q' =>
        bishopCtr(i,j,inc)
        rookCtr(i,j,inc)
      case 'R' =>
        rookCtr(i,j,inc)
      case 'B' =>
        bishopCtr(i,j,inc)
      case 'N' =>
        knightCtr(i,j,inc)
      case  _ => println("This should never be printed!")
    }
  }

  private def decCtr(i : Int, j : Int,token : Char) = {
    token match {
      case 'K' =>
        kingCtr(i,j,dec)
      case 'Q' =>
        bishopCtr(i,j,dec)
        rookCtr(i,j,dec)
      case 'R' =>
        rookCtr(i,j,dec)
      case 'B' =>
        bishopCtr(i,j,dec)
      case 'N' =>
        knightCtr(i,j,dec)
      case  _ => println("This should never be printed!")
    }
  }

  private def displace(index : Int, tokens : List[Char]) : Unit = {
    if(tokenLoc.isEmpty)
      return
    val entry = tokenLoc.pop()
    decCtr(entry._1._1, entry._1._2,entry._2)
    board(entry._1._1)(entry._1._2) = '*'
    if(entry._1._2+1 < n)
      solve(entry._1._1,entry._1._2+1,index,tokens)
    else if(entry._1._1+1 < m)
      solve(entry._1._1+1,0,index,tokens)
    else
      displace(index-1,tokens)
  }

  private def solve(i : Int, j : Int,index : Int, tokens : List[Char]) : Unit = {
    if(index == tokens.size)
      return
    val placedLoc = place(i,j,tokens(index))
    if(placedLoc._1 != -1)
    {
        if(tokenLoc.size == tokens.size)
        {
          solutions += 1
          println(s"Solution #$solutions")
          printBoard()
          println()
          if(tokens != tokens.reverse)
          {
            solutions += 1
            println(s"Solution #$solutions")
            printBoardReflection()
            println()
          }
          displace(index,tokens)
        }
        else
        {
          if(placedLoc._2 +1 < n)
            solve(placedLoc._1,placedLoc._2+1,index+1,tokens)
          else if (placedLoc._1+1 < m)
            solve(placedLoc._1+1,0,index+1,tokens)
          else
            displace(index,tokens)
        }
    }
    else // backtrack
    {
      displace(index-1,tokens)
    }
  }

  def run(writeToFile : Boolean = false) : Int = {
    val permutations = Map.empty[List[Char],Int]
    tokens.permutations.foreach( item => {
      permutations.put(item,0)
    })
    tokens.permutations.foreach(entry =>
      if(permutations.contains(entry.reverse) && entry != entry.reverse) permutations.remove(entry))

    val timeTaken = time {
      permutations.foreach(
        entry => {
          //println(entry._1)
          board = Array.fill(m,n)('*')
          counter = Array.fill(m,n)(0)
          solve(0,0,0,entry._1)
        }
      )
    }

    val seconds : Double = timeTaken/1000000000
    val minutes : Double = seconds/60

    if(writeToFile)
    {
      val pw = new PrintWriter(new File("Stats.txt" ))
      pw.write(s"Number of Solutions : $solutions\n")
      pw.write(s"Time taken in seconds: $seconds\n")
      pw.write(s"Time taken in minutes: ${math.round(minutes)}")
      pw.close()
      solutions
    }
    else
    {
      println(s"Number of Solutions : $solutions\n")
      println(s"Time taken in seconds: $seconds\n")
      println(s"Time taken in minutes: ${math.round(minutes)}")
      solutions
    }
  }
}

object ChessPositions extends App {
  val tokens = List('K','K','Q','Q','B','B','N')
  val m = 7
  val n = 7
  val runner = new ChessPositions(m,n,tokens)
  runner.run(true)
}
