package main

/**
 * Created by saheb on 11/17/15.
 */

import java.io._

import scala.collection.mutable.Stack

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

  private def printBoard(board : Array[Array[Char]], m : Int, n : Int) = {
    for(i <- 0 until  m)
    {
      print(" | ")
      for(j <- 0 until  n)
      {
        print(board(i)(j))
        print(" | ")
      }
      println()
    }
  }

  private def remove(index : Int, list: List[Char]) = list diff List(list(index))

  private def shift(list: List[Char])  = remove(0, list) :+ list.head

  private def difference(a : Int, b : Int)  = math.abs(a-b)

  private def inc(a : Int) = a + 1

  private def dec(a : Int) = a - 1

  private def kingCtr(i : Int, j : Int, m: Int, n : Int,  counter : Array[Array[Int]], f : Int => Int) = {
    if(i + 1 < m) counter(i+1)(j) = f(counter(i+1)(j))
    if(j + 1 < n) counter(i)(j+1) = f(counter(i)(j+1))
    if(i-1 >=0) counter(i-1)(j) = f(counter(i-1)(j))
    if(j-1 >=0) counter(i)(j-1) = f(counter(i)(j-1))
    if(i+1 < m && j+1 < n) counter(i+1)(j+1) = f(counter(i+1)(j+1))
    if(i-1 >=0 && j-1 >= 0) counter(i-1)(j-1) = f(counter(i-1)(j-1))
    if(i+1 < m && j-1 >= 0) counter(i+1)(j-1) = f(counter(i+1)(j-1))
    if(i-1 >= 0 && j+1 < n) counter(i-1)(j+1) = f(counter(i-1)(j+1))
  }

  private def knightCtr(i : Int, j : Int, m: Int, n : Int,  counter : Array[Array[Int]], f : Int => Int) = {
    for(x <- 0 until m)
      for(y <- 0 until n)
      {
        if(difference(i,x) == 2 && difference(j,y) == 1)
        {
          if(i == x && j == y)
            counter(y)(x)
          else
            counter(x)(y)  = f(counter(x)(y))
        }
        else if(difference(i,x) == 1 && difference(j,y) == 2)
        {
          if(i == x && j == y)
            counter(y)(x)
          else
            counter(x)(y) = f(counter(x)(y))
        }
        else
          counter(y)(x)
      }
  }

  private def bishopCtr(i : Int, j : Int, m: Int, n : Int,  counter : Array[Array[Int]], f : Int => Int) = {
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

  private def rookCtr(i : Int, j : Int, m : Int , n : Int, counter : Array[Array[Int]], f : Int => Int) = {
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

  private def place(i : Int, j : Int, m : Int, n : Int, token : Char) : (Int,Int) = {
    if(counter(i)(j) == 0 && notAttacking(i,j,token))
    {
      board(i)(j) = token
      tokenLoc.push(((i,j),token))
      incCtr(i,j,m,n,token,counter)
      (i,j)
    }
    else
    {
      if(j+1 < n)
        place(i,j+1,m,n,token)
      else if(i+1 < m)
        place(i+1,0,m,n,token)
      else
        (-1,-1)
    }
  }

  private def check(i : Int, j : Int) : Boolean = {
    true
  }

  private def incCtr(i : Int, j : Int, m : Int, n : Int, token : Char, counter : Array[Array[Int]]) = {
    token match {
      case 'K' =>
        kingCtr(i,j,m,n,counter,inc)
      case 'Q' =>
        bishopCtr(i,j,m,n,counter,inc)
        rookCtr(i,j,m,n,counter,inc)
      case 'R' =>
        rookCtr(i,j,m,n,counter,inc)
      case 'B' =>
        bishopCtr(i,j,m,n,counter,inc)
      case 'N' =>
        knightCtr(i,j,m,n,counter,inc)
      case  _ => println("This should never be printed!")
    }
  }

  private def decCtr(i : Int, j : Int, m : Int, n : Int, token : Char, counter : Array[Array[Int]]) = {
    token match {
      case 'K' =>
        kingCtr(i,j,m,n,counter,dec)
      case 'Q' =>
        bishopCtr(i,j,m,n,counter,dec)
        rookCtr(i,j,m,n,counter,dec)
      case 'R' =>
        rookCtr(i,j,m,n,counter,dec)
      case 'B' =>
        bishopCtr(i,j,m,n,counter,dec)
      case 'N' =>
        knightCtr(i,j,m,n,counter,dec)
      case  _ => println("This should never be printed!")
    }
  }

  private def displace(token : Char,index : Int, tokens : List[Char]) : Unit = {
    if(tokenLoc.isEmpty)
      return
    val entry = tokenLoc.pop()
    decCtr(entry._1._1, entry._1._2,m,n,entry._2,counter)
    board(entry._1._1)(entry._1._2) = '*'
    if(entry._1._2+1 < n)
      solve(entry._1._1,entry._1._2+1,index,tokens)
    else if(entry._1._1+1 < m)
      solve(entry._1._1+1,0,index,tokens)
    else
      displace(tokens(index),index-1,tokens)
  }

  private def solve(i : Int, j : Int,index : Int, tokens : List[Char]) : Unit = {
    if(index == tokens.size)
      return
    val placedLoc = place(i,j,m,n,tokens(index))
    if(placedLoc._1 != -1)
    {
      val validPosition = check(i,j)
      if(validPosition)
      {
        if(tokenLoc.size == tokens.size)
        {
          solutions += 1
          println(s"Solution #$solutions")
          printBoard(board,m,n)
          println()
          displace(tokens(index),index,tokens)
        }
        else
        {
          if(placedLoc._2 +1 < n)
            solve(placedLoc._1,placedLoc._2+1,index+1,tokens)
          else if (placedLoc._1+1 < m)
            solve(placedLoc._1+1,0,index+1,tokens)
          else
            displace(tokens(index),index,tokens)
        }
      }
      else
      {
        printBoard(board,m,n)
      }
    }
    else // backtrack
    {
      displace(tokens(index),index-1,tokens)
    }
  }

  def run(writeToFile : Boolean = false) : Int = {
    val timeTaken = time {
      tokens.permutations.foreach(
        items => {
          //println(items)
          board = Array.fill(m,n)('*')
          counter = Array.fill(m,n)(0)
          solve(0,0,0,items)
        }
      )
    }

    val seconds = timeTaken/1000000000
    val minutes = seconds/60

    if(writeToFile)
    {
      val pw = new PrintWriter(new File("Stats.txt" ))
      pw.write(s"Number of Solutions : $solutions\n")
      pw.write(s"Time taken in seconds: $seconds\n")
      pw.write(s"Time taken in minutes: $minutes")
      pw.close
      solutions
    }
    else
    {
      println(s"Number of Solutions : $solutions\n")
      println(s"Time taken in seconds: $seconds\n")
      println(s"Time taken in minutes: $minutes")
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
