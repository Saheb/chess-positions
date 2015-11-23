package main

/**
 * Created by saheb on 11/17/15.
 */

import java.io._

import scala.annotation.tailrec
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
    println(s"Solution #$solutions")
    println(board map (_ mkString " | ") mkString "\n")
    println()
  }

  private def printBoardReflection() = {
    println(s"Solution #$solutions")
    println(board reverseMap (_ mkString " | ") mkString "\n")
    println()
  }

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


  private def bishopCtr(i : Int, j : Int, f : Int => Int)= {
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

    def kingAttackFn(entry : ((Int,Int),Char)) : Boolean = {
      difference(entry._1._1, i) <= 1 && difference(entry._1._2,j) <= 1
    }

    def rookAttackFn(entry : ((Int,Int),Char)) : Boolean = {
      difference(entry._1._1, i) == 0 || difference(entry._1._2,j) == 0
    }

    def knightAttackFn(entry : ((Int,Int),Char)) : Boolean = {
      (difference(entry._1._1, i) == 2 && difference(entry._1._2,j) == 1) ||  (difference(entry._1._1, i) == 1 && difference(entry._1._2,j) == 2)
    }

    def bishopAttachFn(entry : ((Int,Int),Char)) : Boolean = {
      difference(entry._1._1, i) == difference(entry._1._2,j)
    }

    token match {
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

  @tailrec
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

  @tailrec
  private def displace(index : Int, tokens : List[Char]) : (Int,Int,Int) = {
    if(tokenLoc.isEmpty)
      (m,n,index)
    else
    {
      val entry = tokenLoc.pop()
      decCtr(entry._1._1, entry._1._2,entry._2)
      board(entry._1._1)(entry._1._2) = '*'
      if(entry._1._2 + 1 >= n && entry._1._1+1 >= m)
        displace(index-1,tokens)
      else
        (entry._1._1,entry._1._2,index)
    }
  }

  @tailrec
  private def solve(i : Int, j : Int,index : Int, tokens : List[Char]) : Int = {
    val placedLoc = place(i,j,tokens(index))
    if(placedLoc._1 != -1)
    {
        if(tokenLoc.size == tokens.size)
        {
          solutions += 1
          printBoard()
          if(tokens != tokens.reverse)
          {
            solutions += 1
            printBoardReflection()
          }
          val entry = displace(index,tokens) // backtrack
          if(entry._2+1 < n)
            solve(entry._1,entry._2+1,entry._3,tokens)
          else if(entry._1+1 < m)
            solve(entry._1+1,0,entry._3,tokens)
          else
            -1
        }
        else
        {
          if(placedLoc._2 +1 < n)
            solve(placedLoc._1,placedLoc._2+1,index+1,tokens)
          else if(placedLoc._1+1 < m)
            solve(placedLoc._1+1,0,index+1,tokens)
          else // backtrack
          {
            val entry = displace(index, tokens)
            if (entry._2 + 1 < n)
              solve(entry._1, entry._2 + 1, entry._3, tokens)
            else if (entry._1 + 1 < m)
              solve(entry._1 + 1, 0, entry._3, tokens)
            else
              -1
          }
        }
    }
    else // backtrack
    {
      val entry = displace(index-1,tokens)
      if (entry._2 + 1 < n)
        solve(entry._1, entry._2 + 1, entry._3, tokens)
      else if (entry._1 + 1 < m)
        solve(entry._1 + 1, 0, entry._3, tokens)
      else
        -1
    }
  }

  def run(writeToFile : Boolean = false) : Int = {
    val permutations = Map.empty[List[Char],Int]
    val timeTaken = time {
      tokens.permutations.foreach(item => {
        permutations.put(item,0)
      })
      tokens.permutations.foreach(entry =>
        if(permutations.contains(entry.reverse) && entry != entry.reverse) permutations.remove(entry))

      permutations.foreach(
        entry => {
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
      pw.write(s"Stats are gathered by running the program on Intellij Idea 14.1.4\n")
      pw.write(s"Dimensions of the board are $m rows and $n columns\n")
      pw.write(s"Tokens for the problem are $tokens\n")
      pw.write(s"Number of Solutions : $solutions\n")
      pw.write(s"Time taken in seconds: $seconds\n")
      pw.write(s"Time taken in minutes: ${math.round(minutes)}")
      pw.close()
    }

    println(s"Dimensions of the board are $m rows and $n columns")
    println(s"Tokens for the problem are $tokens")
    println(s"Number of Solutions : $solutions")
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
  runner.run()// change to true to write resutls to Stats.txt
}
