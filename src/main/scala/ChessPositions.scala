package main

/**
 * Created by saheb on 11/17/15.
 */

import java.io._

import scala.collection.mutable.{Stack,Map}
import scala.util.{Success, Try}

class ChessPositions(val m : Int, val n : Int, val tokens : List[Char]) {

  private var board = Array.fill(m,n)('*')
  private var counter = Array.fill(m,n)(0)
  private val tokenLoc = new Stack[((Int,Int),Char)]()
  private var solutions = 0
  private val attackCache = Map.empty[((Int,Int,Char),(Int,Int,Char)),Boolean]

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
        if(difference(i,x) == difference(j,y))
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

  private def loadCache() : Unit = {
    def kingAttackFn(entry : (Int,Int,Char), entry2 : (Int,Int,Char)) : Boolean = {
      difference(entry._1, entry2._1) <= 1 && difference(entry._2,entry2._2) <= 1
    }

    def rookAttackFn(entry : (Int,Int,Char), entry2 : (Int,Int,Char)) : Boolean = {
      difference(entry._1, entry2._1) == 0 || difference(entry._2,entry2._2) == 0
    }

    def knightAttackFn(entry : (Int,Int,Char), entry2 : (Int,Int,Char)) : Boolean = {
      (difference(entry._1, entry2._1) == 2 && difference(entry._2,entry2._2) == 1) || (difference(entry._1, entry2._1) == 1 && difference(entry._2,entry2._2) == 2)
    }

    def bishopAttachFn(entry : (Int,Int,Char), entry2 : (Int,Int,Char)) : Boolean = {
      difference(entry._1, entry2._1) == difference(entry._2,entry2._2)
    }

    val distinctTokens = tokens.distinct
    for(a<- 0 until m) {
      for (b <- 0 until n) {
        for (x <- 0 until distinctTokens.size) {
          for (y <- 0 until distinctTokens.size) {
            for (i <- 0 until m) {
              for (j <- 0 until n) {
                distinctTokens(x) match {
                  case 'K' =>
                    if(kingAttackFn((a, b, distinctTokens(x)),(i,j,distinctTokens(y))))
                      {
                        attackCache.update(((a, b, distinctTokens(x)), (i,j,distinctTokens(y))), true)
                        attackCache.update(((i,j,distinctTokens(y)),(a, b, distinctTokens(x))), true)
                      }
                  case 'Q' =>
                    if(rookAttackFn((a, b, distinctTokens(x)),(i,j,distinctTokens(y))) || bishopAttachFn((a, b, distinctTokens(x)),(i,j,distinctTokens(y))))
                      {
                        attackCache.update(((a, b, distinctTokens(x)), (i,j,distinctTokens(y))), true)
                        attackCache.update(((i,j,distinctTokens(y)),(a, b, distinctTokens(x))), true)
                      }
                  case 'R' =>
                    if(rookAttackFn((a, b, distinctTokens(x)),(i,j,distinctTokens(y))))
                      {
                        attackCache.update(((a, b, distinctTokens(x)), (i,j,distinctTokens(y))), true)
                        attackCache.update(((i,j,distinctTokens(y)),(a, b, distinctTokens(x))), true)
                      }
                  case 'N' =>
                    if(knightAttackFn((a, b, distinctTokens(x)),(i,j,distinctTokens(y))))
                      {
                        attackCache.update(((a, b, distinctTokens(x)), (i,j,distinctTokens(y))), true)
                        attackCache.update(((i,j,distinctTokens(y)),(a, b, distinctTokens(x))), true)
                      }
                  case 'B' =>
                    if(bishopAttachFn((a, b, distinctTokens(x)),(i,j,distinctTokens(y))))
                      {
                        attackCache.update(((a, b, distinctTokens(x)), (i,j,distinctTokens(y))), true)
                        attackCache.update(((i,j,distinctTokens(y)),(a, b, distinctTokens(x))), true)
                      }
                  case _ =>
                    println("This should never be printed!")
                    false
                }
              }
            }
          }
        }
      }
    }
    attackCache = attackCache filter(p => p._1._1._3 != p._1._2._3)
  }


  private def notAttacking(i : Int, j :Int, token : Char)  : Boolean = {

    def cacheFn(entry : ((Int,Int),Char)) : Boolean = {
      attackCache.getOrElse(((entry._1._1,entry._1._2,entry._2),(i,j,token)),false)
    }

    !(tokenLoc exists cacheFn)
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
      case  _ =>
        println("This should never be printed!")
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
      case  _ =>
        println("This should never be printed!")
    }
  }

  private def displace(index : Int, tokens : List[Char]) : Try[Unit] = Try{
    val entry = tokenLoc.pop()
    decCtr(entry._1._1, entry._1._2,entry._2)
    board(entry._1._1)(entry._1._2) = '*'
    if(entry._1._2+1 < n)
      Success(solve(entry._1._1,entry._1._2+1,index,tokens))
    else if(entry._1._1+1 < m)
      Success(solve(entry._1._1+1,0,index,tokens))
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
          printBoard()
          if(tokens != tokens.reverse)
          {
            solutions += 1
            printBoardReflection()
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
      board = Array.fill(m,n)('*')
      counter = Array.fill(m,n)(0)
      loadCache()
      permutations.foreach(
        entry => {
          //println(entry._1)
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
