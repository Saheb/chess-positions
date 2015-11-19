import main.ChessPositions
import org.scalatest.FunSuite

/**
 * Created by saheb on 11/20/15.
 */

class BasicPositions extends FunSuite {

  test("3 Kings and 2 Rooks in 3 X 3") {
    val test = new ChessPositions(3,3,List('K','K','R'))
    assertResult(test.run())(4)
  }

  test("4 Knights and 2 Rooks in 4 X 4"){
    val test = new ChessPositions(4,4,List('N','N','N','N','R','R'))
    assertResult(test.run())(8)
  }

  test("8 Queens in 8 X 8") {
    val test = new ChessPositions(8,8,List('Q','Q','Q','Q','Q','Q','Q','Q'))
    assertResult(test.run())(92)
  }
}
