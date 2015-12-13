import chess.{Util, ChessPositions}
import org.scalatest.FunSuite

/**
 * Created by saheb on 12/9/15.
 */
class FunctionsTest extends FunSuite{

  test("checking getBoard Function") {
    val chess = new ChessPositions(4,4,List('Q','Q','Q','Q'))
    val board = List.fill(4,4)('*')
    val newBoard = List.fill(3,4)('*') ++ List(List('*','*','*','Q'))
    assertResult(Util.getBoard(Util.createPiece(3,3,'Q'),board))(newBoard)
  }

}
