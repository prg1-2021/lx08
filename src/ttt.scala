object TicTacToe extends App {
  import scala.collection.mutable.HashSet
  import scala.collection.mutable.Queue

  type Board = String
  val POS = (0 until 9).toList

  type Mark = Char
  def flipMark(mark: Mark): Mark = mark match { case '〇' => '×' case _ => '〇' }

  def placeable(board: Board, pos: Int): Boolean = board(pos) == '　'

  def place(board: Board, pos: Int, mark: Mark): Board = board.substring(0, pos) + mark + board.substring(pos + 1)

  def isLeaf(board: Board): Boolean = {
    val Patterns = List((0, 1, 2), (3, 4, 5), (6, 7, 8),   (0, 3, 6), (1, 4, 7), (2, 5, 8),   (0, 4, 8), (2, 4, 6))
    Patterns.exists({
      case (p1: Int, p2: Int, p3: Int) => {
        val (m1, m2, m3) = (board(p1), board(p2), board(p3))
        (m1 != '　' && m1 == m2 && m2 == m3) || board.indexOf('　') == -1
      }
    })
  }

  var leaves = new HashSet[Board]()
  var c = 0
  val Q = Queue[(Board, Mark)](("　" * 9, '〇'))
  while (!Q.isEmpty) {
    val (board: Board, mark: Mark) = Q.dequeue()
    if (isLeaf(board)) c = c + 1
    if (!leaves.contains(board)) {
      if (isLeaf(board)) leaves.add(board)
      else {
        for (pos <- POS) {
          if (placeable(board, pos))
            Q.enqueue((place(board, pos, mark), flipMark(mark)))
        }
      }
    }
  }

  println(leaves.size, leaves.filter(board => board.indexOf('　') == -1).size)
  print('c', c)
}
