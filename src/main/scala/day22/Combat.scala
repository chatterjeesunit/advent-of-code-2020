package day22

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.io.Source

/**
 *
 206
 34324
  291

 */
object Combat {

  case class GameStatus(winnerQueue: Queue[Int] = Queue(),
                        winnerPlayer: Int = 0,
                        currentGameFinished: Boolean = false,
                        allGamesFinished: Boolean = false)

  case class Game(q1:Queue[Int], q2: Queue[Int],
                  p1Deck: Set[String] = Set(), p2Deck: Set[String] = Set(),
                  status: GameStatus = GameStatus()) {

    def isDeckRepeatedForP1(): Boolean = {
      p1Deck.contains(q1.mkString(","))
    }

    def isDeckRepeatedForP2(): Boolean = {
      p2Deck.contains(q2.mkString(","))
    }

    def showCards(): (Game, Int, Int) = {
      if(q1.isEmpty) {
        (this.copy(status = GameStatus(winnerQueue = q2, currentGameFinished = true, winnerPlayer = 2)), 0 , 0)
      } else if (q2.isEmpty) {
        (this.copy(status = GameStatus(winnerQueue = q1, currentGameFinished = true, winnerPlayer = 1)), 0 , 0)
      }
      else {
        val p1DeckNew = p1Deck ++ Set(q1.mkString(","))
        val p2DeckNew = p2Deck ++ Set(q2.mkString(","))
        val (n1: Int, q1_new: Queue[Int]) = q1.dequeue
        val (n2: Int, q2_new: Queue[Int]) = q2.dequeue
        (Game(q1_new, q2_new, p1DeckNew, p2DeckNew, status.copy()), n1, n2)
      }

    }

    def playRecursive(n1: Int, n2: Int): Boolean = q1.size >= n1 && q2.size >= n2
  }


  def main(args: Array[String]): Unit = {
    val input1 = parseInput("day22-input01.txt")
    val input2 = parseInput("day22-input02.txt")



//    println("******* Part 1 - Test ************")
//    val result1 = part1(input1._1, input1._2)
//    println(result1)
//
//    println("\n******* Part 1 - Puzzle ************")
//    val result2 = part1(input2._1, input2._2)
//    println(result2)
//
//    println("\n\n******* Part 2 - Test ************")
//    val result3 = part2(input1._1, input1._2)
//    println(result3)

    println("\n\n******* Part 2 - Puzzle ************")
    val result4 = part2(input2._1, input2._2)
    println(result4)

  }




  def findWinnerRecursive(stack: mutable.Stack[Game], currentGame: Game): GameStatus = {

    (
      currentGame.status.allGamesFinished,
      currentGame.status.currentGameFinished,
      currentGame.isDeckRepeatedForP1(),
      currentGame.isDeckRepeatedForP2()
    ) match {
      case (true, _, _, _) => currentGame.status.copy()
      case (false,true, _, _ ) => currentGame.status.copy(currentGameFinished = false)
      case (false, false, true, false) => GameStatus(currentGame.q1, 1, true, true)
      case (false, false, false, true) => GameStatus(currentGame.q2, 2,  true, true)
      case _ => {
        val (game_new, n1, n2) = currentGame.showCards()

        if(game_new.status.currentGameFinished) {
          game_new.status
        }
        else if(game_new.playRecursive(n1, n2)) {
          val winner = findWinnerRecursive(
            stack.append(game_new),
            Game(q1 = game_new.q1.take(n1), q2 = game_new.q2.take(n2)))
          stack.pop()
          val game = winner.winnerPlayer match {
            case 1 => game_new.copy(q1 = game_new.q1.enqueue(n1).enqueue(n2))
            case 2 => game_new.copy(q2 = game_new.q2.enqueue(n2).enqueue(n1))
            case _ => game_new.copy()
          }
          findWinnerRecursive(stack, game)
        } else {
          if (n1 > n2) {
            findWinnerRecursive(stack, game_new.copy(q1 = game_new.q1.enqueue(n1).enqueue(n2)))
          } else {
            findWinnerRecursive(stack, game_new.copy(q2 = game_new.q2.enqueue(n2).enqueue(n1)))
          }
        }
      }
    }
  }

  def part1(p1: Queue[Int], p2: Queue[Int]) = {

    @tailrec
    def findWinner(q1: Queue[Int], q2: Queue[Int]): Queue[Int] = {
      if (q1.isEmpty) return q2
      else if (q2.isEmpty) return q1

      val (n1: Int, q1_new: Queue[Int]) = q1.dequeue
      val (n2: Int, q2_new: Queue[Int]) = q2.dequeue

      val (q11: Queue[Int], q22:Queue[Int]) =
        if(n1 > n2) {
          (q1_new.enqueue(n1).enqueue(n2), q2_new)
        } else if (n2 > n1) {
          (q1_new, q2_new.enqueue(n2).enqueue(n1))
        } else {
          (q1_new, q2_new)
        }
      findWinner(q11, q22)
    }

    val winner = findWinner(p1, p2)
    println(winner)
    winner.zipWithIndex.foldLeft(0)((acc, res) => acc + (res._1 * (winner.size - res._2)))
  }


  def part2(p1: Queue[Int], p2: Queue[Int]) = {
    val winner = findWinnerRecursive(mutable.Stack(), Game(p1, p2))
    println(winner)
    val size = winner.winnerQueue.size
    winner.winnerQueue.zipWithIndex.foldLeft(0)((acc, res) => acc + (res._1 * (size - res._2)))
  }

  def parseInput(fileName: String): (Queue[Int], Queue[Int]) = {

    val source: Source = Source.fromResource(fileName)
    val lines = source.getLines().toList.mkString("|")
//    println(lines)
    source.close()
    val cardRegex = "([0-9]+)".r
    val parsedInput = lines
      .split("\\|\\|")
      .map(input =>
        input.split("\\|").flatMap(_ match {
          case cardRegex(n) => Some(n)
          case _ => None
        }).foldLeft(Queue[Int]()){(q, n) => q.enqueue(n.toInt)}
      )

//    println(parsedInput)
    (parsedInput(0), parsedInput(1))
  }
}
