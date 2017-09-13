package moonrunner

import org.scalatest.FlatSpec
import org.sundhine.moonrunner.mazerunner._

class NodeTest extends FlatSpec {

  val state = State(Stats(12,24,12), adventureSheet = Set.empty)

  behavior of "maze solver"

  def solveAndUnwrap(maze: Maze): Option[Path] = solveMaze(maze, state).map(_.path)

  it should "solve a simple maze" in {
    val maze = Map(
      1 -> choiceNode(2),
      2 -> victoryNode
    )


    assert(solveAndUnwrap(maze).contains(List(1, 2)))
  }

  it should "solve a more complex maze" in {
    val maze = Map(
      1 -> choiceNode(2, 3, 4),
      2 -> failureNode,
      3 -> choiceNode(5),
      4 -> choiceNode(6, 7),
      5 -> failureNode,
      6 -> failureNode,
      7 -> victoryNode,
    )

    assert(solveAndUnwrap(maze).contains(List(1, 4, 7)))
  }

  it should "be stable, so the same maze in the same way" in {
    val maze = Map(
      1 -> choiceNode(2, 3, 4),
      2 -> choiceNode(4),
      3 -> choiceNode(5),
      4 -> choiceNode(6, 7),
      5 -> failureNode,
      6 -> choiceNode(8),
      7 -> choiceNode(8),
      8 -> victoryNode
    )

    assert(solveAndUnwrap(maze).contains(List(1, 2, 4, 6, 8)))
  }

  it should "handle loops" in {
    val maze = Map(
      1 -> choiceNode(1, 2),
      2 -> victoryNode
    )

    assert(solveAndUnwrap(maze).contains(List(1, 2)))
  }

  it should "throw an error if the maze has no solution" in {
    val maze = Map(
      1 -> failureNode
    )

    assert(solveAndUnwrap(maze).isEmpty)
  }

  behavior of "more complex story nodes"

  it should "correctly update state when the update function is called and expose the correct nodes" in {
    val node = updateWithChoiceNode(lugoshPriestgate, 1,2,3)

    assert(!state.adventureSheet(Lugosh))
    val updatedState = node.update(state)

    assert(updatedState.adventureSheet(Lugosh))
    assert(node.choices(state) == List(1,2,3))
  }

  it should "offer certain choices based on status" in {
    val node = optionalChoice(st => if (st.adventureSheet(Lugosh)) List(1) else List(2))

    assert(node.choices(state) == List(2))

    assert(node.choices(lugoshPriestgate(state)) == List(1))
  }

}
