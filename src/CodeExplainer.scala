import com.raquo.laminar.api.L.*

import scala.annotation.tailrec
import scala.meta.*
import com.raquo.airstream.core.Signal

case class CodeExplainer(
    tree: Tree,
    textIndex: TextIndex,
    openNodes: Var[Set[Int]],
    cursor: Var[CodeMirrorCursor],
    hover: Var[Option[Int]]
) {

  def getTree(id: Int): Option[Tree] =
    direct.get(id)

  lazy val node = div(
    cls := "bg-gray-100 p-2 rounded-lg shadow-md w-full",
    deepestTreeUnderCursor --> pathToCursor,
    deepestTreeUnderCursor.map(_.head) --> hover.someWriter,
    pathToCursor.signal.map(_.toSet) --> append,
    div(
      cls := "flex flex-row gap-2 mb-2 bg-gray-300 rounded-md px-2 py-1 text-m",
      "Code explanation"
    ),
    div(
      cls := "flex flex-row text-m",
      encode(tree)
    )
    // code(encode(tree))
  )

  /** Helper to expand all trees with a given Set[Int] of tree ids */
  private val append = openNodes.updater[Set[Int]]: (cur, next) =>
    cur ++ next
  /** Represents the path from the root to the deepest tree that spans the
    * current cursor position
    */
  private val pathToCursor = Var(List.empty[Int])

  /** Monitor cursor position and find the deepest tree that spans the cursor
    * position, returning a reversed list of ancestors (deepest first) of that
    * tree.
    */
  private lazy val deepestTreeUnderCursor: Signal[List[Int]] =
    cursor.signal.map{ cursor =>
      textIndex
        .posLookup(cursor.line, cursor.ch)
        .map{ offset =>
          val deepest = intervalTree
            .resolve(offset)
            .sortBy{ treeId =>
              offset - direct(treeId).pos.start
            }
          deepest.headOption match {
            case None => Nil
            case Some(value) =>
              @tailrec
              def go(id: Int, cur: List[Int]): List[Int] =
                direct.get(id).flatMap(_.parent).flatMap(reverse.get) match {
                  case None         => cur
                  case Some(parent) => go(parent, parent :: cur)
                }

              go(value, List(value)).reverse
          }
        }
        .getOrElse(Nil)
    }

  private lazy val (
    /** Map from tree id to tree */
    direct,
    /** Map from tree to id */
    reverse
  ) = index(tree)

  private lazy val intervalTree = IntervalTree.construct(reverse.map {
    (tree, id) => OffsetRange(tree.pos.start, tree.pos.end) -> id
  })

  /** Recursively create DOM tree structure that represents the Scalameta tree.
    *
    * TODO: Make this tail recursive.
    */
  private def encode(t: Tree): Element = {
    // val id = reverse(t)
    div(
      text <-- deepestTreeUnderCursor.map { ids => 
        val trees = ids.map(direct)
        Explanation.forTreePath(trees)
      }
    )
  }

  /** Index every tree with a numeric identifier */
  private def index(t: Tree): (Map[Int, Tree], Map[Tree, Int]) = {
    def go(
        next: Seq[Tree],
        n: Int,
        direct: Map[Int, Tree],
        reverse: Map[Tree, Int]
    ): (Map[Int, Tree], Map[Tree, Int]) =
      if next.isEmpty then (direct, reverse)
      else {
        val ids = next.zipWithIndex.map { case (tree, idx) =>
          (n + idx) -> tree
        }.toMap
        val reverseIds = ids.map(_.swap).toMap
        go(
          next.flatMap(_.children),
          n + ids.size,
          direct ++ ids,
          reverse ++ reverseIds
        )
      }
    end go
    go(Seq(t), 0, Map.empty, Map.empty)
  }
}
