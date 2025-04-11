import scala.meta.*

object Explanation {

  
  /** Produces explanation for given list of AST
    *
    * @param trees List of trees from the closest to the cursor to the most distant
    * @return String explaining the syntax at the cursor
    */
  def forTreePath(trees: List[Tree]): String = {
    trees.head match {
      case _: Defn.Given => "given definition"
      case t: Type       => s"Type ${t.text}"
      case t: Template if t.text.contains(" with ") =>
        s"with keyword starts the body of given instance for ${t.inits}"
      case t: Template       => s"Template ${t.text}"
      case t: Type.ArgClause => s"Type parameter ${t.text}"
      case t: Tree =>
        s"Fallback: ${t.text} (${t.getClass().toString.split("$").last})"
    }
  }
}
