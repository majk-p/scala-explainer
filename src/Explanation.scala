import scala.meta.*

object Explanation {

  /** Produces explanation for given list of AST
    *
    * @param trees
    *   List of trees from the closest to the cursor to the most distant
    * @return
    *   String explaining the syntax at the cursor
    */
  def forTreePath(trees: List[Tree]): String = {
    if (trees.isEmpty) return "No syntax element found at cursor position"

    // Get the nearest tree to the cursor
    val tree = trees.head

    // Check for parent context if available
    val parent = if (trees.length > 1) Some(trees(1)) else None

    tree match {
      // Definitions
      case t: Defn.Class =>
        s"Class definition: '${t.name}' - A blueprint for creating objects with shared structure and behavior."

      case t: Defn.Trait =>
        s"Trait definition: '${t.name}' - An interface that can also contain implemented methods and fields."

      case t: Defn.Object =>
        s"Object definition: '${t.name}' - A singleton instance that exists without explicit instantiation."

      case t: Defn.Def =>
        val paramInfo =
          if (t.paramss.isEmpty) "no parameters"
          else s"with ${t.paramss.flatten.length} parameter(s)"
        s"Method definition: '${t.name}' $paramInfo - A named, reusable block of code that performs a specific task."

      case t: Defn.Val =>
        s"Immutable value definition: '${t.pats.mkString(", ")}' - A constant value that cannot be reassigned once initialized."

      case t: Defn.Var =>
        s"Mutable variable definition: '${t.pats.mkString(", ")}' - A variable that can be reassigned after initialization."

      case t: Defn.Type =>
        s"Type alias definition: '${t.name}' - A new name for an existing type, creating a synonym."

      case t: Defn.Given =>
        s"Given instance: '${
            if (t.name.toString.trim.isEmpty) "<anonymous>" else t.name
          }' - Provides implicit evidence for a specific type in the context of Scala 3's given/using mechanism."

      case t: Defn.Enum =>
        s"Enum definition: '${t.name}' - A type representing a fixed set of named values."

      case t: Defn.EnumCase =>
        s"Enum case: '${t.name}' - A specific value within an enumeration."

      case t: Defn.GivenAlias =>
        s"Given alias: Provides a new name for an existing given instance."

      case t: Defn.Macro =>
        s"Macro definition: '${t.name}' - Code that generates code at compile time."

      // Declarations
      case t: Decl.Def =>
        s"Method declaration: '${t.name}' - Defines a method signature without an implementation (abstract method)."

      case t: Decl.Val =>
        s"Value declaration: '${t.pats.mkString(", ")}' - Declares an immutable value without providing an implementation."

      case t: Decl.Var =>
        s"Variable declaration: '${t.pats.mkString(", ")}' - Declares a mutable variable without providing an implementation."

      case t: Decl.Type =>
        s"Abstract type declaration: '${t.name}' - Declares a type without specifying its implementation."

      // Imports
      case t: Import =>
        s"Import statement: '${t.text}' - Makes classes, objects, or members available without fully qualifying their names."

      case t: Importee.Wildcard =>
        "Wildcard import: Imports all members from a package or object."

      case t: Importee.Name =>
        s"Named import: '${t.name}' - Imports a specific named member."

      case t: Importee.Rename =>
        s"Renamed import: '${t.name}' renamed to '${t.rename}' - Imports a member with a different local name."

      case t: Importee.Unimport =>
        s"Unimport: '${t.name}' - Excludes a specific member from a wildcard import."

      // Parameters
      case t: Term.Param =>
        val modifiers =
          if (t.mods.isEmpty) "" else s" (${t.mods.map(_.text).mkString(", ")})"
        val typeAnnot =
          if (t.decltpe.isEmpty) "" else s": ${t.decltpe.get.text}"
        s"Parameter: '${t.name}'$typeAnnot$modifiers - Input value for a method or function."

      case t: Type.Param =>
        val bounds =
          if (t.cbounds.nonEmpty)
            s" with context bounds ${t.cbounds.map(_.text).mkString(", ")}"
          else ""
        s"Type parameter: '${t.name}'${bounds} - A placeholder for a type that will be specified when used."

      // Modifiers
      case t: Mod.Private =>
        "Private modifier: Restricts access to the current class or object."

      case t: Mod.Protected =>
        "Protected modifier: Restricts access to the current class, object, and subclasses."

      case t: Mod.Final =>
        "Final modifier: Prevents overriding or extension."

      case t: Mod.Case =>
        "Case modifier: Creates a class with additional features for pattern matching and data manipulation."

      case t: Mod.Abstract =>
        "Abstract modifier: Indicates that a class or method lacks complete implementation."

      case t: Mod.Sealed =>
        "Sealed modifier: Restricts inheritance to the same file."

      case t: Mod.Implicit =>
        "Implicit modifier (Scala 2): Marks a definition that can be automatically used by the compiler in certain contexts."

      case t: Mod.Lazy =>
        "Lazy modifier: Delays evaluation until the first access."

      case t: Mod.Override =>
        "Override modifier: Explicitly indicates that a method or value overrides a definition from a parent class."

      case t: Mod.Inline =>
        "Inline modifier: Suggests that the compiler should inline the definition at the call site."

      case t: Mod.Transparent =>
        "Transparent modifier: Makes a match expression or an inline method more powerful for type inference."

      case t: Mod.Opaque =>
        "Opaque modifier: Creates a type alias that hides its implementation details."

      case t: Mod.Open =>
        "Open modifier: Explicitly marks a class that can be extended outside its defining module."

      // Types
      case t: Type.Name =>
        s"Type reference: '${t.text}' - Reference to a named type."

      case t: Type.Apply =>
        s"Parameterized type: '${t.text}' - A type with type parameters applied."

      case t: Type.Function =>
        s"Function type: '${t.text}' - Describes the type of a function with specific parameter and return types."

      case t: Type.Tuple =>
        s"Tuple type: '${t.text}' - A type representing an ordered collection of elements of possibly different types."

      case t: Type.With =>
        s"Compound type: '${t.text}' - A type that combines multiple types with the 'with' keyword."

      case t: Type.And =>
        s"Intersection type: '${t.text}' - A type that represents values that have all of the specified types (Scala 3)."

      case t: Type.Or =>
        s"Union type: '${t.text}' - A type that represents values that have one of the specified types (Scala 3)."

      case t: Type.Refine =>
        s"Refined type: A type with additional structural constraints."

      case t: Type.Existential =>
        s"Existential type: A type that involves 'forSome' to declare hidden type parameters."

      case t: Type.Annotate =>
        s"Annotated type: A type with annotations attached."

      case t: Type.Lambda =>
        s"Type lambda: A higher-kinded type abstraction."

      case t: Type.Method =>
        s"Method type: Describes a method's parameter and return types."

      case t: Type.Placeholder =>
        s"Type placeholder (_): A wildcard type used when the specific type is inferred or unimportant."

      case t: Type.Bounds =>
        val lo = t.lo match {
          case Type.Name("Nothing") => ""
          case other => s" >: ${other.map(_.syntax).getOrElse("unknown")}"
        }
        val hi = t.hi match {
          case Type.Name("Any") => ""
          case other => s" <: ${other.map(_.syntax).getOrElse("unknown")}"
        }
        s"Type bounds:${lo}${hi} - Constraints on a type parameter."

      case t: Type.ByName =>
        s"By-name parameter type: '=> ${t.text}' - A parameter that is evaluated only when accessed."

      case t: Type.Repeated =>
        s"Repeated parameter type: '${t.text}*' - A varargs parameter that accepts multiple arguments."

      case t: Type.ArgClause =>
        s"Type parameter clause: '${t.text}' - Contains type parameters for a class, trait, or method."

      // Terms
      case t: Term.Name =>
        s"Reference to '${t.text}' - An identifier that refers to a value, variable, method, or object."

      case t: Term.Select =>
        s"Member selection: '${t.text}' - Accessing a member of an object, class, or package."

      case t: Term.Apply =>
        s"Function or method call: '${t.text}' - Invokes a function or method with arguments."

      case t: Term.ApplyType =>
        s"Type application: '${t.text}' - Specifies type parameters for a generic method or function."

      case t: Term.Block =>
        "Code block: A sequence of expressions enclosed in braces, returning the value of the last expression."

      case t: Term.If =>
        "If expression: A conditional expression that evaluates to one of two alternatives based on a condition."

      case t: Term.Match =>
        "Match expression: Pattern matching that evaluates different expressions based on the structure of a value."

      case t: Term.Try =>
        "Try-catch block: Handles exceptions by attempting an operation and catching potential errors."

      case t: Term.TryWithHandler =>
        "Try with handler: A try block with a custom exception handler."

      case t: Term.Function =>
        val arrowType = if (t.text.contains("=>")) "=>" else "⇒"
        s"Anonymous function (lambda): '${t.text}' - A function defined inline using the $arrowType syntax."

      case t: Term.PartialFunction =>
        "Partial function: A function defined for only some possible input values using case patterns."

      case t: Term.Tuple =>
        s"Tuple expression: '${t.text}' - An ordered collection of elements, possibly of different types."

      case t: Term.ForYield =>
        "For-yield expression: A sequence comprehension that builds a collection by iterating and transforming elements."

      case t: Term.For =>
        "For loop: Iterates over collections to perform side effects without building a result collection."

      case t: Term.New =>
        s"Object instantiation: '${t.text}' - Creates a new instance of a class."

      case t: Term.NewAnonymous =>
        "Anonymous class instantiation: Creates a new instance with additional or overridden members."

      case t: Term.Placeholder =>
        "Underscore placeholder (_): Represents an unnamed parameter in a partially applied function."

      case t: Term.Assign =>
        s"Assignment: '${t.text}' - Assigns a new value to a variable."

      case t: Term.Return =>
        "Return statement: Explicitly returns a value from a method."

      case t: Term.Throw =>
        "Throw statement: Raises an exception."

      case t: Term.Ascribe =>
        s"Type ascription: '${t.text}' - Explicitly specifies the type of an expression."

      case t: Term.Annotate =>
        "Annotated term: An expression with attached annotations."

      case t: Term.While =>
        "While loop: Repeatedly executes a block of code as long as a condition is true."

      case t: Term.Do =>
        "Do-while loop: Executes a block of code at least once, then repeats as long as a condition is true."

      case t: Term.Interpolate =>
        s"String interpolation: '${t.text}' - Embeds expressions within string literals using $$ or ${}."

      case t: Term.Xml =>
        "XML literal: Embedded XML content in Scala code."

      case t: Term.ApplyInfix =>
        s"Infix operator application: '${t.text}' - Calls a method using infix notation (a op b instead of a.op(b))."

      case t: Term.ApplyUnary =>
        s"Unary operator application: '${t.text}' - Applies a prefix operator to an expression."

      case t: Term.This =>
        s"'this' reference: '${t.text}' - References the current instance of a class or object."

      case t: Term.Super =>
        s"'super' reference: '${t.text}' - References the parent class implementation."

      case t: Term.Repeated =>
        s"Repeated argument: '${t.text}' - Expands a sequence as multiple arguments using the * operator."

      case t: Term.QuotedMacroExpr =>
        "Quoted macro expression: Represents code as data for metaprogramming in Scala 3."

      case t: Term.SplicedMacroExpr =>
        "Spliced macro expression: Inserts a computed expression into quoted code in Scala 3."

      case t: Term.EndMarker =>
        s"End marker: '${t.text}' - Explicitly marks the end of an indentation-based block in Scala 3."

      // Literals
      case t: Lit =>
        val typeStr = t match {
          case _: Lit.Int     => "integer"
          case _: Lit.Long    => "long integer"
          case _: Lit.Float   => "floating-point"
          case _: Lit.Double  => "double precision"
          case _: Lit.Char    => "character"
          case _: Lit.String  => "string"
          case _: Lit.Symbol  => "symbol"
          case _: Lit.Boolean => "boolean"
          case _: Lit.Unit    => "unit (empty tuple)"
          case _: Lit.Null    => "null"
          case _              => "literal"
        }
        s"$typeStr literal: '${t.text}'"

      // Pattern matching components
      case t: Pat.Var =>
        s"Pattern variable: '${t.text}' - Binds a matched value to a variable name."

      case t: Pat.Wildcard =>
        "Wildcard pattern (_): Matches any value without binding it to a name."

      case t: Pat.Bind =>
        s"Bind pattern: '${t.text}' - Binds a name to a matched pattern."

      case t: Pat.Alternative =>
        s"Alternative pattern: '${t.text}' - Matches either of two patterns using the | operator."

      case t: Pat.Tuple =>
        s"Tuple pattern: '${t.text}' - Matches a tuple and destructures its components."

      case t: Pat.Extract =>
        s"Extractor pattern: '${t.text}' - Matches using an unapply or unapplySeq method."

      case t: Pat.ExtractInfix =>
        s"Infix extractor pattern: '${t.text}' - Matches using an infix unapply operation."

      case t: Pat.Interpolate =>
        s"String interpolation pattern: '${t.text}' - Matches and extracts from string interpolations."

      case t: Pat.Typed =>
        s"Typed pattern: '${t.text}' - Matches a value of a specific type."

      case t: Pat.SeqWildcard =>
        "Sequence wildcard pattern (_*): Matches zero or more elements in a sequence."

      // Other specific constructs
      case t: Case =>
        "Case clause: A pattern and its associated action in a match expression or partial function."

      case t: Enumerator =>
        val description = t match {
          case _: Enumerator.Generator =>
            "Generator - Iterates over a collection"
          case _: Enumerator.Val =>
            "Value binding - Assigns a value for use in the comprehension"
          case _: Enumerator.Guard =>
            "Guard condition - Filters elements based on a condition"
          case _ => "Enumerator in a for expression"
        }
        s"$description: '${t.text}'"


      // For the body of a given implementation using Template.Body
      case t: Template.Body
          if parent.exists(p =>
            p.isInstanceOf[Template] &&
              (p.parent.exists(_.isInstanceOf[Defn.Given]) ||
                p.text.contains(" with "))
          ) =>
        "Given instance body: Contains the implementation methods and values for this given instance."

      // For the given...with {} syntax
      case t: Template
          if parent
            .exists(_.isInstanceOf[Defn.Given]) && t.text.contains(" with ") =>
        "Implementation block: The 'with' keyword here starts the body of a given instance, containing its implementation."

      case t: Template if t.text.contains(" with ") =>
        "Mixin composition: Combines traits using the 'with' keyword."

      case t: Template.Body =>
        "Implementation block: starts the body of implementation."

      case t: Template =>
        "Class body: Contains field and method definitions, initialized expressions, etc."

      case t: Self =>
        if (t.name.text.isEmpty)
          "Self type: This class requires the specified types."
        else
          s"Self type alias: '${t.name}' - Names the current instance and declares required types."

      case t: Init =>
        s"Parent constructor call: '${t.text}' - Initializes a parent class when extending or mixing in."

      // For entire source codebase selection
      case t: Source =>
        "You have selected the entire code. For more specific explanations, please place your cursor on a particular part of the code you'd like to understand."
      // Fallback for any other unhandled tree types
      case t: Tree =>
        val className = t.getClass.getName
        // val className = t.getClass.getName.split('.').last.split('$').last
        s"${className}: '${t.text}' - (This is a syntax element without a detailed explanation)"
    }
  }
}
