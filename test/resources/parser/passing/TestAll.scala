// basic_structure.amy
object BasicStructure {
  abstract class EmptyType
  case class Constructor1() extends EmptyType
  case class Constructor2(x: Int, y: Boolean) extends EmptyType
  
  def simpleFunction(x: Int): Int = {
    x
  }

  def emptyFunction(): Unit = {
    ()
  }
  
  // Main expression
  simpleFunction(42)
}

object EmptyObject {
  // No definitions or expressions
}

object ExpressionOnly {
  // Only an expression without definitions
  "Hello world!"
}

// literals.amy
object LiteralTests {
  // Integer literals
  val i1: Int = 0;
  val i2: Int = 123456789;
  
  // Boolean literals
  val b1: Boolean = true;
  val b2: Boolean = false;
  
  // String literals
  val s1: String = "";
  val s2: String = "Hello world!";
  val s3: String = "Special chars: !@#$%^&*()_+-=[]{}|;:,.<>/?";
  
  // Unit literal
  val u1: Unit = ();
  
  // Testing all literals
  i1; i2; b1; b2; s1; s2; s3; u1
}

// operators.amy
object OperatorTests {
  // Arithmetic operators
  def testArithmetic(a: Int, b: Int): Int = {
    val add: Int = a + b;
    val sub: Int = a - b;
    val mul: Int = a * b;
    val div: Int = a / b;
    val mod: Int = a % b;
    val neg: Int = -a;
    
    // Testing precedence
    val prec1: Int = a + b * a;
    val prec2: Int = (a + b) * a;
    val prec3: Int = a * b + a;
    val prec4: Int = a * (b + a);
    
    // Testing left associativity
    val assoc1: Int = a + b + a;  // Should be (a + b) + a
    val assoc2: Int = a - b - a;  // Should be (a - b) - a
    
    // Multiple operations
    val complex: Int = a + b * a - (b / a) % b + -a;
    
    complex
  }
  
  // Boolean operators
  def testBoolean(a: Boolean, b: Boolean): Boolean = {
    val and: Boolean = a && b;
    val or: Boolean = a || b;
    val not: Boolean = !a;
    
    // Testing precedence
    val prec1: Boolean = a || b && a;  // b && a evaluated first
    val prec2: Boolean = (a || b) && a;
    val prec3: Boolean = a && b || a;  // a && b evaluated first
    val prec4: Boolean = a && (b || a);
    
    // Testing short-circuit
    val short1: Boolean = true || error("This should not be evaluated");
    val short2: Boolean = false && error("This should not be evaluated");
    
    // Testing complex expressions
    val complex: Boolean = !a && (b || !a) && !(a || b);
    
    complex
  }
  
  // Comparison operators
  def testComparison(a: Int, b: Int): Boolean = {
    val lt: Boolean = a < b;
    val lte: Boolean = a <= b;
    val eq: Boolean = a == b;
    
    // Testing precedence
    val prec1: Boolean = a + b < a * b;
    val prec2: Boolean = (a + b) < (a * b);
    val prec3: Boolean = a < b || a == b;  // < evaluated before ||
    
    // Testing complex expressions
    val complex: Boolean = (a < b) && (a + b <= a * b) || (a == b);
    
    complex
  }
  
  // String concatenation
  def testStrings(a: String, b: String): String = {
    val concat: String = a ++ b;
    
    // Testing precedence with other operations
    val complex: String = a ++ b ++ "suffix";  // Should be (a ++ b) ++ "suffix"
    
    complex
  }
  
  // Testing operator precedence across types
  def testMixed(): Boolean = {
    val i1: Int = 5;
    val i2: Int = 10;
    val b1: Boolean = true;
    val b2: Boolean = false;
    val s1: String = "Hello";
    val s2: String = "World";
    
    // Complex expression mixing different operators
    val result: Boolean = i1 < i2 && b1 || i1 == i2 && !b2;
    
    result
  }
  
  testMixed()
}

// control_flow.amy
object ControlFlowTests {
  // If expressions
  def testIf(a: Int, b: Int): Int = {
    val simple: Int = if (a < b) { a } else { b };
    
    // Nested if
    val nested: Int = if (a < b) {
      if (a < 0) { -a } else { a }
    } else {
      if (b < 0) { -b } else { b }
    };
    
    // If with complex condition
    val complex: Int = if (a < b && 0 < a || b < 0) {
      a + b
    } else {
      a - b
    };
    
    // If returning different types (not valid in Amy)
    // val invalid: Int = if (a < b) { a } else { true };
    
    complex
  }
  
  // Match expressions
  abstract class Tree
  case class Leaf(value: Int) extends Tree
  case class Node(left: Tree, right: Tree) extends Tree
  
  def testMatch(t: Tree): Int = {
    // Simple match
    val simple: Int = t match {
      case Leaf(v) => v
      case Node(l, r) => 0
    };
    
    // Match with wildcard
    val withWildcard: Int = t match {
      case Leaf(_) => 0
      case Node(l, r) => 1
    };
    
    // Nested match
    val nested: Int = t match {
      case Leaf(v) => v
      case Node(l, r) => l match {
        case Leaf(v) => v
        case Node(_, _) => r match {
          case Leaf(v) => v
          case Node(_, _) => 0
        }
      }
    };
    
    // Match with literals and variable patterns
    val v: Int = 42;
    val withLiterals: Int = v match {
      case 0 => 0
      case 42 => 1
      case x => x  // variable pattern
    };
    
    // Match with complex scrutinee
    val complex: Int = (if (simple < 0) { Leaf(-simple) } else { t }) match {
      case Leaf(v) => v
      case Node(l, r) => 0
    };
    
    complex
  }
  
  // Error handling
  def testError(a: Int): Int = {
    if (a < 0) {
      error("Negative input: " ++ Std.intToString(a))
    } else {
      a
    }
  }
  
  // Sequence expressions
  def testSequence(): Int = {
    val a: Int = 5;
    val b: Int = 10;
    
    // Multiple sequences
    a;
    b;
    a + b;
    
    // Sequence with side effects
    Std.printInt(a);
    Std.printInt(b);
    a + b
  }
  
  testSequence()
}


// functions_and_adts.amy
object FunctionsAndADTs {
  // ADT definitions
  abstract class List
  case class Nil() extends List
  case class Cons(head: Int, tail: List) extends List
  
  abstract class Option
  case class None() extends Option
  case class Some(value: Int) extends Option
  
  abstract class Tree
  case class Leaf(value: Int) extends Tree
  case class Node(value: Int, left: Tree, right: Tree) extends Tree
  
  // Basic functions
  def length(list: List): Int = {
    list match {
      case Nil() => 0
      case Cons(h, t) => 1 + length(t)
    }
  }
  
  def sum(list: List): Int = {
    list match {
      case Nil() => 0
      case Cons(h, t) => h + sum(t)
    }
  }
  
  // Recursive functions
  def factorial(n: Int): Int = {
    if (n <= 1) { 1 }
    else { n * factorial(n-1) }
  }
  
  def fibonacci(n: Int): Int = {
    if (n <= 0) { 0 }
    else {
        if (n == 1) { 1 } 
        else { fibonacci(n-1) + fibonacci(n-2) }
    }
  }
  
  // Higher-order function simulation (Amy doesn't directly support this)
  def applyOperation(a: Int, b: Int, operation: Int): Int = {
    if (operation == 0) { a + b }
    else {
        if (operation == 1) { a - b }
        else {
            if (operation == 2) { a * b }
            else {
                if (operation == 3) { a / b }
                else { error("Invalid operation") }
            }
        }
    }
  }
  
  // Function with multiple parameters
  def max(a: Int, b: Int): Int = {
    if (b < a) { a } else { b }
  }
  
  // Function returning ADTs
  def find(list: List, value: Int): Option = {
    list match {
      case Nil() => None()
      case Cons(h, t) => if (h == value) { Some(h) } else { find(t, value) }
    }
  }
  
  // Tree operations
  def insert(tree: Tree, value: Int): Tree = {
    tree match {
      case Leaf(v) => 
        if (value < v) { Node(v, Leaf(value), Leaf(0)) }
        else { Node(v, Leaf(0), Leaf(value)) }
      case Node(v, l, r) =>
        if (value < v) { Node(v, insert(l, value), r) }
        else { Node(v, l, insert(r, value)) }
    }
  }
  
  def contains(tree: Tree, value: Int): Boolean = {
    tree match {
      case Leaf(v) => v == value
      case Node(v, l, r) => 
        if (v == value) { true }
        else {
            if (value < v) { contains(l, value) }
            else { contains(r, value) }
        }
    }
  }
  
  // Create and use ADTs
  val emptyList: List = Nil();
  val list: List = Cons(1, Cons(2, Cons(3, Nil())));
  val emptyTree: Tree = Leaf(0);
  
  val listLength: Int = length(list);
  val listSum: Int = sum(list);
  val fact5: Int = factorial(5);
  val fib6: Int = fibonacci(6);
  
  // Building a tree
  val tree: Tree = insert(insert(insert(emptyTree, 5), 3), 7);
  
  // Testing everything
  contains(tree, 3)
}

// module_and_qualified.amy
// First module with basic definitions
object Module1 {
  abstract class List
  case class Nil() extends List
  case class Cons(head: Int, tail: List) extends List
  
  def length(list: List): Int = {
    list match {
      case Nil() => 0
      case Cons(h, t) => 1 + length(t)
    }
  }
  
  def sum(list: List): Int = {
    list match {
      case Nil() => 0
      case Cons(h, t) => h + sum(t)
    }
  }
  
  // Create a list for other modules to use
  val empty: List = Nil();
  val numbers: List = Cons(1, Cons(2, Cons(3, Nil())));
  1
}

// Second module that refers to definitions in first module
object Module2 {
  def useModule1(): Int = {
    // Using qualified names to refer to Module1's definitions
    val list: Module1.List = Module1.Cons(1, Module1.Cons(4, Module1.Nil()));
    val length: Int = Module1.length(list);
    val sum: Int = Module1.sum(list);
    
    // Match on ADT from another module
    val first: Int = list match {
      case Module1.Nil() => 0
      case Module1.Cons(h, _) => h
    };
    
    first + length + sum
  }
  
  useModule1()
}

// Self-referring module
object Recursive {
  abstract class Tree
  case class Empty() extends Tree
  case class Node(value: Int, left: Tree, right: Tree) extends Tree
  
  // Reference own definitions with and without qualification
  def insert(tree: Tree, value: Int): Tree = {
    tree match {
      case Empty() => Node(value, Empty(), Empty())
      case Recursive.Node(v, l, r) => // Qualified reference to own constructor
        if (value < v) {
          Node(v, insert(l, value), r)  // Unqualified reference 
        } else {
          Recursive.Node(v, l, Recursive.insert(r, value))  // Mixed qualified/unqualified
        }
    }
  }
  
  // Create and use a tree
  val emptyTree: Tree = Empty();
  val tree: Tree = insert(insert(insert(emptyTree, 5), 3), 7);
  
  // Call own function
  insert(tree, 9)
}

// edge_cases.amy
object EdgeCases {
  // Empty function body
  def emptyBody(): Unit = {
    ()
  }
  
  // Functions with no parameters
  def noParams(): Int = {
    42
  }
  
  // Empty class with no constructors
  abstract class EmptyClass
  
  // Constructor with no fields
  case class EmptyConstructor() extends EmptyClass
  
  // Nested if expressions
  def nestedIf(a: Int): Int = {
    if (a < 0) {
      if (a < -10) {
        -100
      } else {
        -10
      }
    } else {
      if (10 < a) {
        100
      } else {
        10
      }
    }
  }
  
  // Nested match expressions
  def nestedMatch(a: Int): Int = {
    a match {
      case 0 => 0
      case 1 => 1
      case n => n match {
        case 2 => 2
        case 3 => 3
        case m => m * 10
      }
    }
  }
  
  // Complex expressions with many operators
  def complexExpr(a: Int, b: Int): Int = {
    a + b * 2 - 3 / 4 % 5 + -a * (b + 1) / 2
  }
  
  // Long sequences
  def longSequence(): Int = {
    val a: Int = 1;
    val b: Int = 2;
    val c: Int = 3;
    val d: Int = 4;
    val e: Int = 5;
    a;
    b;
    c;
    d;
    e
  }
  
  // Parenthesized expressions
  def parenthesized(a: Int, b: Int): Int = {
    (a + b) * (a - b) / ((a * b) % (a / b))
  }
  
  // Many nested parentheses
  def manyParens(a: Int): Int = {
    (((((a + 1) * 2) - 3) / 4) % 5)
  }
  
  // Complex boolean expressions
  def booleanExpr(a: Boolean, b: Boolean): Boolean = {
    !a && b || a && !b || !(a || b) && (a || !b)
  }
  
  // Multiple bindings in match patterns
  abstract class Pair
  case class P(x: Int, y: Int) extends Pair
  
  def matchMultiple(p: Pair): Int = {
    p match {
      case P(x, y) => x + y
    }
  }
  
  // Using underscore in patterns
  def matchUnderscores(p: Pair): Int = {
    p match {
      case P(x, _) => x
    }
  }
  
  // Using literals in patterns
  def matchLiterals(a: Int): Boolean = {
    a match {
      case 0 => true
      case 1 => false
      case _ => true
    }
  }
  
  // Edge case for operator precedence
  def operatorEdge(): Int = {
    1 + 2 * 3 + 4 * 5 + 6
  }
  
  // Max recursion depth test
  def countdown(n: Int): Int = {
    if (n <= 0) {
      0
    } else {
      countdown(n-1)
    }
  }
  
  // Test using every operator in one expression
  def allOperators(a: Int, b: Int, c: Boolean, d: Boolean, s: String): Boolean = {
    (a + b - a * b / a % b < a && a <= b) || 
    (c && d || !c || !d) && 
    (a == b) && 
    (s ++ "test" == s)
  }
  
  // Test semicolon at various positions
  def testSemicolons(): Int = {
    val a: Int = 1;
    (val b: Int = 2; b + a)
  }
  
  testSemicolons()
}

// precedence.amy
object OperatorPrecedence {
  // Test all operator precedence levels systematically
  
  // Level 1: val, ;
  def testLevel1(): Int = {
    val x: Int = 1;
    val y: Int = 2;
    x; y
  }
  
  // Level 2: if, match
  def testLevel2(a: Int): Int = {
    if (0 < a) { 1 } else { 0 }
    match {
      case 1 => 10
      case 0 => 0
      case _ => -1
    }
  }
  
  // Level 3: ||
  def testLevel3(a: Boolean, b: Boolean, c: Boolean): Boolean = {
    a || b || c
  }
  
  // Level 4: &&
  def testLevel4(a: Boolean, b: Boolean, c: Boolean): Boolean = {
    a && b && c
  }
  
  // Level 5: ==
  def testLevel5(a: Int, b: Int, c: Int): Boolean = {
    a == b == c == a  // Should be ((a == b) == c) == a
  }
  
  // Level 6: <, <=
  def testLevel6(a: Int, b: Int, c: Int): Boolean = {
    a < b <= c        // Should be (a < b) && (b <= c)
  }
  
  // Level 7: +, -, ++
  def testLevel7(a: Int, b: Int, c: Int, s1: String, s2: String): Int = {
    a + b - c
  }
  
  def testLevel7String(s1: String, s2: String, s3: String): String = {
    s1 ++ s2 ++ s3    // Should be (s1 ++ s2) ++ s3
  }
  
  // Level 8: *, /, %
  def testLevel8(a: Int, b: Int, c: Int): Int = {
    a * b / c % a     // Should be ((a * b) / c) % a
  }
  
  // Level 9: Unary -, !
  def testLevel9(a: Int, b: Boolean): Int = {
    -a                // Just unary minus
  }
  
  def testLevel9Bool(b: Boolean): Boolean = {
    !b                // Just not
  }
  
  // Level 10: error, calls, variables, literals
  def testLevel10(): Int = {
    error("test")
  }
  
  // Cross-level precedence tests
  
  // Test level 3 (||) vs level 4 (&&)
  def test3vs4(a: Boolean, b: Boolean, c: Boolean): Boolean = {
    a || b && c       // Should be a || (b && c)
  }
  
  // Test level 4 (&&) vs level 5 (==)
  def test4vs5(a: Boolean, b: Int, c: Int): Boolean = {
    a && b == c       // Should be a && (b == c)
  }
  
  // Test level 5 (==) vs level 6 (<, <=)
  def test5vs6(a: Int, b: Int, c: Int): Boolean = {
    a == b < c        // Should be a == (b < c)
  }
  
  // Test level 6 (<, <=) vs level 7 (+, -, ++)
  def test6vs7(a: Int, b: Int, c: Int): Boolean = {
    a < b + c         // Should be a < (b + c)
  }
  
  // Test level 7 (+, -, ++) vs level 8 (*, /, %)
  def test7vs8(a: Int, b: Int, c: Int): Int = {
    a + b * c         // Should be a + (b * c)
  }
  
  // Test level 8 (*, /, %) vs level 9 (Unary -, !)
  def test8vs9(a: Int, b: Int): Int = {
    a * -b            // Should be a * (-b)
  }
  
  def test8vs9Bool(a: Int, b: Boolean): Int = {
    a * (if (!b) { 1 } else { 0 })   // Using ! inside if
  }
  
  // Test level 9 (Unary -, !) vs level 10 (calls, etc)
  def test9vs10(a: Int): Int = {
    -(a + 1)          // Should be -(a + 1)
  }
  
  // Complex mixed precedence tests
  def complexMixed1(a: Int, b: Int, c: Boolean, d: Boolean): Boolean = {
    a + b * a < b / a && c || !d     // Should be ((a + (b * a)) < (b / a) && c) || !d
  }
  
  def complexMixed2(a: Int, b: Int, c: Boolean, d: Boolean): Boolean = {
    a + b == a * b && c || a < b && d // Should be (((a + b) == (a * b)) && c) || ((a < b) && d)
  }
  
  complexMixed2(1, 2, true, false)
}

// comments_and_all.amy
object CommentsAndAll {
  // This is a single line comment
  
  /* This is 
     a multi-line
     comment */
  
  /* Comments with special chars: !@#$%^&*()_+-=[]{}|;:,.<>/? */
  
  /* Comment with /* characters (nested comments not supported) */

  abstract class Data // Comment after class
  
  case class Value(x: Int /* comment in parameter */) extends Data // Comment after constructor
  
  def /* comment before name */ testFunction(
    a: Int, // Comment after parameter
    b: Int
  ): Int = { // Comment after return type
    /* Comment at start of function body */
    val x: Int = a + b; // Comment after statement
    
    // If with comments
    if (0 < x) { // Comment after condition
      x // Comment after then expression
    } else {
      -x // Comment after else expression
    }
    
    /* Final return value */
  }
  
  // Testing all features of Amy in one function
  def allFeatures(input: Int): Int = {
    // Local variables
    val isPositive: Boolean = 0 < input;
    val absValue: Int = if (isPositive) { input } else { -input };
    
    // Create an ADT value
    val data: Data = Value(absValue);
    
    // Pattern matching
    val result: Int = data match {
      case Value(x) => x * 2
    };
    
    // Arithmetic operations with all operators
    val computed: Int = result + 10 - 5 * 2 / 2 % 3;
    
    // Boolean operations
    val check: Boolean = 0 < computed && (computed < 100 || computed == 50);
    
    // String operations
    val message: String = "Result: " ++ Std.intToString(computed);
    
    // Sequence and side effects
    Std.printString(message);
    
    // If-else with complex condition
    if (check && isPositive || 100 < computed) {
      // Recursive call
      allFeatures(computed / 2)
    } else {
      // Error handling (conditional)
      if (computed < 0) {
        error("Negative result: " ++ Std.intToString(computed))
      } else {
        computed
      }
    }
  }
  
  // Main expression
  allFeatures(42)
}