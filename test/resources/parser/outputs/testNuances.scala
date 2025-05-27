object Empty {


}


object Minimal {

  42
}


object First {

  1
}


object Second {

  2
}


object Functions {

  def add(x: Int, y: Int): Int = {
    (x + y)
  }
}


object NoParams {

  def constant(): Int = {
    42
  }
}


object Recursion {

  def fact(i: Int): Int = {
    (if((i < 2)) {
      1
    } else {
      (i * fact((i - 1)))
    })
  }
}


object Types {

  abstract class List
}


object CaseClasses {

  abstract class List

  case class Nil() extends List

  case class Cons(v: Int, v: List) extends List
}


object MultipleCase {

  abstract class Shape

  case class Circle(v: Int) extends Shape

  case class Rectangle(v: Int, v: Int) extends Shape

  case class Triangle(v: Int, v: Int, v: Int) extends Shape
}


object Literals {

  42;
  "Hello";
  true;
  false;
  ()
}


object SpecialStrings {

  "String with spaces";
  "String with escaped quotes";
  "String with \n and \t"
}


object Arithmetic {

  (1 + 2);
  (3 - 4);
  (5 * 6);
  (7 / 8);
  (9 % 10);
  -(11)
}


object Precedence {

  (1 + (2 * 3));
  ((1 + 2) * 3);
  (1 + (2 * 3));
  ((1 * 2) + (3 * 4))
}


object Booleaas {

  (true && false);
  (true || false);
  !(true);
  !((false && true))
}


object Comparison {

  (1 < 2);
  (3 <= 4);
  (5 == 6);
  (7 == 7)
}


object Strings {

  (("Hello" ++ " ") ++ "world!")
}


object IfExpressions {

  (if(true) {
    1
  } else {
    2
  });
  (if((1 < 2)) {
    "less"
  } else {
    "greater"
  })
}


object NestedIf {

  (if(true) {
    (if(false) {
      1
    } else {
      2
    })
  } else {
    3
  })
}


object Sequences {

  1;
  2;
  3
}


object ValDeclarations {

  val x: Int =
    1;
  val y: Int =
    2;
  (x + y)
}


object NestedVals {

  val x: Int =
    1;
  val y: Int =
    (x + 3);
  (x + y)
}


object SimpleMatch {

  abstract class List

  case class Nil() extends List

  case class Cons(v: Int, v: List) extends List

  val list: List =
    Nil();
  list match {
    case Nil() =>
      0
    case Cons(h, t) =>
      h
  }
}


object MultiMatch {

  abstract class Shape

  case class Circle(v: Int) extends Shape

  case class Rectangle(v: Int, v: Int) extends Shape

  val shape: Shape =
    Circle(5);
  shape match {
    case Circle(r) =>
      r
    case Rectangle(w, h) =>
      (w * h)
  }
}


object WildcardMatch {

  abstract class List

  case class Nil() extends List

  case class Cons(v: Int, v: List) extends List

  def head(l: List): Int = {
    l match {
      case Nil() =>
        error("head(Nil)")
      case Cons(h, _) =>
        h
    }
  }
}


object LiteralMatch {

  val x: Int =
    1;
  x match {
    case 0 =>
      "zero"
    case 1 =>
      "one"
    case _ =>
      "other"
  }
}


object Errors {

  def divide(a: Int, b: Int): Int = {
    (if((b == 0)) {
      error("Division by zero")
    } else {
      (a / b)
    })
  }

  error("This is an error message")
}


object Qualified {

  def func(x: Int): Int = {
    (x + 1)
  }
}


object Caller {

  Qualified.func(42)
}


object AllPrecedence {

  val result: Boolean =
    ((!(false) && (1 < (2 + (3 * 4)))) || (5 == 6));
  result
}


object Parenthesized {

  ((1 + 2) * (3 + 4));
  (((1 + 2) * 3) + 4);
  (1 + (2 * (3 + 4)))
}


object SequenceVal {

  val x: Int =
    1;
  val y: Int =
    2;
  x;
  y;
  (x + y)
}


object LegalNesting {

  (if(true) {
    (
      val x: Int =
        1;
      x
    )
  } else {
    2
  });
  val y: Int =
    (if(false) {
      3
    } else {
      4
    });
  y
}


object ValAsOperand {

  val x: Int =
    2;
  (x + 1)
}


object MatchAsBinaryOperand {

  x match {
    case 1 =>
      2
  }
}


object Comprehensive {

  abstract class List

  case class Nil() extends List

  case class Cons(v: Int, v: List) extends List

  def length(l: List): Int = {
    l match {
      case Nil() =>
        0
      case Cons(h, t) =>
        (1 + length(t))
    }
  }

  def range(from: Int, to: Int): List = {
    (if((to < from)) {
      Nil()
    } else {
      Cons(from, range((from + 1), to))
    })
  }

  val list: List =
    range(1, 10);
  length(list)
}


object ComplexExpression {

  val x: Int =
    1;
  val y: Int =
    2;
  (if((x < y)) {
    (
      val z: String =
        "x is less than y";
      Std.printString(z);
      x
    )
  } else {
    (
      val w: String =
        "x is not less than y";
      Std.printString(w);
      y
    )
  })
}


object NestedMatch1 {

  abstract class Tree

  case class Leaf(v: Int) extends Tree

  case class Node(v: Tree, v: Tree) extends Tree

  val tree: Tree =
    Node(Leaf(1), Leaf(2));
  tree match {
    case Leaf(v) =>
      v
    case Node(l, r) =>
      l match {
        case Leaf(v1) =>
          v1
        case Node(_, _) =>
          0
      }
  }
}


object NestedMatch2 {

  abstract class Expr

  case class Num(v: Int) extends Expr

  case class Add(v: Expr, v: Expr) extends Expr

  case class Mul(v: Expr, v: Expr) extends Expr

  def eval(e: Expr): Int = {
    e match {
      case Num(n) =>
        n
      case Add(e1, e2) =>
        (eval(e1) + eval(e2))
      case Mul(e1, e2) =>
        e1 match {
          case Num(n) =>
            (n * eval(e2))
          case _ =>
            e2 match {
              case Num(n) =>
                (eval(e1) * n)
              case _ =>
                (eval(e1) * eval(e2))
            }
        }
    }
  }
}


object MatchInMatchCase {

  abstract class Option

  case class Some(v: Int) extends Option

  case class None() extends Option

  def process(o1: Option, o2: Option): Int = {
    o1 match {
      case Some(v1) =>
        o2 match {
          case Some(v2) =>
            (v1 + v2)
          case None() =>
            v1
        }
      case None() =>
        o2 match {
          case Some(v2) =>
            v2
          case None() =>
            0
        }
    }
  }
}


object UnitMatch {

  def testUnit(x: Unit): String = {
    x match {
      case () =>
        "unit"
    }
  }

  testUnit(())
}


object EmptyBodyMatch {

  abstract class Result

  case class Success(v: Int) extends Result

  case class Failure() extends Result

  val r: Result =
    Success(42);
  val result: Int =
    r match {
      case Success(v) =>
        v
      case Failure() =>
        (
          error("Operation failed");
          0
        )
    };
  1
}


object ModuleA {

  abstract class Tree

  case class Leaf(v: Int) extends Tree

  case class Node(v: Tree, v: Tree) extends Tree
}


object QualifiedMatch {

  def process(t: ModuleA.Tree): Int = {
    t match {
      case ModuleA.Leaf(v) =>
        v
      case ModuleA.Node(l, r) =>
        1
    }
  }
}


object DeepMatching {

  abstract class Tree

  case class Leaf(v: Int) extends Tree

  case class Node(v: Tree, v: Tree) extends Tree

  def countNodes(t: Tree): Int = {
    t match {
      case Leaf(_) =>
        1
      case Node(Leaf(_), Leaf(_)) =>
        3
      case Node(Node(_, _), Leaf(_)) =>
        4
      case Node(Leaf(_), Node(_, _)) =>
        4
      case Node(Node(_, _), Node(_, _)) =>
        5
    }
  }
}


object NestedWildcards {

  abstract class Pair

  case class IntPair(v: Int, v: Int) extends Pair

  case class NestedPair(v: Pair, v: Pair) extends Pair

  def analyze(p: Pair): String = {
    p match {
      case IntPair(a, b) =>
        "IntPair"
      case NestedPair(IntPair(_, _), _) =>
        "Nested with IntPair first"
      case NestedPair(_, IntPair(_, _)) =>
        "Nested with IntPair second"
      case NestedPair(_, _) =>
        "Fully nested"
    }
  }
}


object SequentialMatches {

  abstract class Option

  case class Some(v: Int) extends Option

  case class None() extends Option

  val a: Option =
    Some(1);
  val b: Option =
    None();
  a match {
    case Some(v) =>
      v
    case None() =>
      0
  };
  b match {
    case Some(v) =>
      v
    case None() =>
      0
  }
}


object EmptyParensMatch {

  abstract class Boolea

  case class True() extends Boolea

  case class False() extends Boolea

  def not(b: Boolea): Boolea = {
    b match {
      case True() =>
        False()
      case False() =>
        True()
    }
  }
}


object BooleanLiteralMatch {

  def describe(b: Boolea): String = {
    b match {
      case true =>
        "It's true"
      case false =>
        "It's false"
    }
  }
}


object MatchWithIf {

  abstract class Expr

  case class Value(v: Int) extends Expr

  case class Add(v: Expr, v: Expr) extends Expr

  def eval(e: Expr): Int = {
    (if(true) {
      e match {
        case Value(n) =>
          n
        case Add(e1, e2) =>
          (eval(e1) + eval(e2))
      }
    } else {
      0
    })
  }
}


object MatchWithVal {

  abstract class Option

  case class Some(v: Int) extends Option

  case class None() extends Option

  val opt: Option =
    Some(42);
  val x: Int =
    10;
  opt match {
    case Some(v) =>
      (v + x)
    case None() =>
      x
  }
}


object MatchAsReturn {

  abstract class Tree

  case class Leaf(v: Int) extends Tree

  case class Node(v: Tree, v: Tree) extends Tree

  def height(t: Tree): Int = {
    t match {
      case Leaf(_) =>
        1
      case Node(l, r) =>
        (
          val lh: Int =
            height(l);
          val rh: Int =
            height(r);
          (if((rh < lh)) {
            (lh + 1)
          } else {
            (rh + 1)
          })
        )
    }
  }
}


object MatchWithError {

  abstract class Result

  case class Success(v: Int) extends Result

  case class Failure(v: String) extends Result

  def process(r: Result): Int = {
    r match {
      case Success(v) =>
        v
      case Failure(msg) =>
        error(msg)
    }
  }
}


object MatchWithSequence {

  abstract class Command

  case class Print(v: String) extends Command

  case class Calculate(v: Int, v: Int) extends Command

  def execute(cmd: Command): Int = {
    cmd match {
      case Print(msg) =>
        (
          Std.printString(msg);
          0
        )
      case Calculate(a, b) =>
        (
          val result: Int =
            (a + b);
          Std.printInt(result);
          result
        )
    }
  }
}


object MatchWithSemicolon {

  abstract class Option

  case class Some(v: Int) extends Option

  case class None() extends Option

  val opt: Option =
    Some(1);
  opt match {
    case Some(v) =>
      v
    case None() =>
      0
  };
  Std.printString("Match completed")
}


object MultipleMatchesSameValue {

  abstract class Tree

  case class Leaf(v: Int) extends Tree

  case class Node(v: Tree, v: Tree) extends Tree

  def process(t: Tree): Int = {
    val isLeaf: Boolean =
      t match {
        case Leaf(_) =>
          true
        case Node(_, _) =>
          false
      };
    val value: Int =
      t match {
        case Leaf(v) =>
          v
        case Node(l, r) =>
          (process(l) + process(r))
      };
    value
  }
}
