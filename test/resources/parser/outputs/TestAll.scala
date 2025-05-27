object BasicStructure {

  abstract class EmptyType

  case class Constructor1() extends EmptyType

  case class Constructor2(v: Int, v: Boolean) extends EmptyType

  def simpleFunction(x: Int): Int = {
    x
  }

  def emptyFunction(): Unit = {
    ()
  }

  simpleFunction(42)
}


object EmptyObject {


}


object ExpressionOnly {

  "Hello world!"
}


object LiteralTests {

  val i1: Int =
    0;
  val i2: Int =
    123456789;
  val b1: Boolean =
    true;
  val b2: Boolean =
    false;
  val s1: String =
    "";
  val s2: String =
    "Hello world!";
  val s3: String =
    "Special chars: !@#$%^&*()_+-=[]{}|;:,.<>/?";
  val u1: Unit =
    ();
  i1;
  i2;
  b1;
  b2;
  s1;
  s2;
  s3;
  u1
}


object OperatorTests {

  def testArithmetic(a: Int, b: Int): Int = {
    val add: Int =
      (a + b);
    val sub: Int =
      (a - b);
    val mul: Int =
      (a * b);
    val div: Int =
      (a / b);
    val mod: Int =
      (a % b);
    val neg: Int =
      -(a);
    val prec1: Int =
      (a + (b * a));
    val prec2: Int =
      ((a + b) * a);
    val prec3: Int =
      ((a * b) + a);
    val prec4: Int =
      (a * (b + a));
    val assoc1: Int =
      ((a + b) + a);
    val assoc2: Int =
      ((a - b) - a);
    val complex: Int =
      (((a + (b * a)) - ((b / a) % b)) + -(a));
    complex
  }

  def testBoolean(a: Boolean, b: Boolean): Boolean = {
    val and: Boolean =
      (a && b);
    val or: Boolean =
      (a || b);
    val not: Boolean =
      !(a);
    val prec1: Boolean =
      (a || (b && a));
    val prec2: Boolean =
      ((a || b) && a);
    val prec3: Boolean =
      ((a && b) || a);
    val prec4: Boolean =
      (a && (b || a));
    val short1: Boolean =
      (true || error("This should not be evaluated"));
    val short2: Boolean =
      (false && error("This should not be evaluated"));
    val complex: Boolean =
      ((!(a) && (b || !(a))) && !((a || b)));
    complex
  }

  def testComparison(a: Int, b: Int): Boolean = {
    val lt: Boolean =
      (a < b);
    val lte: Boolean =
      (a <= b);
    val eq: Boolean =
      (a == b);
    val prec1: Boolean =
      ((a + b) < (a * b));
    val prec2: Boolean =
      ((a + b) < (a * b));
    val prec3: Boolean =
      ((a < b) || (a == b));
    val complex: Boolean =
      (((a < b) && ((a + b) <= (a * b))) || (a == b));
    complex
  }

  def testStrings(a: String, b: String): String = {
    val concat: String =
      (a ++ b);
    val complex: String =
      ((a ++ b) ++ "suffix");
    complex
  }

  def testMixed(): Boolean = {
    val i1: Int =
      5;
    val i2: Int =
      10;
    val b1: Boolean =
      true;
    val b2: Boolean =
      false;
    val s1: String =
      "Hello";
    val s2: String =
      "World";
    val result: Boolean =
      (((i1 < i2) && b1) || ((i1 == i2) && !(b2)));
    result
  }

  testMixed()
}


object ControlFlowTests {

  def testIf(a: Int, b: Int): Int = {
    val simple: Int =
      (if((a < b)) {
        a
      } else {
        b
      });
    val nested: Int =
      (if((a < b)) {
        (if((a < 0)) {
          -(a)
        } else {
          a
        })
      } else {
        (if((b < 0)) {
          -(b)
        } else {
          b
        })
      });
    val complex: Int =
      (if((((a < b) && (0 < a)) || (b < 0))) {
        (a + b)
      } else {
        (a - b)
      });
    complex
  }

  abstract class Tree

  case class Leaf(v: Int) extends Tree

  case class Node(v: Tree, v: Tree) extends Tree

  def testMatch(t: Tree): Int = {
    val simple: Int =
      t match {
        case Leaf(v) =>
          v
        case Node(l, r) =>
          0
      };
    val withWildcard: Int =
      t match {
        case Leaf(_) =>
          0
        case Node(l, r) =>
          1
      };
    val nested: Int =
      t match {
        case Leaf(v) =>
          v
        case Node(l, r) =>
          l match {
            case Leaf(v) =>
              v
            case Node(_, _) =>
              r match {
                case Leaf(v) =>
                  v
                case Node(_, _) =>
                  0
              }
          }
      };
    val v: Int =
      42;
    val withLiterals: Int =
      v match {
        case 0 =>
          0
        case 42 =>
          1
        case x =>
          x
      };
    val complex: Int =
      (if((simple < 0)) {
        Leaf(-(simple))
      } else {
        t
      }) match {
        case Leaf(v) =>
          v
        case Node(l, r) =>
          0
      };
    complex
  }

  def testError(a: Int): Int = {
    (if((a < 0)) {
      error(("Negative input: " ++ Std.intToString(a)))
    } else {
      a
    })
  }

  def testSequence(): Int = {
    val a: Int =
      5;
    val b: Int =
      10;
    a;
    b;
    (a + b);
    Std.printInt(a);
    Std.printInt(b);
    (a + b)
  }

  testSequence()
}


object FunctionsAndADTs {

  abstract class List

  case class Nil() extends List

  case class Cons(v: Int, v: List) extends List

  abstract class Option

  case class None() extends Option

  case class Some(v: Int) extends Option

  abstract class Tree

  case class Leaf(v: Int) extends Tree

  case class Node(v: Int, v: Tree, v: Tree) extends Tree

  def length(list: List): Int = {
    list match {
      case Nil() =>
        0
      case Cons(h, t) =>
        (1 + length(t))
    }
  }

  def sum(list: List): Int = {
    list match {
      case Nil() =>
        0
      case Cons(h, t) =>
        (h + sum(t))
    }
  }

  def factorial(n: Int): Int = {
    (if((n <= 1)) {
      1
    } else {
      (n * factorial((n - 1)))
    })
  }

  def fibonacci(n: Int): Int = {
    (if((n <= 0)) {
      0
    } else {
      (if((n == 1)) {
        1
      } else {
        (fibonacci((n - 1)) + fibonacci((n - 2)))
      })
    })
  }

  def applyOperation(a: Int, b: Int, operation: Int): Int = {
    (if((operation == 0)) {
      (a + b)
    } else {
      (if((operation == 1)) {
        (a - b)
      } else {
        (if((operation == 2)) {
          (a * b)
        } else {
          (if((operation == 3)) {
            (a / b)
          } else {
            error("Invalid operation")
          })
        })
      })
    })
  }

  def max(a: Int, b: Int): Int = {
    (if((b < a)) {
      a
    } else {
      b
    })
  }

  def find(list: List, value: Int): Option = {
    list match {
      case Nil() =>
        None()
      case Cons(h, t) =>
        (if((h == value)) {
          Some(h)
        } else {
          find(t, value)
        })
    }
  }

  def insert(tree: Tree, value: Int): Tree = {
    tree match {
      case Leaf(v) =>
        (if((value < v)) {
          Node(v, Leaf(value), Leaf(0))
        } else {
          Node(v, Leaf(0), Leaf(value))
        })
      case Node(v, l, r) =>
        (if((value < v)) {
          Node(v, insert(l, value), r)
        } else {
          Node(v, l, insert(r, value))
        })
    }
  }

  def contains(tree: Tree, value: Int): Boolean = {
    tree match {
      case Leaf(v) =>
        (v == value)
      case Node(v, l, r) =>
        (if((v == value)) {
          true
        } else {
          (if((value < v)) {
            contains(l, value)
          } else {
            contains(r, value)
          })
        })
    }
  }

  val emptyList: List =
    Nil();
  val list: List =
    Cons(1, Cons(2, Cons(3, Nil())));
  val emptyTree: Tree =
    Leaf(0);
  val listLength: Int =
    length(list);
  val listSum: Int =
    sum(list);
  val fact5: Int =
    factorial(5);
  val fib6: Int =
    fibonacci(6);
  val tree: Tree =
    insert(insert(insert(emptyTree, 5), 3), 7);
  contains(tree, 3)
}


object Module1 {

  abstract class List

  case class Nil() extends List

  case class Cons(v: Int, v: List) extends List

  def length(list: List): Int = {
    list match {
      case Nil() =>
        0
      case Cons(h, t) =>
        (1 + length(t))
    }
  }

  def sum(list: List): Int = {
    list match {
      case Nil() =>
        0
      case Cons(h, t) =>
        (h + sum(t))
    }
  }

  val empty: List =
    Nil();
  val numbers: List =
    Cons(1, Cons(2, Cons(3, Nil())));
  1
}


object Module2 {

  def useModule1(): Int = {
    val list: Module1.List =
      Module1.Cons(1, Module1.Cons(4, Module1.Nil()));
    val length: Int =
      Module1.length(list);
    val sum: Int =
      Module1.sum(list);
    val first: Int =
      list match {
        case Module1.Nil() =>
          0
        case Module1.Cons(h, _) =>
          h
      };
    ((first + length) + sum)
  }

  useModule1()
}


object Recursive {

  abstract class Tree

  case class Empty() extends Tree

  case class Node(v: Int, v: Tree, v: Tree) extends Tree

  def insert(tree: Tree, value: Int): Tree = {
    tree match {
      case Empty() =>
        Node(value, Empty(), Empty())
      case Recursive.Node(v, l, r) =>
        (if((value < v)) {
          Node(v, insert(l, value), r)
        } else {
          Recursive.Node(v, l, Recursive.insert(r, value))
        })
    }
  }

  val emptyTree: Tree =
    Empty();
  val tree: Tree =
    insert(insert(insert(emptyTree, 5), 3), 7);
  insert(tree, 9)
}


object EdgeCases {

  def emptyBody(): Unit = {
    ()
  }

  def noParams(): Int = {
    42
  }

  abstract class EmptyClass

  case class EmptyConstructor() extends EmptyClass

  def nestedIf(a: Int): Int = {
    (if((a < 0)) {
      (if((a < -(10))) {
        -(100)
      } else {
        -(10)
      })
    } else {
      (if((10 < a)) {
        100
      } else {
        10
      })
    })
  }

  def nestedMatch(a: Int): Int = {
    a match {
      case 0 =>
        0
      case 1 =>
        1
      case n =>
        n match {
          case 2 =>
            2
          case 3 =>
            3
          case m =>
            (m * 10)
        }
    }
  }

  def complexExpr(a: Int, b: Int): Int = {
    (((a + (b * 2)) - ((3 / 4) % 5)) + ((-(a) * (b + 1)) / 2))
  }

  def longSequence(): Int = {
    val a: Int =
      1;
    val b: Int =
      2;
    val c: Int =
      3;
    val d: Int =
      4;
    val e: Int =
      5;
    a;
    b;
    c;
    d;
    e
  }

  def parenthesized(a: Int, b: Int): Int = {
    (((a + b) * (a - b)) / ((a * b) % (a / b)))
  }

  def manyParens(a: Int): Int = {
    (((((a + 1) * 2) - 3) / 4) % 5)
  }

  def booleanExpr(a: Boolean, b: Boolean): Boolean = {
    (((!(a) && b) || (a && !(b))) || (!((a || b)) && (a || !(b))))
  }

  abstract class Pair

  case class P(v: Int, v: Int) extends Pair

  def matchMultiple(p: Pair): Int = {
    p match {
      case P(x, y) =>
        (x + y)
    }
  }

  def matchUnderscores(p: Pair): Int = {
    p match {
      case P(x, _) =>
        x
    }
  }

  def matchLiterals(a: Int): Boolean = {
    a match {
      case 0 =>
        true
      case 1 =>
        false
      case _ =>
        true
    }
  }

  def operatorEdge(): Int = {
    (((1 + (2 * 3)) + (4 * 5)) + 6)
  }

  def countdown(n: Int): Int = {
    (if((n <= 0)) {
      0
    } else {
      countdown((n - 1))
    })
  }

  def allOperators(a: Int, b: Int, c: Boolean, d: Boolean, s: String): Boolean = {
    (((((a + b) - (((a * b) / a) % b)) < a) && (a <= b)) || (((((c && d) || !(c)) || !(d)) && (a == b)) && ((s ++ "test") == s)))
  }

  def testSemicolons(): Int = {
    val a: Int =
      1;
    val b: Int =
      2;
    (b + a)
  }

  testSemicolons()
}


object OperatorPrecedence {

  def testLevel1(): Int = {
    val x: Int =
      1;
    val y: Int =
      2;
    x;
    y
  }

  def testLevel2(a: Int): Int = {
    (if((0 < a)) {
      1
    } else {
      0
    }) match {
      case 1 =>
        10
      case 0 =>
        0
      case _ =>
        -(1)
    }
  }

  def testLevel3(a: Boolean, b: Boolean, c: Boolean): Boolean = {
    ((a || b) || c)
  }

  def testLevel4(a: Boolean, b: Boolean, c: Boolean): Boolean = {
    ((a && b) && c)
  }

  def testLevel5(a: Int, b: Int, c: Int): Boolean = {
    (((a == b) == c) == a)
  }

  def testLevel6(a: Int, b: Int, c: Int): Boolean = {
    ((a < b) <= c)
  }

  def testLevel7(a: Int, b: Int, c: Int, s1: String, s2: String): Int = {
    ((a + b) - c)
  }

  def testLevel7String(s1: String, s2: String, s3: String): String = {
    ((s1 ++ s2) ++ s3)
  }

  def testLevel8(a: Int, b: Int, c: Int): Int = {
    (((a * b) / c) % a)
  }

  def testLevel9(a: Int, b: Boolean): Int = {
    -(a)
  }

  def testLevel9Bool(b: Boolean): Boolean = {
    !(b)
  }

  def testLevel10(): Int = {
    error("test")
  }

  def test3vs4(a: Boolean, b: Boolean, c: Boolean): Boolean = {
    (a || (b && c))
  }

  def test4vs5(a: Boolean, b: Int, c: Int): Boolean = {
    (a && (b == c))
  }

  def test5vs6(a: Int, b: Int, c: Int): Boolean = {
    (a == (b < c))
  }

  def test6vs7(a: Int, b: Int, c: Int): Boolean = {
    (a < (b + c))
  }

  def test7vs8(a: Int, b: Int, c: Int): Int = {
    (a + (b * c))
  }

  def test8vs9(a: Int, b: Int): Int = {
    (a * -(b))
  }

  def test8vs9Bool(a: Int, b: Boolean): Int = {
    (a * (if(!(b)) {
      1
    } else {
      0
    }))
  }

  def test9vs10(a: Int): Int = {
    -((a + 1))
  }

  def complexMixed1(a: Int, b: Int, c: Boolean, d: Boolean): Boolean = {
    ((((a + (b * a)) < (b / a)) && c) || !(d))
  }

  def complexMixed2(a: Int, b: Int, c: Boolean, d: Boolean): Boolean = {
    ((((a + b) == (a * b)) && c) || ((a < b) && d))
  }

  complexMixed2(1, 2, true, false)
}


object CommentsAndAll {

  abstract class Data

  case class Value(v: Int) extends Data

  def testFunction(a: Int, b: Int): Int = {
    val x: Int =
      (a + b);
    (if((0 < x)) {
      x
    } else {
      -(x)
    })
  }

  def allFeatures(input: Int): Int = {
    val isPositive: Boolean =
      (0 < input);
    val absValue: Int =
      (if(isPositive) {
        input
      } else {
        -(input)
      });
    val data: Data =
      Value(absValue);
    val result: Int =
      data match {
        case Value(x) =>
          (x * 2)
      };
    val computed: Int =
      ((result + 10) - (((5 * 2) / 2) % 3));
    val check: Boolean =
      ((0 < computed) && ((computed < 100) || (computed == 50)));
    val message: String =
      ("Result: " ++ Std.intToString(computed));
    Std.printString(message);
    (if(((check && isPositive) || (100 < computed))) {
      allFeatures((computed / 2))
    } else {
      (if((computed < 0)) {
        error(("Negative result: " ++ Std.intToString(computed)))
      } else {
        computed
      })
    })
  }

  allFeatures(42)
}