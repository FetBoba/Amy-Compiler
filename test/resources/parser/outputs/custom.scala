object Factorial {

  (if(1) {
    1
  } else {
    2
  }) match {
    case Leaf() =>
      error("Cannot get value from a Leaf")
    case Node(v, _, _) =>
      v
  } match {
    case Leaf() =>
      error("Cannot get value from a Leaf")
    case Node(v, _, _) =>
      v
  };
  2
}


object Factorial {

  def fact(i: Int): Int = {
    (if((i < 2)) {
      1
    } else {
      (
        val rec: Int =
          fact((i - 1));
        (i * rec)
      )
    })
  }

  val x: Int =
    (if((1 == 2)) {
      1
    } else {
      2
    });
  val y: Int =
    t match {
      case Leaf() =>
        error("Cannot get value from a Leaf")
      case Node(v, _, _) =>
        v
    } match {
      case Leaf() =>
        error("Cannot get value from a Leaf")
      case Node(v, _, _) =>
        v
    };
  t match {
    case Leaf() =>
      error("Cannot get value from a Leaf")
    case Node(v, _, _) =>
      v
  } match {
    case Leaf() =>
      error("Cannot get value from a Leaf")
    case Node(v, _, _) =>
      v
  };
  (if(t match {
    case Leaf() =>
      error("Cannot get value from a Leaf")
    case Node(v, _, _) =>
      v
  }) {
    1
  } else {
    2
  });
  1
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
