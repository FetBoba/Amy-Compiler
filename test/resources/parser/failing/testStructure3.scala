object MissingParens {
  abstract class List
  case class Cons h: Int, t: List extends List
}