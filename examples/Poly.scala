object Poly {    
    abstract class List[A]
    case class Nil[A]() extends List[A]
    case class Cons[A](h: A, t: List[A]) extends List[A]

    def length[A](l: List[A]): Int = {
        l match {
            case Nil() => 0
            case Cons(_, t) => 1 + length(t)
        }
    }
    case class Cons2[A, B](h1: A, h2: B, t: List[List[Int]]) extends List[A]
        // Wrong, type parameters don’t match
}