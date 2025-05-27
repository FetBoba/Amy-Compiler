object ValInArg {
  def foo(x: Int): Int = { x }
  foo(val y: Int = 5; y)
}