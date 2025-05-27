object NestedMatchInvalid {
  x match {
    case 1 => y match {
      case 2 case 3 => 4
    }
  }
}