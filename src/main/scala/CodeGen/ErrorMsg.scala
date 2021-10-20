package CodeGen

object ErrorMsg {
  val overflow = "OverflowError: the result is too small/large to store in a 4-byte signed-integer."

  val divideByZero = "DivideByZeroError: divide or modulo by zero"

  val nullReference = "NullReferenceError: dereference a null reference"

  val arrayOutOfBoundsLo = "ArrayOutOfBoundsError: negative index out of bounds"

  val arrayOutOfBoundsHi = "ArrayOutOfBoundsError: index too large"
}