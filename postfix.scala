// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object C3a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// ADD YOUR CODE BELOW
//======================


// (1) 
def is_op(op: String) : Boolean =  ops.contains(op)
def prec(op1: String, op2: String) : Boolean = precs.getOrElse(op1, 0) >= precs.getOrElse(op2, 0)

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
  case Nil =>
    // If the input is empty, move all operators from the stack to the output
    (st.reverse ++ out).reverse

  case tok :: rest if tok.forall(_.isDigit) =>
    // If the token is a number, add it to the output list
    syard(rest, st, tok :: out)

  case tok :: rest if is_op(tok) =>
    // If the token is an operator, pop operators with higher or equal precedence
    st match {
      case op :: ops if is_op(op) && prec(op, tok) =>
        syard(toks, ops, op :: out) // Pop operator from stack to output
      case _ =>
        syard(rest, tok :: st, out) // Push the current operator onto the stack
    }

  case "(" :: rest =>
    // If the token is a left parenthesis, push it onto the stack
    syard(rest, "(" :: st, out)

  case ")" :: rest =>
    // If the token is a right parenthesis, pop until a left parenthesis is found
    st.span(_ != "(") match {
      case (toOutput, _ :: ops) =>
        syard(rest, ops, toOutput.reverse ++ out) // Discard the left parenthesis
      case _ =>
        throw new IllegalArgumentException("Mismatched parentheses")
    }

  case invalidToken :: _ =>
    throw new IllegalArgumentException(s"Invalid token: $invalidToken")
}


// test cases
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (2) 
def compute(toks: Toks, st: List[Int] = Nil) : Int = toks match {
  case Nil =>
    // If input is empty, the result is the top of the stack
    st match {
      case result :: Nil => result
      case _ => throw new IllegalArgumentException("Invalid postfix expression")
    }

  case tok :: rest if tok.forall(_.isDigit) =>
    // If the token is a number, push it onto the stack
    compute(rest, tok.toInt :: st)

  case tok :: rest if is_op(tok) =>
    // If the token is an operator, pop two numbers from the stack and apply the operation
    st match {
      case b :: a :: stackRest =>
        val result = tok match {
          case "+" => a + b
          case "-" => a - b
          case "*" => a * b
          case "/" => a / b
          case _   => throw new IllegalArgumentException(s"Unknown operator: $tok")
        }
        compute(rest, result :: stackRest)
      case _ =>
        throw new IllegalArgumentException("Invalid postfix expression")
    }

  case tok :: _ =>
    // Handle invalid tokens
    throw new IllegalArgumentException(s"Invalid token: $tok")
}

// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}

object Main {
  def main(args: Array[String]): Unit = {
    import C3a._

    // Example test cases
    println(syard(split("3 + 4 * ( 2 - 1 )")))  // Expected: List(3, 4, 2, 1, -, *, +)
    println(compute(syard(split("3 + 4 * ( 2 - 1 )"))))  // Expected: 7

    println(syard(split("10 + 12 * 33"))) // Expected: List(10, 12, 33, *, +)
    println(compute(syard(split("10 + 12 * 33")))) // Expected: 406

    println(syard(split("( 5 + 7 ) * 2"))) // Expected: List(5, 7, +, 2, *)
    println(compute(syard(split("( 5 + 7 ) * 2")))) // Expected: 24

    println(syard(split("5 + 7 / 2"))) // Expected: List(5, 7, 2, /, +)
    println(compute(syard(split("5 + 7 / 2")))) // Expected: 8

  }
}
