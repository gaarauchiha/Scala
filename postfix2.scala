// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object C3b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// ADD YOUR CODE BELOW
//======================


  def is_op(op: String): Boolean = ops.contains(op)
  def prec(op1: String, op2: String): Boolean = assoc(op1) match {
    case LA => precs.getOrElse(op1, 0) >= precs.getOrElse(op2, 0) // Left-associative
    case RA => precs.getOrElse(op1, 0) > precs.getOrElse(op2, 0)  // Right-associative
  }




// (3) 
def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = toks match {
    case Nil =>
      // If the input is empty, move all operators from the stack to the output
      (st.reverse ++ out).reverse

    case tok :: rest if tok.forall(_.isDigit) =>
      // If the token is a number, add it to the output list
      syard(rest, st, tok :: out)

    case tok :: rest if is_op(tok) =>
      // If the token is an operator, pop operators based on precedence and associativity
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
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4)
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
            case "^" => BigInt(a).pow(b).toInt // Use BigInt.pow for power operation
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
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536


  def main(args: Array[String]): Unit = {
    println(syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))) // Expected: List(3, 4, 8, *, 5, 1, -, 2, 3, ^, ^, /, +)
    println(compute(syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3")))) // Expected: 3

    println(compute(syard(split("4 ^ 3 ^ 2"))))      // Expected: 262144
    println(compute(syard(split("4 ^ ( 3 ^ 2 )"))))  // Expected: 262144
    println(compute(syard(split("( 4 ^ 3 ) ^ 2"))))  // Expected: 4096
    println(compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))) // Expected: 65536
  }



}
