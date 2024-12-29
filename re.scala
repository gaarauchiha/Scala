// Main Part 3 about Regular Expression Matching
//==============================================

object M3 {

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALTs(rs: List[Rexp]) extends Rexp  // alternatives 
case class SEQs(rs: List[Rexp]) extends Rexp  // sequences
case class STAR(r: Rexp) extends Rexp         // star


//the usual binary choice and binary sequence can be defined 
//in terms of ALTs and SEQs
def ALT(r1: Rexp, r2: Rexp) = ALTs(List(r1, r2))
def SEQ(r1: Rexp, r2: Rexp) = SEQs(List(r1, r2))

// some convenience for typing regular expressions

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

import scala.language.implicitConversions

given Conversion[String, Rexp] = (s => charlist2rexp(s.toList))

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

// some examples for the conversion and extension:

// val areg : Rexp = "a" | "b"
//  => ALTs(List(CHAR('a'), CHAR('b')))
//
// val sreg : Rexp = "a" ~ "b"
//  => SEQs(List(CHAR('a'), CHAR('b'))) 
//
// val star_reg : Rexp = ("a" ~ "b").%
//  => STAR(SEQs(List(CHAR('a'), CHAR('b')))) 

// ADD YOUR CODE BELOW
//======================

// (1)
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALTs(rs) => rs.exists(nullable)
  case SEQs(rs) => rs.forall(nullable)
  case STAR(_) => true
}

// (2) 
def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALTs(rs) => ALTs(rs.map(der(c, _)))
  case SEQs(Nil) => ZERO
  case SEQs(r1 :: rs) => 
    if (nullable(r1)) 
      ALT(SEQs(der(c, r1) :: rs), der(c, SEQs(rs)))
    else 
      SEQs(der(c, r1) :: rs)
  case STAR(r1) => SEQs(List(der(c, r1), STAR(r1)))
}

// (3) 
def denest(rs: List[Rexp]) : List[Rexp] = rs match {
  case Nil => Nil
  case ZERO :: rest => denest(rest)
  case ALTs(subRs) :: rest => subRs ::: denest(rest)
  case r :: rest => r :: denest(rest)
}

// (4)
def flts(rs: List[Rexp], acc: List[Rexp] = Nil) : List[Rexp] = rs match {
  case Nil => acc
  case ZERO :: _ => List(ZERO)
  case ONE :: rest => flts(rest, acc)
  case SEQs(subRs) :: rest => flts(rest, acc ::: subRs)
  case r :: rest => flts(rest, acc ::: List(r))
}

// (5)
def ALTs_smart(rs: List[Rexp]) : Rexp = rs match {
  case Nil => ZERO
  case r :: Nil => r
  case _ => ALTs(rs)
}

def SEQs_smart(rs: List[Rexp]) : Rexp = rs match {
  case Nil => ONE
  case List(ZERO) => ZERO
  case r :: Nil => r
  case _ => SEQs(rs)
}

// (6)
def simp(r: Rexp) : Rexp = r match {
  case ALTs(rs) => ALTs_smart(denest(rs.map(simp)).distinct)
  case SEQs(rs) => SEQs_smart(flts(rs.map(simp)))
  case _ => r // for ZERO, ONE, and CHAR
}

// (7)
def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c :: cs => ders(cs, simp(der(c, r)))
}
def matcher(r: Rexp, s: String): Boolean = nullable(ders(s.toList, r))

// (8) 
def size(r: Rexp): Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_) => 1
  case ALTs(rs) => 1 + rs.map(size).sum
  case SEQs(rs) => 1 + rs.map(size).sum
  case STAR(r1) => 1 + size(r1)
}


// Some testing data
//===================
/*

simp(ALT(ONE | CHAR('a'), CHAR('a') | ONE))   // => ALTs(List(ONE, CHAR(a)))
simp(((CHAR('a') | ZERO) ~ ONE) | 
     (((ONE | CHAR('b')) | CHAR('c')) ~ (CHAR('d') ~ ZERO)))   // => CHAR(a)

matcher(("a" ~ "b") ~ "c", "ab")   // => false
matcher(("a" ~ "b") ~ "c", "abc")  // => true


// the supposedly 'evil' regular expression (a*)* b
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

matcher(EVIL, "a" * 1000)          // => false
matcher(EVIL, "a" * 1000 ++ "b")   // => true


// size without simplifications
size(der('a', der('a', EVIL)))             // => 36
size(der('a', der('a', der('a', EVIL))))   // => 83

// size with simplification
size(simp(der('a', der('a', EVIL))))           // => 7
size(simp(der('a', der('a', der('a', EVIL))))) // => 7

// Python needs around 30 seconds for matching 28 a's with EVIL. 
// Java 9 and later increase this to an "astonishing" 40000 a's in
// 30 seconds.
//
// Lets see how long it really takes to match strings with 
// 5 Million a's...it should be in the range of a few
// of seconds.

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  "%.5f".format((end - start)/(i * 1.0e9))
}

for (i <- 0 to 5000000 by 500000) {
  println(s"$i ${time_needed(2, matcher(EVIL, "a" * i))} secs.") 
}

// another "power" test case 
simp(Iterator.iterate(ONE:Rexp)(r => SEQ(r, ONE | ONE)).drop(50).next()) == ONE

// the Iterator produces the rexp
//
//      SEQ(SEQ(SEQ(..., ONE | ONE) , ONE | ONE), ONE | ONE)
//
//    where SEQ is nested 50 times.

*/

}



object TestM3 extends App {
  
  import M3._
  
  // Test cases for simp
  println(simp(ALT(ONE | CHAR('a'), CHAR('a') | ONE))) // Should print ALTs(List(ONE, CHAR(a)))
  println(simp(((CHAR('a') | ZERO) ~ ONE) | 
               (((ONE | CHAR('b')) | CHAR('c')) ~ (CHAR('d') ~ ZERO)))) // Should print CHAR(a)

  // Test cases for matcher
  println(matcher(("a" ~ "b") ~ "c", "ab")) // Should print false
  println(matcher(("a" ~ "b") ~ "c", "abc")) // Should print true

  // Test cases for 'evil' regular expression
  val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))
  
  println(matcher(EVIL, "a" * 1000)) // Should print false
  println(matcher(EVIL, "a" * 1000 ++ "b")) // Should print true

  // Test size without simplifications
  println(size(der('a', der('a', EVIL)))) // Should print 36
  println(size(der('a', der('a', der('a', EVIL))))) // Should print 83

  // Test size with simplification
  println(size(simp(der('a', der('a', EVIL))))) // Should print 7
  println(size(simp(der('a', der('a', der('a', EVIL)))))) // Should print 7

  // Timing test for EVIL regex
  def time_needed[T](i: Int, code: => T) = {
    val start = System.nanoTime()
    for (j <- 1 to i) code
    val end = System.nanoTime()
    "%.5f".format((end - start)/(i * 1.0e9))
  }

  for (i <- 0 to 5000000 by 500000) {
    println(s"$i: ${time_needed(2, matcher(EVIL, "a" * i))} secs.")
  }

  // Power test case for simplification
  val powerTest = Iterator.iterate(ONE: Rexp)(r => SEQ(r, ONE | ONE)).drop(50).next()
  println(simp(powerTest) == ONE) // Should print true
}