// Main Part 2 about Evil Wordle
//===============================


object M2 { 

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================


//(1)
def get_wordle_list(url: String) : List[String] = {
  Try(Source.fromURL(url).getLines().toList).getOrElse(List.empty)
}

// val secrets = get_wordle_list("https://nms.kcl.ac.uk/christian.urban/wordle.txt")
// secrets.length // => 12972
// secrets.filter(_.length != 5) // => Nil

//(2)
def removeN[A](xs: List[A], elem: A, n: Int) : List[A] = {
  xs.foldLeft((List.empty[A], n)) { case ((acc, remaining), x) =>
    if (x == elem && remaining > 0) (acc, remaining - 1)
    else (x :: acc, remaining)
  }._1.reverse
}


// removeN(List(1,2,3,2,1), 3, 1)  // => List(1, 2, 2, 1)
// removeN(List(1,2,3,2,1), 2, 1)  // => List(1, 3, 2, 1)
// removeN(List(1,2,3,2,1), 1, 1)  // => List(2, 3, 2, 1)
// removeN(List(1,2,3,2,1), 0, 2)  // => List(1, 2, 3, 2, 1)

// (3)
abstract class Tip
case object Absent extends Tip
case object Present extends Tip
case object Correct extends Tip


def pool(secret: String, word: String) : List[Char] =  {
  secret.toList.filterNot(c => word.zip(secret).exists { case (w, s) => w == s && w == c })
}

def aux(secret: List[Char], word: List[Char], pool: List[Char]) : List[Tip] = {
  (secret, word) match {
    case (s :: ss, w :: ws) if s == w => Correct :: aux(ss, ws, pool)
    case (s :: ss, w :: ws) if pool.contains(w) => Present :: aux(ss, ws, pool.filterNot(_ == w))
    case (_ :: ss, _ :: ws) => Absent :: aux(ss, ws, pool)
    case _ => List()
  }
}

def score(secret: String, word: String) : List[Tip] = {
  aux(secret.toList, word.toList, pool(secret, word))
}


// score("chess", "caves") // => List(Correct, Absent, Absent, Present, Correct)
// score("doses", "slide") // => List(Present, Absent, Absent, Present, Present)
// score("chess", "swiss") // => List(Absent, Absent, Absent, Correct, Correct)
// score("chess", "eexss") // => List(Present, Absent, Absent, Correct, Correct)

// (4)
def eval(t: Tip) : Int = t match {
  case Correct => 10
  case Present => 1
  case Absent  => 0
}

def iscore(secret: String, word: String) : Int = {
  score(secret, word).map(eval).sum
}

//iscore("chess", "caves") // => 21
//iscore("chess", "swiss") // => 20

// (5)
def lowest(secrets: List[String], word: String, current: Int, acc: List[String]) : List[String] = {
  secrets match {
    case Nil => acc
    case secret :: rest =>
      val score = iscore(secret, word)
      if (score < current) lowest(rest, word, score, List(secret))
      else if (score == current) lowest(rest, word, current, secret :: acc)
      else lowest(rest, word, current, acc)
  }
}

def evil(secrets: List[String], word: String) : List[String] = {
  lowest(secrets, word, Int.MaxValue, List.empty)
}


//evil(secrets, "stent").length
//evil(secrets, "hexes").length
//evil(secrets, "horse").length
//evil(secrets, "hoise").length
//evil(secrets, "house").length

// (6)
def frequencies(secrets: List[String]) : Map[Char, Double] =  {
  val allChars = secrets.flatMap(_.toList)
  val charCount = allChars.groupBy(identity).mapValues(_.size)
  val total = allChars.size.toDouble
  charCount.map { case (char, count) => char -> (1 - count / total) }.toMap
}

// (7)
def rank(frqs: Map[Char, Double], s: String) : Double = {
  s.flatMap(c => frqs.get(c)).sum
}


def ranked_evil(secrets: List[String], word: String) : List[String] = {
  val evilWords = evil(secrets, word)
  val frqs = frequencies(secrets)
  val ranked = evilWords.map(w => (w, rank(frqs, w)))
  val maxRank = ranked.map(_._2).max
  ranked.filter(_._2 == maxRank).map(_._1)
}




  def main(args: Array[String]): Unit = {
    // Test get_wordle_list
    val url = "https://nms.kcl.ac.uk/christian.urban/wordle.txt"
    val secrets = get_wordle_list(url)
    println(s"Number of words in secrets: ${secrets.length}")
    println(s"Words not of length 5: ${secrets.filter(_.length != 5).length}")

    // Test removeN
    println(removeN(List(1,2,3,2,1), 2, 1)) // Expected: List(1, 3, 2, 1)
    
    // Test score
    println(score("chess", "caves")) // Expected: List(Correct, Absent, Absent, Present, Correct)
    
    // Test iscore
    println(iscore("chess", "caves")) // Expected: 21
    
    // Test evil
    val evilWords = evil(secrets, "stent")
    println(s"Number of evil words for 'stent': ${evilWords.length}")
    
    // Test frequencies
    val freqMap = frequencies(secrets)
    println(freqMap.take(5)) // prints first 5 entries of the map
    
    // Test rank
    println(rank(freqMap, "adobe"))
    
    // Test ranked_evil
    val rankedEvil = ranked_evil(secrets, "abbey")
    println(s"Most evil word for 'abbey': ${rankedEvil}")
  }


// Test removeN
    println(removeN(List(1,2,3,2,1), 3, 1)) // Expected: List(1, 2, 2, 1)
    println(removeN(List(1,2,3,2,1), 2, 1)) // Expected: List(1, 3, 2, 1)
    println(removeN(List(1,2,3,2,1), 1, 1)) // Expected: List(2, 3, 2, 1)
    println(removeN(List(1,2,3,2,1), 0, 2)) // Expected: List(1, 2, 3, 2, 1)
    
    // Test score
    println(score("chess", "caves")) // Expected: List(Correct, Absent, Absent, Present, Correct)
    println(score("doses", "slide")) // Expected: List(Present, Absent, Absent, Present, Present)
    println(score("chess", "swiss")) // Expected: List(Absent, Absent, Absent, Correct, Correct)
    println(score("chess", "eexss")) // Expected: List(Present, Absent, Absent, Correct, Correct)
    
    // Test iscore
    println(iscore("chess", "caves")) // Expected: 21
    println(iscore("chess", "swiss")) // Expected: 20



}
