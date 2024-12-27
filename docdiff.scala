// Core Part 2 about Code Similarity
//===================================


object C2 { 

// ADD YOUR CODE BELOW
//======================

//(1)
def clean(s: String) : List[String] = {
    val reg = "\\w+".r
    reg.findAllIn(s).toList.map(_.toLowerCase) // Extract words and convert to lowercase
  }
  


//(2)
def occurrences(xs: List[String]): Map[String, Int] = {
    xs.distinct.map(word => word -> xs.count(_ == word)).toMap
  }


//(3)
def prod(lst1: List[String], lst2: List[String]) : Int =  {
    val occ1 = occurrences(lst1)
    val occ2 = occurrences(lst2)
    (occ1.keys.toList ++ occ2.keys.toList).distinct.map(key => occ1.getOrElse(key, 0) * occ2.getOrElse(key, 0)).sum
  }

//(4)
def overlap(lst1: List[String], lst2: List[String]) : Double = {
    val d1DotD2 = prod(lst1, lst2).toDouble
    val d1DotD1 = prod(lst1, lst1).toDouble
    val d2DotD2 = prod(lst2, lst2).toDouble
    d1DotD2 / Math.max(d1DotD1, d2DotD2)
  }

def similarity(s1: String, s2: String) : Double = {
    val doc1 = clean(s1)
    val doc2 = clean(s2)
    overlap(doc1, doc2)
  }

}
