// Core Part 1 about the 3n+1 conjecture
//============================================

object C1 {


// ADD YOUR CODE BELOW
//======================


//(1) 
def collatz(n: Long) : Long = {
  require(n > 0, "Input must be a positive number.")
  if (n == 1) 0
  else if (n % 2 == 0) 1 + collatz(n / 2)
  else 1 + collatz(3 * n + 1)
}

//(2) 
def collatz_max(bnd: Long) : (Long, Long) = {
  require(bnd > 0, "Bound must be a positive number.")
  (1L to bnd).map(n => (collatz(n), n)).maxBy(_._1)
}
//(3)
def is_pow_of_two(n: Long) : Boolean = {
  n > 0 && (n & (n - 1)) == 0
}

def is_hard(n: Long) : Boolean = {
  is_pow_of_two(3 * n + 1)
}

def last_odd(n: Long) : Long = {
  require(!is_pow_of_two(n), "Input must not be a power of two.")

  def helper(current: Long): Long = {
    if (is_pow_of_two(current)) current / 2
    else if (current % 2 == 0) helper(current / 2)
    else helper(3 * current + 1)
  }

  helper(n)
}


}
