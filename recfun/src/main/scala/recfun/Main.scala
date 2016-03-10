package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
  	if (c==0 || c==1) 1 
  	else pascal(c-1,r-1)+pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def f(chars:List[Char], counts:Int):Int = {
      if(chars.isEmpty || counts < 0) counts
      else if (chars.head == '(') f(chars.tail, counts+1) 
      else if (chars.head == ')') f(chars.tail, counts-1)
      else f(chars.tail, counts)  
    }
    f(chars,0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else if(money <= 0 && !coins.isEmpty) 0
    else countChange(money,coins.tail) +  countChange(money-coins.head, coins)
  }
}
