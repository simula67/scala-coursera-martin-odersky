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
  def pascal(c: Int, r: Int): Int =
    {
      if ((c == 0) && (r == 0))
        1
      else if ((c < 0) || (r < 0))
        0
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    {
      def real_balance(status: Int, remaining: List[Char]): Boolean =
        {
          if (remaining.isEmpty) {
            if (status != 0)
              false;
            else
              true;
          } else {
            if (status < 0)
              false;
            else {
              if (remaining.head == '(')
                real_balance(status + 1, remaining.tail);
              else {
                if (remaining.head == ')')
                  real_balance(status - 1, remaining.tail);
                else
                  real_balance(status, remaining.tail);
              }

            }
          }
        }
      real_balance(0, chars)
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    {
      var possibilities: Int = 0;
      def addPossibility(money: Int, coins: List[Int]): Unit =
        {
          if ((money != 0) && (!coins.isEmpty)) {
            for (j <- 0 to money / coins.head) {
              if (((j + 1) * coins.head) == money)
                possibilities += 1;
              else
                addPossibility((money - ((j + 1) * coins.head)), coins.tail);
            }
            addPossibility(money, coins.tail);
          }
        }
      addPossibility(money, coins)
      possibilities;
    }

}
