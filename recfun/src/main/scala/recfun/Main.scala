package recfun

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
    if (c == r || c == 0) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
    * Exercise 2
    *
    * chars.isEmpty: Boolean returns whether a list is empty
    * chars.head: Char returns the first element of the list
    * chars.tail: List[Char] returns the list without the first element
    */
  def balance(chars: List[Char]): Boolean = {

    def balanceCounter(leftParenthesisAcc: Int, rightParenthesisAcc: Int, subChars: List[Char]): Int = {
      if (subChars.isEmpty || rightParenthesisAcc > leftParenthesisAcc) {
        leftParenthesisAcc - rightParenthesisAcc
      } else {
        val headChar = subChars.head

        if (headChar.equals('(')) {
          balanceCounter(leftParenthesisAcc + 1, rightParenthesisAcc, subChars.tail)
        } else if (headChar.equals(')')) {
          balanceCounter(leftParenthesisAcc, rightParenthesisAcc + 1, subChars.tail)
        } else {
          balanceCounter(leftParenthesisAcc, rightParenthesisAcc, subChars.tail)
        }
      }
    }

    if (balanceCounter(0, 0, chars) == 0) {
     return true
    }

    false

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      return 1
    if (money < 0 || (coins.isEmpty && money >= 1))
      return 0
    countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
