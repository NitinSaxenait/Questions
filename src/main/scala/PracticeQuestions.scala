import scala.util.control.Breaks.break


class Question {
  def makeEachFirstCharCapital(string: String): String = {
    string.split(' ').map(_.capitalize).mkString(" ")
  }

  def reverseString(str: String): String = {
    (for (count <- str.length - 1 to 0 by -1) yield str(count)).mkString("")
  }

  def reverseEachWord(str: String): String = {
    val get = str.split(' ').map { word =>
      (for (count <- word.length - 1 to 0 by -1) yield word(count)).mkString(" ")
    }
    get.mkString("  ")
  }

  def countDuplicateChar(str1: String): Unit = {
    val storeCharacter: Array[Char] = new Array[Char](str1.length)
    var counter = 0
    val str =  str1.toLowerCase()
    for (count <- 0 until str.length) {
      if (!storeCharacter.contains(str.charAt(count))) {
        for (innerCount <- count + 1 until (str.length)) {
          if (str.charAt(count) == str.charAt(innerCount)) {
            storeCharacter(count) = str.charAt(count)
            counter += 1
          }
        }
        println(s"The character - ${str.charAt(count)} has count ${counter + 1}")
        counter = 0
      } else
        println(s"This character - ${str.charAt(count)} is already counted")
    }
  }

  def capitalFirstCharAndReverseWord(str: String): String = {
    val getWords = str.toLowerCase().split(' ').map { word =>
      word.capitalize
    }
    getWords.map(response =>
      (for (count <- response.length - 1 to 0 by -1) yield response(count)).mkString(" ")
    ).mkString(" ")
  }


  def checkStringPalindrome(str: String): Boolean = {
    /*]
    ABCBA
    * */
    val getString = (for (count <- str.length - 1 to 0 by -1) yield str(count)).mkString("")

    val getResult = str == getString
    println(s"The reverse of $str is ${getString}")
    getResult
  }

  def checkIntPalindrome(value: Int): Boolean = {
    val intToString = value.toString
    println(s"Integer to string we get $intToString")
    val getString = (for (count <- intToString.length - 1 to 0 by -1) yield intToString(count)).mkString("")
    val isTrue = intToString == getString
    isTrue
  }

  /*
  * Fibonacci Series:
    first = 0, second = 1 third = first+second
   */
  def fibonacci(number: Int): (String, Int) = {
    val storeNumber = new Array[Int](number + 1)
    storeNumber(0) = 0
    storeNumber(1) = 1
    for (count <- 2 to number) {
      storeNumber(count) = storeNumber(count - 1) + storeNumber(count - 2)
    }
    (storeNumber.mkString(" "), storeNumber.last)

  }

  def sortAny[T <% Ordered[T]](list: Array[T])= {
    var count = 0
    while (count < list.length) {
      /*list.indices.foreach { index =>*/
      for (count <- 0 until list.length -1) {
        if (list(count) > list(count + 1)) {
          val temp = list(count)
          list(count) = list(count + 1)
          list(count + 1) = temp
        }
      }
      count = count + 1
    }

    list
  }
  def uniqueCharater() {
    val value = "abacbbad"
    for (count <- 0 until value.length) {
      if(value.indexOf(value(count)) == value.lastIndexOf(value(count)))
        println(s"Value ${value(count)}")
    }
  }
}

object PracticeQuestions extends App {
  val str = "SAMAR is awesome and know about Scala"
  val strArray = Array("geeksforgeeks", "geeks", "geek",
    "geezer")
  val palin = "LEVEL"
  val strArr = Array("Hello", "Hell", "Heli")

  val questionClass = new Question
   val getResult = questionClass.makeEachFirstCharCapital(str)
/*   val getResult2 = questionClass.usingLoop(str)*/
   val reverse = questionClass.reverseString(str)
   val reverseWord = questionClass.reverseEachWord(str)
   val duplicateChar = questionClass.countDuplicateChar(str)
  val revePract = questionClass.capitalFirstCharAndReverseWord(str)
/*  val commonChar = questionClass.commonLongestPrefix(strArr)*/
  val checkPalindrome = questionClass.checkIntPalindrome(3553)
    val findPalindrome = questionClass.fibonacci(9)
  val array = Array(10, 9, 8, 7, 6, 5, 5, 3, 2, 1)
  val doubleArray = Array(10.9, 8.2)
  val arrayChar = Array('d','c', 'b', 'a')
  val sortingInteger = questionClass.sortAny(strArr)
  println(findPalindrome)
println(reverseWord)
 /* println(sortingInteger.mkString("Array(", ", ", ")"))*/

}

object Sorting extends App {

  def sortingString(str: String): String = {

    val strArray = str.toCharArray
    var outerCount = 0
    while (outerCount < strArray.length) {
      for (count <- 0 until strArray.length - 1) {

        if (strArray(count) > strArray(count + 1)) {

          val temp = strArray(count)
          strArray(count) = strArray(count + 1)
          strArray(count + 1) = temp

        }

      }
      outerCount = outerCount + 1
    }
    strArray.mkString
  }

  println(sortingString("dcba"))


}
