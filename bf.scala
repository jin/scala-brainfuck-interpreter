import scala.io.Source;
import scala.io.BufferedSource;
import scala.collection.mutable.Stack;
import scala.io.StdIn;

object BF extends App {

  def parseBFSource(filePath: String) = {
    val instructions = loadInstructions(Source.fromFile(filePath))
    parseInstructions(instructions)
  }

  def initializeCells(): Array[Int] = new Array[Int](30000)

  def loadInstructions(source: BufferedSource): List[Char] = {
    source.foldLeft(List[Char]()) {
      (acc, char) => char match {
        case '+' | '-' | '.' | ',' | '[' | ']' | '<' | '>' => acc :+ char
        case _ => acc 
      }
    }
  }

  def parseInstructions(instructions: List[Char]) = {

    val cells = initializeCells
    val lbIndexes = Stack[Int]() // left bracket indexes

    val instructionCount = instructions.size

    var instructionIndex = 0
    var cellIndex = 0

    def printDebug(instruction: Char)= {
      println(cells.toList.take(4), instruction, instructionIndex, cellIndex)
      println(instructions.foldLeft("") (_ + _))
      val pointerArray = new Array[String](instructionIndex + 1)
      pointerArray(instructionIndex) = "^"
      println(pointerArray.foldLeft("") {
        (acc, v) => if (v == null) {
          acc + " "
        } else {
          acc + v
        }
      })
    }

    while (instructionIndex < instructionCount) {
      val i = instructions(instructionIndex)
      printDebug(i)
      if (isPlus(i)) {

        incCellValue(cells, cellIndex)

      } else if (isMinus(i)) {

        decCellValue(cells, cellIndex)

      } else if (isNext(i)) {

        cellIndex += 1

      } else if (isPrev(i)) {

        cellIndex -= 1

      } else if (isRead(i)) {

        cells(cellIndex) = StdIn.readChar.toInt

      } else if (isPrint(i)) {

        print(cells(cellIndex).toChar)

      } else if (isLB(i)) {

        if (cells(cellIndex) == 0) {
          if (!lbIndexes.isEmpty && lbIndexes.top == instructionIndex) {
            lbIndexes.pop
          }
          while (instructions(instructionIndex) != ']') {
            instructionIndex += 1
          }
        } else {
          if (lbIndexes.isEmpty || lbIndexes.top != instructionIndex) {
            lbIndexes.push(instructionIndex)
          }
        }

      } else if (isRB(i)) {

        if (cells(cellIndex) == 0) {
          lbIndexes.pop
        } else {
          instructionIndex = lbIndexes.top - 1
        }

      } else throw new Exception("SyntaxError")

      instructionIndex += 1
    }
  }

  def incCellValue(cells: Array[Int], idx: Int) = cells(idx) += 1
  def decCellValue(cells: Array[Int], idx: Int) = cells(idx) -= 1

  def isPlus(c: Char) = c == '+'
  def isLB(c: Char) = c == '['
  def isRB(c: Char) = c == ']'
  def isMinus(c: Char) = c == '-'
  def isNext(c: Char) = c == '>'
  def isPrev(c: Char) = c == '<'
  def isPrint(c: Char) = c == '.'
  def isRead(c: Char) = c == ','

  override def main(args: Array[String]) {
    if (args.isEmpty) { throw new Exception("Please enter a filename") }
    parseBFSource(args(0))
  }
}
