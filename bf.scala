import scala.io.Source
import scala.io.BufferedSource
import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap
import scala.io.StdIn

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
    val scopes = buildScopes(instructions)
    var instructionIndex = 0
    var cellIndex = 0

    def printDebug(instruction: Char)= {
      val pointerArray = new Array[String](instructionIndex + 1)
      pointerArray(instructionIndex) = "^"

      println(cells.toList.take(10), instruction, instructionIndex, cellIndex)
      println(instructions.foldLeft("") (_ + _))
      println(pointerArray.foldLeft("") {
        (acc, v) => if (v == null) { acc + " " } else { acc + v } 
      })
    }

    while (instructionIndex < instructions.size) {
      val i = instructions(instructionIndex)

      if (isPlus(i))        incCellValue(cells, cellIndex)
      else if (isMinus(i))  decCellValue(cells, cellIndex)
      else if (isNext(i))   cellIndex += 1 
      else if (isPrev(i))   cellIndex -= 1
      else if (isRead(i))   cells(cellIndex) = StdIn.readChar.toInt
      else if (isPrint(i))  print(cells(cellIndex).toChar) 
      else if (isLB(i) && isZero(cells, cellIndex))   instructionIndex = scopes.get(instructionIndex).get
      else if (isRB(i) && !isZero(cells, cellIndex))  instructionIndex = scopes.get(instructionIndex).get 

      instructionIndex += 1
    }
  }

  def buildScopes(instructions: List[Char]): HashMap[Int, Int] = {
    val scopes = HashMap[Int, Int]()
    val leftIdxes = Stack[Int]()
    // use stack for LB and queue for RB indexes
    for (n <- 0 to (instructions.size - 1)) yield {
      if (isLB(instructions(n))) {
        leftIdxes.push(n)
      } else if (isRB(instructions(n))) {
        assert(!leftIdxes.isEmpty)

        val leftIdx = leftIdxes.pop
        scopes.update(leftIdx, n)
        scopes.update(n, leftIdx)
      }
    }

    assert(leftIdxes.isEmpty)
    scopes
  }

  def incCellValue(cells: Array[Int], idx: Int) = cells(idx) += 1
  def decCellValue(cells: Array[Int], idx: Int) = cells(idx) -= 1
  def isZero(cells: Array[Int], idx: Int) = cells(idx) == 0

  def isPlus(c: Char) = c == '+'
  def isMinus(c: Char) = c == '-'
  def isLB(c: Char) = c == '['
  def isRB(c: Char) = c == ']'
  def isNext(c: Char) = c == '>'
  def isPrev(c: Char) = c == '<'
  def isPrint(c: Char) = c == '.'
  def isRead(c: Char) = c == ','

  override def main(args: Array[String]) {
    if (args.isEmpty) { throw new Exception("Please enter a filename") }
    parseBFSource(args(0))
  }
}
