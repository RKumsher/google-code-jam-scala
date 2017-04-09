package kumsher.ryan.googlecodejam.year2012.qualifying.round.problem.a

import scala.io.Source

object SpeakingInTounges {

  def main(args: Array[String]): Unit = {
    println(createCharacterMap1)
    println(createCharacterMap2)
    println(createCharacterMap3)
  }

  def createCharacterMap1(): Map[Char, Char] = {
    val inputFile = Source.fromFile("sample-input.txt")
    val outputFile = Source.fromFile("sample-output.txt")
    val inputLines = inputFile.getLines().toList
    val outputLines = outputFile.getLines().toList
    var characterMappings = Map('z' -> 'q', 'q' -> 'z')
    for (i <- 1 to (inputLines.length - 1)) {
      val inputLine = inputLines(i)
      val outputLine = outputLines(i - 1).substring((outputLines(i - 1) indexOf ": ") + 2)
      for (j <- 0 to inputLine.length - 1) {
        characterMappings += (inputLine(j) -> outputLine(j))
      }
    }
    characterMappings
  }

  def createCharacterMap2(): Map[Char, Char] = {
    val inputLines = Source.fromFile("sample-input.txt").getLines().toList
    val outputLines = Source.fromFile("sample-output.txt").getLines().toList
    var inputCharacters = List[Char]()
    var outputCharacters = List[Char]()
    inputLines.tail.foreach(line => inputCharacters = inputCharacters ::: line.toList)
    outputLines.foreach(line => outputCharacters = outputCharacters ::: line.substring((line indexOf ": ") + 2).toList)
    var characterMap = Map('z' -> 'q', 'q' -> 'z')
    inputCharacters.zipWithIndex.foreach { case (char, index) => characterMap += (char -> outputCharacters(index)) }
    characterMap
  }

  def createCharacterMap3(): Map[Char, Char] = {
    val inputLines = getLines("sample-input.txt")
    val outputLines = getLines("sample-output.txt")
    var inputCharacters = getCharacters(inputLines.tail, line => line)
    var outputCharacters = getCharacters(outputLines, line => line.substring((line indexOf ": ") + 2))
    var characterMap = Map('z' -> 'q', 'q' -> 'z')
    inputCharacters.zipWithIndex.foreach { case (char, index) => characterMap += (char -> outputCharacters(index)) }
    characterMap
  }

  def createCharacterMap4(): Map[Char, Char] = {
    val inputLines = getLines("sample-input.txt")
    val outputLines = getLines("sample-output.txt")
    var inputCharacters = getCharacters(inputLines.tail, line => line)
    var outputCharacters = getCharacters(outputLines, line => line.substring((line indexOf ": ") + 2))
    var characterMap = Map('z' -> 'q', 'q' -> 'z')
    inputCharacters.zip(outputCharacters)
  }

  def getLines(fileName: String) = Source.fromFile(fileName).getLines().toList

  def getCharacters(lines: List[String], filter: (String => String)) = {
    var characters = List[Char]()
    lines.foreach(line => characters = characters ::: filter(line).toList)
    characters
  }

  def getCharacterMap(input: List[Char], output: List[Char]): Map[Char, Char] = {
    if (input.isEmpty) {
      Map()
    } else {
      Map(input.first -> output.first) ::: getCharacterMap(input.drop(1), output.drop(1))
    }
  }

}
