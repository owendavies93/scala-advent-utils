package scalaadventutils

import scala.collection.mutable.ArrayBuffer

class Grid
    ( val grid:   ArrayBuffer[Boolean]
    , val width:  Int
    , val height: Int
    ) {

    private val on  = "#"
    private val off = "."

    def countOn() = grid.filter(_ == true).size

    def get(x: Int, y: Int): Boolean = {
        var x_ = if (x < 0) width + x  else x
        var y_ = if (y < 0) height + y else y

        grid((y_ % height) * width + (x_ % width))
    }

    def step(stepFn: (Int, Int) => Boolean): Grid = {
        val nextGrid = ArrayBuffer.fill(height * width)(false)

        for (y <- 0 until height) {
            for (x <- 0 until width) {
                nextGrid(y * width + x) = stepFn(x, y)
            }
        }

        return new Grid(nextGrid, width, height)
    }

    override def toString(): String = {
        val sb = new StringBuilder

        for (y <- 0 until height) {
            for (x <- 0 until width) {
                sb.append(if(get(x, y)) on else off)
            }

            if (y < width - 1) {
                sb.append("\n")
            }
        }

        return sb.toString()
    }
}

object GridUtils {

    @throws(classOf[EmptyInputException])
    @throws(classOf[IncorrectSizeException])
    def from2DCharArray
        ( arr: List[String]
        , onChar: Char)
        : Grid = {

        if (arr.isEmpty) {
            throw new EmptyInputException("Can't make CA from empty grid")
        }

        val height = arr.size
        val width  = arr(0).size

        val buff = ArrayBuffer.fill(height * width)(false)

        arr.zipWithIndex.foreach {
            case (line, y) => {
                if (line.size != width) {
                    throw new IncorrectSizeException("Incorrect line size")
                }

                line.zipWithIndex.foreach {
                    case (c, x) => {
                        val index = y * width + x
                        buff(index) = c == onChar
                    }
                }
            }
        }

        return new Grid(buff, width, height)
    }

}

final case class EmptyInputException
    ( private val message: String = ""
    , private val cause: Throwable = None.orNull
    ) extends Exception(message, cause)

final case class IncorrectSizeException
    ( private val message: String = ""
    , private val cause: Throwable = None.orNull
    ) extends Exception(message, cause)

