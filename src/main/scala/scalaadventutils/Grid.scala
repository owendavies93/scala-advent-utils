package scalaadventutils

import scala.collection.mutable.ArrayBuffer
import scala.math.sqrt

class Grid
    ( val grid:   ArrayBuffer[Boolean]
    , val width:  Int
    , val height: Int
    ) {

    private val on  = "#"
    private val off = "."

    private val nonDiagNeighbourList = List(
        (-1, 0), (0, -1), (1, 0), (0, 1)
    )

    def checkBounds(x: Int, y: Int): Boolean = {
        val xMatch = x match {
            case i if 0 until width contains x => true
            case _                             => false
        }

        val yMatch = y match {
            case i if 0 until height contains i => true
            case _                              => false
        }

        xMatch && yMatch
    }

    def countOn = grid.filter(_ == true).size

    def getOn =
        (0 until height).flatMap(y =>
            (0 until width).map(x => (x, y))
        ).filter(p => get(p._1, p._2))

    def get(x: Int, y: Int): Boolean = {
        var x_ = if (x < 0) width + x  else x
        var y_ = if (y < 0) height + y else y

        grid((y_ % height) * width + (x_ % width))
    }

    def nonDiagNeighbours(x: Int, y: Int, wrapping: Boolean = true) =
        nonDiagNeighbourList.map(n => (x + n._1, y + n._2))
                            .filter(n => wrapping || checkBounds(n._1, n._2))

    def step(stepFn: (Int, Int) => Boolean): Grid = {
        val nextGrid = ArrayBuffer.fill(height * width)(false)

        for (y <- 0 until height) {
            for (x <- 0 until width) {
                nextGrid(y * width + x) = stepFn(x, y)
            }
        }

        new Grid(nextGrid, width, height)
    }

    def flip: Grid = step((x: Int, y: Int) => get(width - x - 1, y))

    def rotate: Grid = step((x: Int, y: Int) => get(height - y - 1, x))

    def transformations: List[Grid] = List(
        this, rotate, rotate.rotate, rotate.rotate.rotate,
        flip, flip.rotate, flip.rotate.rotate, flip.rotate.rotate.rotate
    )

    /*
        Only works correct on square Grids
    */
    def split(size: Int): List[Grid] =
        if (size * size >= width * height) List(this)
        else {
            val groups = grid.grouped(size).zipWithIndex.toList

            val partitions =
                (0 to 1).flatMap(i => groups.filter(_._2 % 2 == i))
                        .map(_._1).grouped(size).map(_.flatten.toArray)
                        .toList

            partitions.map(g => new Grid(ArrayBuffer(g:_*), size, size))
        }

    override def equals(that: Any): Boolean = that match {
        case g: Grid => g.width == width && g.height == height && g.grid.sameElements(grid)
        case _ => false
    }

    override def hashCode() = width.## * height.## * grid.toSeq.##

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

        sb.toString()
    }
}

object GridUtils {

    @throws(classOf[EmptyInputException])
    @throws(classOf[IncorrectSizeException])
    def from2DCharArray(arr: List[String], onChar: Char): Grid = {
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

        new Grid(buff, width, height)
    }

    /*
        Only supports the creation of square grids
        Could be extended to support a height and width if needed
    */
    @throws(classOf[EmptyInputException])
    def from1DCharArray(arr: Array[Char], onChar: Char): Grid = {
        if (arr.isEmpty) {
            throw new EmptyInputException("Can't make grid from empty grid")
        }

        val dimension = sqrt(arr.size).toInt
        val buff = arr.map(_ == onChar)

        new Grid(ArrayBuffer(buff:_*), dimension, dimension)
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

