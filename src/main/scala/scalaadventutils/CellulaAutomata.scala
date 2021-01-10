package scalaadventutils

import scala.collection.mutable.ArrayBuffer

class CellulaAutomata
    ( grid: ArrayBuffer[Boolean]
    , width: Int
    , height: Int
    ) extends Grid(grid, width, height) {

    private val neighbourList = List(
        (-1, -1), (-1, 0), (0, -1), (1, -1),
        (1, 1), (-1, 1), (1, 0), (0, 1)
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

        return xMatch && yMatch
    }

    override def get(x: Int, y: Int) =
        if (checkBounds(x, y)) grid(y * width + x) else false

    def neighbours(x: Int, y: Int) =
        neighbourList.map(n => (x + n._1, y + n._2))
                     .filter(n => checkBounds(n._1, n._2))

    override def step(stepFn: (Int, Int) => Boolean): CellulaAutomata = {
        val nextGrid = ArrayBuffer.fill(height * width)(false)

        for (y <- 0 until width) {
            for (x <- 0 until height) {
                nextGrid(y * width + x) = stepFn(x, y)
            }
        }

        return new CellulaAutomata(nextGrid, width, height)
    }
}

object CAUtils {

    @throws(classOf[EmptyInputException])
    @throws(classOf[IncorrectSizeException])
    def from2DCharArray
        ( arr: List[String]
        , onChar: Char)
        : CellulaAutomata = {

        val grid = GridUtils.from2DCharArray(arr, onChar)
        return new CellulaAutomata(grid.grid, grid.width, grid.height)
    }
}
