package scalaadventutils

import org.scalatest.funsuite.AnyFunSuite

class GridSpec extends AnyFunSuite {
    val grid = List[String](
        "#.#",
        ".#.",
        "#.#"
    )

    test("Grid: get") {
        val grid1 = GridUtils.from2DCharArray(grid, '#')

        assert( grid1.get(0, 0))
        assert(!grid1.get(0, 1))
        assert(!grid1.get(1, 0))

        assert( grid1.get(3, 0))
        assert( grid1.get(3, 3))
        assert(!grid1.get(7, 0))

        assert( grid1.get(-1, 0))

        assert(grid.mkString("\n") == grid1.toString())
    }

    test("Grid: step") {
        var grid1 = GridUtils.from2DCharArray(grid, '#')

        def stepFn(x: Int, y: Int): Boolean = {
            return x <= 1 && y <= 1
        }

        grid1 = grid1.step(stepFn)

        val grid2 = List[String](
            "##.",
            "##.",
            "..."
        )

        assert(grid2.mkString("\n") == grid1.toString())
    }
}

