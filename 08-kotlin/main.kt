import kotlin.system.exitProcess
import java.io.File

data class Vector2(val x: Int, val y: Int)

fun isInBounds(vector: Vector2, maxX: Int, maxY: Int): Boolean {
    return vector.x in 0..maxX && vector.y in 0..maxY
}

fun getAntinodes(
    vector1: Vector2,
    vector2: Vector2,
    maxX: Int,
    maxY: Int,
): List<Vector2> {
    val dx = vector1.x - vector2.x
    val dy = vector1.y - vector2.y

    val antinodes = listOf(
        Vector2(vector1.x + dx, vector1.y + dy),
        Vector2(vector2.x - dx, vector2.y - dy),
    ).filter { vector -> isInBounds(vector, maxX, maxY) }

    return antinodes
}

fun solvePartOne(input: List<String>): Int {
    val maxX = input[0].length - 1
    val maxY = input.size - 1

    var antennas = mutableMapOf<Char, MutableList<Vector2>>()
    var antinodes = mutableSetOf<Vector2>()

    input.forEachIndexed { y, row ->
        row.forEachIndexed { x, cell ->
            if (cell != '.') {
                val thisVector = Vector2(x, y)

                if (antennas.containsKey(cell)) {
                    for (otherVector in antennas[cell]!!) {
                        antinodes.addAll(
                            getAntinodes(thisVector, otherVector, maxX, maxY)
                        )
                    }
                }

                antennas.computeIfAbsent(cell) { mutableListOf() }
                    .add(thisVector)
            }
        }
    }

    return antinodes.size
}


fun main(args: Array<String>) {
    if (args.isEmpty()) {
        System.err.println("Input file not provided")
        exitProcess(1)
    }

    val inputFile = args[0]
    val input = File(inputFile).readLines()

    println("--- Day 8: Resonant Collinearity ---")
    println("Answer for part 1: ${solvePartOne(input)}")
}
