package main

import (
    "fmt"
    "os"
    "regexp"
    "strconv"
)

func solvePartOne(input string) int {
    mulPattern, _ := regexp.Compile(`mul\(\d{1,3},\d{1,3}\)`)
    digitPattern, _ := regexp.Compile(`\d+`)

    mulMatches := mulPattern.FindAllString(input, -1)

    answer := 0
    for _, mul:= range mulMatches {
        digitMatches := digitPattern.FindAllString(mul, 2)
        d1, _ := strconv.Atoi(digitMatches[0])
        d2, _ := strconv.Atoi(digitMatches[1])
        answer += d1 * d2
    }

    return answer
}

func main() {
    if len(os.Args) < 2 {
        fmt.Println("Input file not provided")
        os.Exit(1)
    }
    inputFile := os.Args[1]
    data, err := os.ReadFile(inputFile)
    if err != nil {
        panic(err)
    }
    input := string(data)

    fmt.Println("--- Day 3: Mull It Over ---")
    fmt.Printf("Answer for part 1: %d\n", solvePartOne(input))

}
