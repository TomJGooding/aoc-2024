package main

import (
    "fmt"
    "os"
    "regexp"
    "strconv"
)

var mulPattern = regexp.MustCompile(`mul\((\d{1,3}),(\d{1,3})\)`)

func solvePartOne(input string) int {
    mulMatches := mulPattern.FindAllStringSubmatch(input, -1)

    answer := 0
    for _, mul:= range mulMatches {
        digit1, _ := strconv.Atoi(mul[1])
        digit2, _ := strconv.Atoi(mul[2])
        answer += digit1 * digit2
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
