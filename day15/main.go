package main

import (
	"fmt"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func playGameUntil(until, turnno, cur int, num2turn map[int]int) int {
	for ; turnno < until; turnno += 1 {
		last := cur
		if lastSeen, notFirst := num2turn[cur]; notFirst {
			cur = turnno - lastSeen
		} else {
			cur = 0
		}
		num2turn[last] = turnno
	}
	return cur
}

func withFresh(until int) int {
	input := []int{0, 13, 1, 16, 6, 17}
	num2turn := map[int]int{}
	for turn, num := range input {
		num2turn[num] = turn + 1
	}

	turnno := len(input)
	start := input[turnno-1]
	return playGameUntil(until, turnno, start, num2turn)
}

func main() {
	fmt.Println("Part 1:")
	fmt.Println(withFresh(2020))

	fmt.Println("Part 2:")
	fmt.Println(withFresh(30000000))
}
