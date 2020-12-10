package main

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strconv"
	"strings"
)

func max(a, b int) int {
	if a < b {
		return b
	}
	return a
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	fi, err := ioutil.ReadFile("day10/input.txt")
	check(err)
	ficontent := string(fi)
	lines := strings.Split(ficontent, "\n")
	lines = lines[:len(lines)-1]

	adapters := []int{}

	for _, i := range lines {
		j, err := strconv.Atoi(i)
		if err != nil {
			panic(err)
		}
		adapters = append(adapters, j)
	}
	sort.Ints(adapters)
	adapters = append(adapters, adapters[len(adapters)-1] + 3)

	fmt.Print("Part 1:\n")
	base := 0
	diffs := []int{0, 0, 0, 0}
	for _, adapter := range adapters {
		diffs[adapter-base] += 1
		base = adapter
	}
	fmt.Print(diffs[1] * diffs[3])

	fmt.Print("\nPart 2:\n")
	max_adapter := adapters[len(adapters) - 1]
	connect_ways := make([]int, max_adapter+1)
	connect_ways[0] = 1
	known_adapters := map[int]bool{}
	for _, adapter := range adapters {
		known_adapters[adapter] = true
	}
	for i := 1; i <= max_adapter; i += 1 {
		_, exists := known_adapters[i]
		if !exists {
			continue
		}
		for j := max(i-3, 0); j < i; j += 1 {
			connect_ways[i] += connect_ways[j]
		}
	}
        fmt.Printf("%d\n", connect_ways[max_adapter])
}
