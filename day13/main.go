package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

var NONE = -1

func part1(departTime int, buses []int) int {
	waitTime := 1_000_000
	busId := 1_000_000
	timeToNext := func(n int) int {
		return ((n - (departTime % n)) % n)
	}
	for _, b := range buses {
		if b == -1 {
			continue
		}
		if timeToNext(b) < timeToNext(busId) {
			waitTime = timeToNext(b)
			busId = b
		}
	}
	return waitTime * busId
}

func main() {
	fi, err := ioutil.ReadFile("day13/input.txt")
	check(err)
	ficontent := string(fi)
	lines := strings.Split(ficontent, "\n")
	lines = lines[:len(lines)-1]
	departTime, err := strconv.Atoi(lines[0])
	check(err)
	buses := []int{}
	for _, n := range strings.Split(lines[1], ",") {
		if n == "x" {
			buses = append(buses, -1)
		} else {
			bus, err := strconv.Atoi(n)
			check(err)
			buses = append(buses, bus)
		}
	}

	fmt.Println("Part 1:")
	fmt.Println(part1(departTime, buses))

	fmt.Println("Part 2:")
}
