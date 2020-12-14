package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func sumMemory(memory map[int]int) int {
	res := 0
	for _, v := range memory {
		res += v
	}
	return res
}

func updateMemory1(memory *map[int]int, mask *string, addr, value int) {
	for bit, idx := 1, len(*mask)-1; idx >= 0; bit, idx = bit*2, idx-1 {
		switch (*mask)[idx] {
		case '0':
			value &^= bit
		case '1':
			value |= bit
		}
	}
	(*memory)[addr] = value
}

func updateMemory2(memory *map[int]int, mask *string, addr, value int) {
	permuteBits := []int{}
	for bit, idx := 1, len(*mask)-1; idx >= 0; bit, idx = bit*2, idx-1 {
		switch (*mask)[idx] {
		case '1':
			addr |= bit
		case 'X':
			permuteBits = append(permuteBits, bit)
		}
	}
	addrPermutations := []int{addr}
	for _, bit := range permuteBits {
		nextPermutes := []int{}
		for _, addr := range addrPermutations {
			// create permutations of bit set to 0 and 1
			nextPermutes = append(nextPermutes, addr&^bit, addr|bit)
		}
		addrPermutations = nextPermutes
	}
	for _, addr := range addrPermutations {
		(*memory)[addr] = value
	}
}

func emulate(lines []string, updateMemory func(*map[int]int, *string, int, int)) int {
	memory := map[int]int{}
	mask := ""
	reMask := regexp.MustCompile("mask = ([01X]+)")
	reWrite := regexp.MustCompile(`mem\[(\d+)\] = (\d+)`)
	for _, line := range lines {
		maskMatch := reMask.FindStringSubmatch(line)
		if maskMatch != nil {
			mask = maskMatch[1]
			continue
		}
		writeMatch := reWrite.FindStringSubmatch(line)
		addr, err := strconv.Atoi(writeMatch[1])
		check(err)
		value, err := strconv.Atoi(writeMatch[2])
		check(err)
		updateMemory(&memory, &mask, addr, value)
	}
	return sumMemory(memory)
}

func main() {
	fi, err := ioutil.ReadFile("day14/input.txt")
	check(err)
	ficontent := string(fi)
	lines := strings.Split(ficontent, "\n")
	lines = lines[:len(lines)-1]

	fmt.Println("Part 1:")
	fmt.Println(emulate(lines, updateMemory1))

	fmt.Println("Part 2:")
	fmt.Println(emulate(lines, updateMemory2))
}
