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

type cmd struct {
	nm byte
	by int
}

type position struct {
	x int
	y int
}

func rotr(p position, by int) position {
	for i := 0; i < by; i += 1 {
		p.x, p.y = p.y, -p.x
	}
	return p
}

func route1(cmds []cmd) position {
	pos := position{x: 0, y: 0}
	dir := position{x: 1, y: 0}
	for _, cmd := range cmds {
		switch cmd.nm {
		case 'N':
			pos.y += cmd.by
		case 'S':
			pos.y -= cmd.by
		case 'E':
			pos.x += cmd.by
		case 'W':
			pos.x -= cmd.by
		case 'L':
			dir = rotr(dir, 4-((cmd.by/90)%4))
		case 'R':
			dir = rotr(dir, ((cmd.by / 90) % 4))
		case 'F':
			pos.x += dir.x * cmd.by
			pos.y += dir.y * cmd.by
		}
	}
	return pos
}

func route2(cmds []cmd) position {
	pos := position{x: 0, y: 0}
	wp := position{x: 10, y: 1}
	for _, cmd := range cmds {
		switch cmd.nm {
		case 'N':
			wp.y += cmd.by
		case 'S':
			wp.y -= cmd.by
		case 'E':
			wp.x += cmd.by
		case 'W':
			wp.x -= cmd.by
		case 'L':
			wp = rotr(wp, 4-((cmd.by/90)%4))
		case 'R':
			wp = rotr(wp, ((cmd.by / 90) % 4))
		case 'F':
			pos.x += wp.x * cmd.by
			pos.y += wp.y * cmd.by
		}
	}
	return pos
}

func abs(n int) int {
	if n < 0 {
		return -n
	}
	return n
}

func distMan(p position) int {
	return abs(p.x) + abs(p.y)
}

func main() {
	fi, err := ioutil.ReadFile("day12/input.txt")
	check(err)
	ficontent := string(fi)
	lines := strings.Split(ficontent, "\n")
	lines = lines[:len(lines)-1]

	cmds := []cmd{}
	for _, line := range lines {
		by, err := strconv.Atoi(line[1:])
		check(err)
		nm := line[0]
		cmds = append(cmds, cmd{nm, by})
	}

	fmt.Print("Part 1:\n")
	fmt.Println(distMan(route1(cmds)))

	fmt.Print("Part 2:\n")
	fmt.Println(distMan(route2(cmds)))
}
