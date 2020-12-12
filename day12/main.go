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

func rleft(p position, amt int) position {
	for i := 0; i < amt; i += 1 {
		nx := -p.y
		ny := p.x
		p.x = nx
		p.y = ny
	}
	return p
}

func rright(p position, amt int) position {
	for i := 0; i < amt; i += 1 {
		nx := p.y
		ny := -p.x
		p.x = nx
		p.y = ny
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
			dir = rleft(dir, cmd.by/90)
		case 'R':
			dir = rright(dir, cmd.by/90)
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
			wp = rleft(wp, cmd.by/90)
		case 'R':
			wp = rright(wp, cmd.by/90)
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
