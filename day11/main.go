package main

import (
	"fmt"
	"io/ioutil"
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

type seats [][]rune

func countOccurrences(seats seats) int {
	count := 0
	for _, row := range seats {
		for _, seat := range row {
			if seat == '#' {
				count += 1
			}
		}
	}
	return count
}

func fixedPoint(f func(seats) seats, x seats) seats {
	y := f(x)
	for r := range x {
		for c := range x[r] {
			if x[r][c] != y[r][c] {
				return fixedPoint(f, y)
			}
		}
	}
	return x
}

func validSeat(seats seats, r, c int) bool {
	return r >= 0 && c >= 0 && r < len(seats) && c < len(seats[0])
}

func stepBoarding(occupiedAround func(seats, int, int) int, updateSeat func(rune, int) rune, plane seats) seats {
	newPlane := [][]rune{}
	for r := range plane {
		nrow := []rune{}
		for c := range plane[r] {
			nseat := updateSeat(plane[r][c], occupiedAround(plane, r, c))
			nrow = append(nrow, nseat)
		}
		newPlane = append(newPlane, nrow)
	}
	return newPlane
}

func main() {
	fi, err := ioutil.ReadFile("day11/input.txt")
	check(err)
	ficontent := string(fi)
	lines := strings.Split(ficontent, "\n")
	lines = lines[:len(lines)-1]

	plane := [][]rune{}
	for _, line := range lines {
		plane = append(plane, []rune(line))
	}

	fmt.Print("Part 1:\n")
	{
		occupiedAround := func(plane seats, r, c int) int {
			occupied := 0
			for dr := -1; dr <= 1; dr += 1 {
				for dc := -1; dc <= 1; dc += 1 {
					nr := r + dr
					nc := c + dc
					if (nr == r && nc == c) || !validSeat(plane, nr, nc) {
						continue
					}
					if plane[nr][nc] == '#' {
						occupied += 1
					}
				}
			}
			return occupied
		}
		updateSeat := func(seat rune, occupiedAround int) rune {
			if seat == 'L' && occupiedAround == 0 {
				return '#'
			}
			if seat == '#' && occupiedAround >= 4 {
				return 'L'
			}
			return seat
		}
		step := func(plane seats) seats {
			return stepBoarding(occupiedAround, updateSeat, plane)
		}

		fmt.Println(countOccurrences(fixedPoint(step, plane)))
	}

	fmt.Print("Part 2:\n")
	{
		occupiedAround := func(plane seats, r, c int) int {
			occupied := 0
			for dr := -1; dr <= 1; dr += 1 {
				for dc := -1; dc <= 1; dc += 1 {
					if dr == 0 && dc == 0 {
						continue
					}
					for nr, nc := r+dr, c+dc; validSeat(plane, nr, nc); nr, nc = nr+dr, nc+dc {
						if plane[nr][nc] == '#' {
							occupied += 1
							break
						}
						if plane[nr][nc] == 'L' {
							break
						}
					}
				}
			}
			return occupied
		}
		updateSeat := func(seat rune, occupiedAround int) rune {
			if seat == 'L' && occupiedAround == 0 {
				return '#'
			}
			if seat == '#' && occupiedAround >= 5 {
				return 'L'
			}
			return seat
		}
		step := func(plane seats) seats {
			return stepBoarding(occupiedAround, updateSeat, plane)
		}

		fmt.Println(countOccurrences(fixedPoint(step, plane)))
	}
}
