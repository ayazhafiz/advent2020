package main

import (
	"fmt"
	"io/ioutil"
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

func parseDeck(lines []string) deck {
	cards := []int{}
	for _, line := range lines {
		if line == "" {
			continue
		}
		c, err := strconv.Atoi(line)
		check(err)
		cards = append(cards, c)
	}
	return deck{cards}
}

func parse2players(input string) (deck, deck) {
	players := strings.Split(input, "\n\n")
	deck1, deck2 := strings.Split(players[0], "\n"), strings.Split(players[1], "\n")
	p1 := parseDeck(deck1[1:])
	p2 := parseDeck(deck2[1:])
	return p1, p2
}

type deck struct {
	cards []int
}

func (d *deck) draw() int {
	top := d.cards[0]
	d.cards = d.cards[1:]
	return top
}

func (d *deck) add(card int) *deck {
	d.cards = append(d.cards, card)
	return d
}

func (d *deck) score() int {
	l := len(d.cards)
	total := 0
	for i, mult := 0, l; i < l; i, mult = i+1, mult-1 {
		total += mult * d.cards[i]
	}
	return total
}

func (d *deck) size() int {
	return len(d.cards)
}

func (d *deck) empty() bool {
	return d.size() == 0
}

func (d *deck) pr() string {
	return fmt.Sprint(d.cards)
}

func (d *deck) take(n int) deck {
	cp := make([]int, n)
	copy(cp, d.cards[:n])
	return deck{cards: cp}
}

func score(winner int, deck1, deck2 deck) int {
	switch winner {
	case 1:
		return deck1.score()
	case 2:
		return deck2.score()
	}
	panic("Winner ID not valid!")
}

func combat(deck1, deck2 deck) ( /* winner */ int, deck, deck) {
	for !deck1.empty() && !deck2.empty() {
		top1, top2 := deck1.draw(), deck2.draw()
		if top1 > top2 {
			deck1.add(top1).add(top2)
		} else {
			deck2.add(top2).add(top1)
		}
	}
	if deck1.empty() {
		return 2, deck1, deck2
	}
	return 1, deck1, deck2
}

func hashDecks(deck1, deck2 deck) string {
	return deck1.pr() + "|" + deck2.pr()
}

func combatRecursive(deck1, deck2 deck) ( /* winner */ int, deck, deck) {
	seen := map[string]bool{}
	for !deck1.empty() && !deck2.empty() {
		roundHash := hashDecks(deck1, deck2)
		if seen[roundHash] {
			return 1, deck1, deck2
		}
		seen[roundHash] = true

		top1, top2 := deck1.draw(), deck2.draw()

		if deck1.size() >= top1 && deck2.size() >= top2 {
			winner, _, _ := combatRecursive(deck1.take(top1), deck2.take(top2))
			switch winner {
			case 1:
				deck1.add(top1).add(top2)
			case 2:
				deck2.add(top2).add(top1)
			}
		} else if top1 > top2 {
			deck1.add(top1).add(top2)
		} else {
			deck2.add(top2).add(top1)
		}
	}
	if deck1.empty() {
		return 2, deck1, deck2
	}
	return 1, deck1, deck2
}

func main() {
	fi, err := ioutil.ReadFile("day22/input.txt")
	check(err)
	ficontent := string(fi)

	{
		p1, p2 := parse2players(ficontent)

		fmt.Println("Part 1:")
		fmt.Println(score(combat(p1, p2)))
	}

	{
		p1, p2 := parse2players(ficontent)

		fmt.Println("Part 2:")
		fmt.Println(score(combatRecursive(p1, p2)))
	}
}
