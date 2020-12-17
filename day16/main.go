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

type bound struct {
	lo, hi int
}

func (b bound) validate(v int) bool {
	return b.lo <= v && v <= b.hi
}

type constraint struct {
	bounds []bound
}

func (c constraint) validate(v int) bool {
	for _, bound := range c.bounds {
		if bound.validate(v) {
			return true
		}
	}
	return false
}

type ticket struct {
	values []int
}

// Returns the scanning error rate for a ticket given a set of constraints, and
// whether the ticket is valid (has all fields meeting the constraints).
func (t ticket) scanErrorRate(constraints []constraint) (int, bool) {
	errorRate := 0
	valid := true
checkValues:
	for _, v := range t.values {
		for _, c := range constraints {
			if c.validate(v) {
				continue checkValues
			}
		}
		errorRate += v
		valid = false
	}
	return errorRate, valid
}

func (t ticket) col(i int) int {
	return t.values[i]
}

type data struct {
	fields        map[string]constraint
	yourTicket    ticket
	nearbyTickets []ticket
}

func parse(input string) data {
	splits := strings.Split(input, "\n\n")

	fields := map[string]constraint{}
	reField := regexp.MustCompile(`(.+): (\d+)-(\d+) or (\d+)-(\d+)`)
	for _, sfield := range strings.Split(splits[0], "\n") {
		match := reField.FindStringSubmatch(sfield)
		lo1, _ := strconv.Atoi(match[2])
		hi1, _ := strconv.Atoi(match[3])
		lo2, _ := strconv.Atoi(match[4])
		hi2, _ := strconv.Atoi(match[5])
		bounds := []bound{{lo1, hi1}, {lo2, hi2}}
		fields[match[1]] = constraint{bounds}
	}

	parseTicket := func(s string) ticket {
		values := []int{}
		for _, sv := range strings.Split(s, ",") {
			v, _ := strconv.Atoi(sv)
			values = append(values, v)
		}
		return ticket{values}
	}

	yourTicket := parseTicket(strings.Split(splits[1], "\n")[1])
	snearbyTickets := strings.Split(splits[2], "\n")
	nearbyTickets := []ticket{}
	for _, ticket := range snearbyTickets[1 : len(snearbyTickets)-1] {
		nearbyTickets = append(nearbyTickets, parseTicket(ticket))
	}

	return data{fields, yourTicket, nearbyTickets}
}

func (d data) values() (map[string]constraint, ticket, []ticket) {
	return d.fields, d.yourTicket, d.nearbyTickets
}

func listConstraints(fields map[string]constraint) []constraint {
	lst := []constraint{}
	for _, c := range fields {
		lst = append(lst, c)
	}
	return lst
}

func listFieldNames(fields map[string]constraint) []string {
	lst := []string{}
	for n := range fields {
		lst = append(lst, n)
	}
	return lst
}

func nearbyTicketErrorRate(fields map[string]constraint, tickets []ticket) int {
	rate := 0
	constraints := listConstraints(fields)
	for _, ticket := range tickets {
		prate, _ := ticket.scanErrorRate(constraints)
		rate += prate
	}
	return rate
}

func filterValid(fields map[string]constraint, tickets []ticket) []ticket {
	valid := []ticket{}
	constraints := listConstraints(fields)
	for _, ticket := range tickets {
		if _, ok := ticket.scanErrorRate(constraints); ok {
			valid = append(valid, ticket)
		}
	}
	return valid
}

func lst2set(lst []string) map[string]bool {
	set := map[string]bool{}
	for _, s := range lst {
		set[s] = true
	}
	return set
}

func singleton(m map[string]bool) string {
	for s := range m {
		return s
	}
	panic("Not a singleton")
}

func solveFieldOrder(fields map[string]constraint, tickets []ticket) []string {
	// Part 1 :: assign each column a set of possible field names.
	//           at this point each column may have >= 1 field name options;
	//           later, we will prune this to a discrete solution.
	optsPerColumn := []map[string]bool{}
	numCols := len(tickets[0].values)
	for c := 0; c < numCols; c += 1 {
		colOpts := []string{}
		for _, candField := range listFieldNames(fields) {
			constraint := fields[candField]
			validForAll := true
			for _, ticket := range tickets {
				if !constraint.validate(ticket.col(c)) {
					if c == 2 {
					}
					validForAll = false
				}
			}
			if validForAll {
				colOpts = append(colOpts, candField)
			}
		}
		if c == 2 {
		}
		optsPerColumn = append(optsPerColumn, lst2set(colOpts))
	}

	// Part 2 :: solve the field constraints.
	//           just figure out what column has to be a certain field, then
	//           reduce all other column options; rinse and repeat until
	//           we're done.
	allDone := func(solution []map[string]bool) bool {
		for _, opts := range solution {
			if len(opts) > 1 {
				return false
			}
		}
		return true
	}
	solved := map[int]bool{}
	for !allDone(optsPerColumn) {
	findColSolution:
		for n, opts := range optsPerColumn {
			if len(opts) == 1 && !solved[n] {
				constrainedSolution := singleton(opts)
				for m, otherOpts := range optsPerColumn {
					if n == m {
						continue
					}
					delete(otherOpts, constrainedSolution)
				}
				solved[n] = true
				break findColSolution
			}
		}
	}

	order := []string{}
	for _, field := range optsPerColumn {
		order = append(order, singleton(field))
	}
	return order
}

func productFieldsDeparture(fields []string, t ticket) int {
	product := 1
	for i, field := range fields {
		if strings.HasPrefix(field, "departure") {
			product *= t.col(i)
		}
	}
	return product
}

func main() {
	fi, err := ioutil.ReadFile("day16/input.txt")
	check(err)
	ficontent := string(fi)

	fields, yourTicket, nearbyTickets := parse(ficontent).values()

	fmt.Println("Part 1:")
	fmt.Println(nearbyTicketErrorRate(fields, nearbyTickets))

	fmt.Println("Part 2:")
	validTickets := append(filterValid(fields, nearbyTickets), yourTicket)
	fieldOrder := solveFieldOrder(fields, validTickets)
	departureProduct := productFieldsDeparture(fieldOrder, yourTicket)
	fmt.Println(departureProduct)
}
