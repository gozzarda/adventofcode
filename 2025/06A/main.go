package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func Astois(words []string) ([]int, error) {
	nums := make([]int, 0, len(words))
	for _, word := range words {
		num, err := strconv.Atoi(word)
		if err != nil {
			return nil, err
		}
		nums = append(nums, num)
	}
	return nums, nil
}

func main() {
	reader := bufio.NewReader(os.Stdin)

	grid := [][]int{}

	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			log.Fatal(err)
		}

		words := strings.Fields(line)

		nums, err := Astois(words)
		if err == nil {
			grid = append(grid, nums)
		} else {
			total := 0
			for c, word := range words {
				switch word {
				case "+":
					subTotal := 0
					for r := range len(grid) {
						subTotal += grid[r][c]
					}
					total += subTotal
				case "*":
					subTotal := 1
					for r := range len(grid) {
						subTotal *= grid[r][c]
					}
					total += subTotal
				default:
					log.Fatal(err)
				}
			}
			fmt.Println(total)
			return
		}
	}
}
