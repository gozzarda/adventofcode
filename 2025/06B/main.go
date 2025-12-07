package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strings"
	"unicode"
)

func main() {
	reader := bufio.NewReader(os.Stdin)

	colEmpty := []bool{}
	colValue := []int{}

	for {
		line, err := reader.ReadString('\n')
		for len(colEmpty) < len(line) {
			colEmpty = append(colEmpty, true)
			colValue = append(colValue, 0)
		}

		if !strings.ContainsAny(line, "+*") {
			for i, r := range line {
				if unicode.IsDigit(r) {
					colEmpty[i] = false
					colValue[i] = 10*colValue[i] + int(r-'0')
				}
			}
		} else {
			total := 0
			subTotal := 0
			currOp := '?'
			for i, r := range line {
				if !unicode.IsSpace(r) {
					total += subTotal
					currOp = r
					switch currOp {
					case '+':
						subTotal = 0
					case '*':
						subTotal = 1
					default:
						log.Fatal("Invalid op char", currOp)
					}
				}

				if !colEmpty[i] {
					switch currOp {
					case '+':
						subTotal += colValue[i]
					case '*':
						subTotal *= colValue[i]
					default:
						log.Fatal("Invalid op state", currOp)
					}
				}
			}
			total += subTotal

			fmt.Println(total)
			return
		}

		if err == io.EOF {
			break
		}
		if err != nil {
			log.Fatal(err)
		}
	}
}
