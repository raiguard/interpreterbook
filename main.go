package main

import (
	"fmt"
	"interpreterbook/repl"
	"os"
)

func main() {
	fmt.Println("Monkey alpha version	https://interpreterbook.com")
	repl.Start(os.Stdin, os.Stdout)
}
