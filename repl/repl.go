package repl

import (
	"fmt"
	"interpreterbook/lexer"
	"interpreterbook/parser"
	"interpreterbook/token"
	"io"
	"os"

	"github.com/chzyer/readline"
)

const PROMPT = ">> "

func Start(in io.Reader, out io.Writer) {
	rl, err := readline.New("> ")
	if err != nil {
		panic(err)
	}
	defer rl.Close()

	for {
		line, err := rl.Readline()
		if err != nil {
			break
		}

		l := lexer.New(line)

		for tok := l.NextToken(); tok.Type != token.EOF; tok = l.NextToken() {
			fmt.Fprintf(out, "%+v\n", tok)
		}

		l = lexer.New(line)
		p := parser.New(l)
		program := p.ParseProgram()
		for _, err := range p.Errors() {
			fmt.Fprintln(os.Stderr, err)
		}
		if len(p.Errors()) > 0 {
			continue
		}
		fmt.Println(program.String())
	}
}
