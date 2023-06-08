package parser

import (
	"fmt"
	"interpreterbook/ast"
	"interpreterbook/lexer"
	"interpreterbook/token"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestLetStatements(t *testing.T) {
	input := `
let x = 5;
let y = 10;
let foobar = 838383;
`
	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)
	assert.NotNil(t, program)
	assert.Equal(t, 3, len(program.Statements))

	tests := []struct {
		expectedIdentifier string
	}{
		{"x"},
		{"y"},
		{"foobar"},
	}

	for i, tt := range tests {
		stmt := program.Statements[i]
		testLetStatement(t, stmt, tt.expectedIdentifier)
	}
}

func TestReturnStatements(t *testing.T) {
	input := `
return 5;
return 10;
return 993322;
`
	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)

	assert.Equal(t, 3, len(program.Statements))

	for _, stmt := range program.Statements {
		returnStmt, ok := stmt.(*ast.ReturnStatement)
		assert.True(t, ok)
		assert.Equal(t, "return", returnStmt.TokenLiteral())
	}
}

func TestBlockStatement(t *testing.T) {
	input := "{ let i = 5; i + 1 }"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	assert.Equal(t, 1, len(program.Statements))

	stmt, ok := program.Statements[0].(*ast.BlockStatement)
	assert.True(t, ok)

	assert.Equal(t, 2, len(stmt.Statements))
	assert.Equal(t, "let i = 5;", stmt.Statements[0].String())
	assert.Equal(t, "(i + 1)", stmt.Statements[1].String())
}

func TestIdentifierExpression(t *testing.T) {
	input := "foobar;"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	assert.Equal(t, 1, len(program.Statements))
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	assert.True(t, ok)

	ident, ok := stmt.Expression.(*ast.Identifier)
	assert.True(t, ok)
	assert.Equal(t, "foobar", ident.Value)
	assert.Equal(t, "foobar", ident.TokenLiteral())
}

func TestIntegerLiteralExpression(t *testing.T) {
	input := "5;"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)
	assert.Equal(t, 1, len(program.Statements))

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	assert.True(t, ok)

	literal, ok := stmt.Expression.(*ast.IntegerLiteral)
	assert.True(t, ok)
	assert.Equal(t, int64(5), literal.Value)
	assert.Equal(t, "5", literal.TokenLiteral())
}

func TestParsingPrefixExpressions(t *testing.T) {
	prefixTests := []struct {
		input        string
		operator     string
		integerValue int64
	}{
		{"!5", "!", 5},
		{"-15", "-", 15},
	}

	for _, tt := range prefixTests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)
		assert.Equal(t, 1, len(program.Statements))
		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		assert.True(t, ok)

		exp, ok := stmt.Expression.(*ast.PrefixExpression)
		assert.True(t, ok)
		assert.Equal(t, tt.operator, exp.Operator)
		testIntegerLiteral(t, exp.Right, tt.integerValue)
	}
}

func TestParsingInfixExpressions(t *testing.T) {
	infixTests := []struct {
		input      string
		leftValue  int64
		operator   string
		rightValue int64
	}{
		{"5 + 5;", 5, "+", 5},
		{"5 - 5;", 5, "-", 5},
		{"5 * 5;", 5, "*", 5},
		{"5 / 5;", 5, "/", 5},
		{"5 > 5;", 5, ">", 5},
		{"5 < 5;", 5, "<", 5},
		{"5 == 5;", 5, "==", 5},
		{"5 != 5;", 5, "!=", 5},
	}

	for _, tt := range infixTests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		assert.Equal(t, 1, len(program.Statements))
		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		assert.True(t, ok)

		exp, ok := stmt.Expression.(*ast.InfixExpression)
		assert.True(t, ok)

		testIntegerLiteral(t, exp.Left, tt.leftValue)

		assert.Equal(t, tt.operator, exp.Operator)

		testIntegerLiteral(t, exp.Right, tt.rightValue)
	}
}

func TestParsingParenthesizedExpressions(t *testing.T) {
	infixTests := []struct {
		input string
		stmt  ast.Statement
	}{
		{"5 + (5 + 5) / 5;", &ast.ExpressionStatement{
			Token: token.Token{Type: token.INT, Literal: "5"},
			Expression: &ast.InfixExpression{
				Token:    token.Token{Type: token.PLUS, Literal: "+"},
				Left:     &ast.IntegerLiteral{Token: token.Token{Type: token.INT, Literal: "5"}, Value: 5},
				Operator: "+",
				Right: &ast.InfixExpression{
					Token: token.Token{Type: token.SLASH, Literal: "/"},
					Left: &ast.PrefixExpression{
						Token:    token.Token{Type: token.LPAREN, Literal: "("},
						Operator: "(",
						Right: &ast.InfixExpression{
							Token:    token.Token{Type: token.PLUS, Literal: "+"},
							Left:     &ast.IntegerLiteral{Token: token.Token{Type: token.INT, Literal: "5"}, Value: 5},
							Operator: "+",
							Right:    &ast.IntegerLiteral{Token: token.Token{Type: token.INT, Literal: "5"}, Value: 5},
						},
					},
					Operator: "+",
					Right:    &ast.IntegerLiteral{Token: token.Token{Type: token.INT, Literal: "5"}, Value: 5},
				},
			},
		}},
	}

	for _, tt := range infixTests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		assert.Equal(t, 1, len(program.Statements))
		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		assert.True(t, ok)

		assert.Equal(t, "(5 + ((((5 + 5)) / 5))", stmt.String())
	}
}

func TestBooleanExpression(t *testing.T) {
	input := "true;"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	assert.Equal(t, 1, len(program.Statements))

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	assert.True(t, ok)

	expr, ok := stmt.Expression.(*ast.Boolean)
	assert.True(t, ok)

	assert.Equal(t, true, expr.Value)
	assert.Equal(t, "true", expr.TokenLiteral())
}

func TestIfExpression(t *testing.T) {
	input := "if (i + 3) { return true; } else { false }"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	assert.Equal(t, 1, len(program.Statements))

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	assert.True(t, ok)

	expr, ok := stmt.Expression.(*ast.IfExpression)
	assert.True(t, ok)

	assert.Equal(t, "(((i + 3))", expr.Condition.String())
	assert.Equal(t, "{ return true; }", expr.Consequence.String())
	assert.Equal(t, "{ false }", expr.Alternative.String())
}

func checkParserErrors(t *testing.T, p *Parser) {
	errors := p.Errors()
	if len(errors) == 0 {
		return
	}

	t.Errorf("parser has %d errors", len(errors))
	for _, msg := range errors {
		t.Errorf("parser error: %q", msg)
	}
	t.FailNow()
}

func testLetStatement(t *testing.T, s ast.Statement, name string) {
	assert.Equal(t, "let", s.TokenLiteral())
	letStmt, ok := s.(*ast.LetStatement)
	assert.True(t, ok)
	assert.Equal(t, name, letStmt.Name.Value)
	assert.Equal(t, name, letStmt.Name.TokenLiteral())
}

func testIntegerLiteral(t *testing.T, il ast.Expression, value int64) {
	integ, ok := il.(*ast.IntegerLiteral)
	assert.True(t, ok)
	assert.Equal(t, value, integ.Value)
	assert.Equal(t, fmt.Sprintf("%d", value), integ.TokenLiteral())
}
