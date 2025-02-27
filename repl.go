package main

import (
	"bufio"
	"fmt"
	"os"
)

// creates a new top level frame with default
// bindings for special forms
func NewTopLevelFrame() Frame {
	newSpecialForm := func(s string) Value {
		return Value{
			Car:   nil,
			Cdr:   nil,
			Type:  SpecialForm,
			Value: s,
		}
	}
	return Frame{
		parent: nil,
		bindings: map[string]Value{
			"atom?":         newSpecialForm("atom?"),         // returns true if argument is an atom (i.e. not a cons)
			"car":           newSpecialForm("car"),           // returns the first element of a cons cell
			"cdr":           newSpecialForm("cdr"),           // returns the second element of a cons cell
			"cond":          newSpecialForm("cond"),          // conditional
			"cons":          newSpecialForm("cons"),          // join two elements into a new cons cell
			"label":         newSpecialForm("label"),         // associate a symbol with something
			"lambda":        newSpecialForm("lambda"),        // create a new function (evaluates arguments then runs in lower environment)
			"macro":         newSpecialForm("macro"),         // create a new macro (does not evaluate arguments and runs in caller's environment)
			"eq?":           newSpecialForm("eq?"),           // test if 2 things are structurally equal
			"quote":         newSpecialForm("quote"),         // pass second argument without evaluating
			">":             newSpecialForm(">"),             // test if l is greater than r
			"+":             newSpecialForm("+"),             // add values
			"-":             newSpecialForm("-"),             // subtract values
			"*":             newSpecialForm("*"),             // multiply values
			"/":             newSpecialForm("/"),             // divide valuesn
			"%":             newSpecialForm("%"),             // take remainder
			"gensym":        newSpecialForm("gensym"),        // create a new guaranteed unique symbol
			"set!":          newSpecialForm("set!"),          // look up a binding and update its value
			"type":          newSpecialForm("type"),          // get the type of a value
			"macroexpand-1": newSpecialForm("macroexpand-1"), // expand a macro without evaluating
			"bound?":        newSpecialForm("bound?"),        // return whether a symbol is bound
		},
	}
}

func LoadFile(fileName string, frame Frame) (Frame, error) {
	bytes, err := os.ReadFile(fileName)
	loadedFrame := &frame

	if err != nil {
		return frame, err
	}
	body := string(bytes[:])
	for body != "" {
		var parsedExpression Value
		var err error
		body, parsedExpression, err = Read(body)
		if err != nil {
			return frame, err
		}
		_, err = Eval(parsedExpression, loadedFrame)
		if err != nil {
			return frame, err
		}
	}
	return *loadedFrame, nil
}

func Repl(frame *Frame) {
	fmt.Printf("Lisp interactive session\n")
	scanner := bufio.NewScanner(os.Stdin)
	for {
		var userInput string
		fmt.Printf("\n")
		scanner.Scan()
		userInput = scanner.Text()
		for userInput != "" {
			var parsedExpression Value
			var err error
			userInput, parsedExpression, err = Read(userInput)
			if err != nil {
				fmt.Printf("%v", err)
				continue
			}
			var output Value
			output, err = Eval(parsedExpression, frame)
			if err != nil {
				fmt.Printf("%v", err)
				continue
			}
			fmt.Printf("%v", Print(output))
		}
	}
}

func main() {
	frame := NewTopLevelFrame()
	frame, loadErr := LoadFile("stdlib.lisp", frame)
	if loadErr != nil {
		fmt.Printf("error loading file: %v", loadErr)
		return
	}
	Repl(&frame)
	//	_, v, _ := Read("((lambda () 3))")
	//	result, _, _ := Eval(v, frame)
	//	fmt.Printf("%v", Print(result))

}
