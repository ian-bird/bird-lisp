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
			"assemble":      newSpecialForm("assemble"),      // calls the assembler for compiled object code
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

	frame, loadErr = LoadFile("compiled-lib.lisp", frame)
	if loadErr != nil {
		fmt.Printf("error loading file: %v", loadErr)
		return
	}

	// _, input, _ := Read("(to-assembly '(lambda (expression cont)(cond ((atom? expression)(if (atom? cont)(` , cont , expression)(substitute (car (cadr cont)) expression (nth cont 2))))((and (eq? (car expression) 'lambda))  (` , cont ,(M expression)))((and (eq? (count expression) 3) (eq? (car expression) 'label))(if (atom? (nth expression 2))(` call-with-cont label ,(cadr expression) ,(nth expression 2) , cont)(T (nth expression 2)(` lambda (arg #) (call-with-cont label , (cadr expression) arg # , cont)))))((eq? 'cond (car expression)) (foldr (lambda (pred-and-consq acc)(if (atom? (car pred-and-consq))(` branch ,(car pred-and-consq) ,(T (cadr pred-and-consq) cont) , acc)(T (car pred-and-consq) (` lambda (pred-sym #) (branch pred-sym # ,(T (cadr pred-and-consq) cont) , acc)))))(if (eq? (car (last expression)) 'else)(T (cadr (last expression)) cont)(T '() cont))(cdr (if (eq? (car (last expression)) 'else)(drop-last expression)expression))))((and (eq? (count expression) 2) (eq? 'quote (car expression)))(` , cont (quote , (cadr expression))))((eq? '(gensym) expression) (` call-with-cont gensym , cont))((>= (count expression) 1) (let ((gensyms (map (lambda (_) (gensym)) expression))(gensyms-and-symbols (zip gensyms expression)))(foldr (lambda (gensym-and-symbol acc)(if (atom? (cadr gensym-and-symbol))acc(T (cadr gensym-and-symbol)(` lambda (, (car gensym-and-symbol)) , acc))))(` call-with-cont ,@ (map (lambda (gensym-and-symbol)((if (atom? (cadr gensym-and-symbol))cadrcar)gensym-and-symbol))gensyms-and-symbols) , cont)gensyms-and-symbols)))(else '()))))")
	// result, err := Eval(input, &frame)

	// if err != nil {
	// 	fmt.Printf("%v", err)
	// }

	// fmt.Printf("%v", Print(result))
	Repl(&frame)
}
