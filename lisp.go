package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"strings"
)

// this is the type enum for values
type ValueType int

// these are all the valid types for a value
const (
	Nil         ValueType = iota // a nil value, for end of list etc
	String                       // a string of characters
	Number                       // a float64
	Symbol                       // sort of like a string, but atomic. Also used for bindings.
	Boolean                      // true or false
	Function                     // a callable function
	Macro                        // a callable macro
	SpecialForm                  // one of the forms handled by the underlying system, not the interpreter
	ConsCell                     // links together other values
)

// this is a value struct.
// it can contain two pointers and no data
// or it can contain a value
type Value struct {
	Car   *Value    // left pointer of a consCell
	Cdr   *Value    // right pointer of a consCell
	Type  ValueType // the type of this value
	Value any       // the value (if any)
}

// a frame contains bindings that associate
// certain strings (the value of symbol Values)
// with other values
type Frame struct {
	parent   *Frame           // the frame above this one
	bindings map[string]Value // its bindings
}

// this is a global counter used to prevent gensym collisions
var gensymCounter = 0

// converta a list of values (car points to element, cdr to tail)
// into an array of elements
func toArray(value Value) []Value {
	result := make([]Value, 0)
	for value.Cdr != nil {
		result = append(result, *value.Car)
		value = *value.Cdr
	}
	return result
}

// converts an array of values into a list (car points to element, cdr to tail)
func toList(values []Value) Value {
	if len(values) == 0 {
		return Value{
			Car:   nil,
			Cdr:   nil,
			Type:  Nil,
			Value: nil,
		}
	} else {
		return Value{
			Car:   &values[0],
			Cdr:   &[]Value{toList(values[1:])}[0],
			Type:  ConsCell,
			Value: nil,
		}
	}
}

// returns true if 2 values are equal, checks recursively
func areEqual(a, b Value) bool {
	if a.Type != b.Type {
		return false
	}
	if a.Type != ConsCell && a.Value != b.Value {
		return false
	}
	if a.Type == ConsCell {
		return areEqual(*a.Car, *b.Car) && areEqual(*a.Cdr, *b.Cdr)
	}
	return true
}

// returns all the current bindings for the frame tower provided,
// i.e. all the bound symbols and the values they'll return if looked up
func getBindings(frame Frame) map[string]Value {
	bindings := make(map[string]Value)
	if frame.parent != nil {
		bindings = getBindings(*frame.parent)
	}
	for k, v := range frame.bindings {
		bindings[k] = v
	}
	return bindings
}

// converts all the current bindings into a plain text representation
func printFrame(frame Frame) string {
	bindings := getBindings(frame)
	result := ""
	for k, v := range bindings {
		result = fmt.Sprintf("%v%v => %v\n", result, k, Print(v))
	}
	return result
}

// applies a function or a macro
// argument evaluation will have already occured
func apply(fn Value, args []Value) (Value, error) {
	nilValue := Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Nil,
		Value: nil,
	}
	fnStatements := toArray(fn)
	params := fnStatements[0]

	argFrame := Frame{
		bindings: make(map[string]Value, 0),
		// bindings is a map, maps are references,
		// parent is a pointer so it'll point to the same record
		// so copying is ok here
		parent: fn.Value.(*Frame),
	}
	defer func() { fn.Value = argFrame.parent }()

	// create a new frame by binding the arguments passed in
	// to the parameters stored in the literal that we're applying
	// this is going to be the base level frame, i.e. its bindings will be checked
	// before lexical and dynamic variables.
	if params.Type == ConsCell {
		paramsList := toArray(params)
		if len(paramsList) != len(args) {
			return nilValue, fmt.Errorf("apply %v: wrong number of arguments passed", fn)
		}
		for i := range paramsList {
			argFrame.bindings[paramsList[i].Value.(string)] = args[i]
		}
	} else if params.Type == Nil {
		if len(args) != 0 {
			return nilValue, fmt.Errorf("apply %v: wrong number of arguments passed", fn)
		}
	} else {
		argFrame.bindings[params.Value.(string)] = toList(args)
	}

	// we only return the result of the last expression in the lambda / macro
	var result Value
	for _, toEvaluate := range fnStatements[1:] {
		var err error
		result, err = Eval(toEvaluate, &argFrame)
		if err != nil {
			return nilValue, err
		}
	}
	return result, nil
}

// looks up the binding definition for a symbol
func lookup(frame Frame, value Value) (Value, error) {
	nilValue := Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Nil,
		Value: nil,
	}

	if value.Type != Symbol {
		return nilValue, fmt.Errorf("lookup: cannot look up value for non-symbol '%v'", value)
	}

	v, ok := frame.bindings[value.Value.(string)]

	// if no definition found in this frame,
	// check the ones above it
	if !ok {
		if frame.parent == nil {
			return nilValue, fmt.Errorf("lookup: no binding found for symbol '%v'", value)
		}
		return lookup(*frame.parent, value)
	}

	return v, nil
}

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
			"atom?":     newSpecialForm("atom?"),
			"car":       newSpecialForm("car"),
			"cdr":       newSpecialForm("cdr"),
			"cond":      newSpecialForm("cond"),
			"cons":      newSpecialForm("cons"),
			"label":     newSpecialForm("label"),
			"lambda":    newSpecialForm("lambda"),
			"macro":     newSpecialForm("macro"),
			"eq?":       newSpecialForm("eq?"),
			"quote":     newSpecialForm("quote"),
			">":         newSpecialForm(">"),
			"+":         newSpecialForm("+"),
			"-":         newSpecialForm("-"),
			"*":         newSpecialForm("*"),
			"/":         newSpecialForm("/"),
			"%":         newSpecialForm("%"),
			"char-list": newSpecialForm("char-list"),
			"println":   newSpecialForm("println"),
			"gensym":    newSpecialForm("gensym"),
		},
	}
}

// evaluates an s-expression and returns its result
func Eval(toEvaluate Value, frame *Frame) (Value, error) {
	// define some commonly used variables / patterns in the body
	nilValue := Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Nil,
		Value: nil,
	}
	passUpError := func(err error) error {
		return fmt.Errorf("eval %v:\n %v", Print(toEvaluate), err.Error())
	}
	newError := func(errVal string) error {
		return passUpError(fmt.Errorf("env:\n%verror:%v", printFrame(*frame), errVal))
	}

	// how we evaluate is based on the type
	switch toEvaluate.Type {
	// strings, numbers, booleans, nil, macro literal and function literal are self-evaluating
	case String:
		return toEvaluate, nil
	case Number:
		return toEvaluate, nil
	case Boolean:
		return toEvaluate, nil
	// symbols evaluate to their binding. An error is generated if trying to evaluate an unbound symbol
	case Symbol:
		lookedUpVal, err := lookup(*frame, toEvaluate)
		if err != nil {
			return nilValue, passUpError(err)
		}
		return lookedUpVal, nil
	case Nil:
		return nilValue, nil
	case Function:
		return toEvaluate, nil
	case Macro:
		return toEvaluate, nil
	// cons cells represent function calls or special forms and need to be evaluated differently
	case ConsCell:
		listElements := toArray(toEvaluate)
		var firstThing Value
		var err error
		firstThing, err = Eval(listElements[0], frame)
		arguments := listElements[1:]
		if err != nil {
			return nilValue, passUpError(err)
		}

		// the first thing determines how to evaluate the rest of the list
		switch firstThing.Type {
		// if its a special form, then we need to follow specific rules depending on the form
		case SpecialForm:
			switch firstThing.Value.(string) {
			// checks if the argument is an atom (i.e., not a list)
			// evaluates arguments
			case "atom?":
				if len(arguments) != 1 {
					return nilValue, newError("wrong number of arguments passed to atom?")
				}
				var evaluatedArg Value
				evaluatedArg, err = Eval(arguments[0], frame)
				if err != nil {
					return nilValue, passUpError(err)
				}
				return Value{
					Car:  nil,
					Cdr:  nil,
					Type: Boolean,
					Value: func() bool {
						return evaluatedArg.Type != ConsCell
					}(),
				}, nil
			// get the first element of a cons cell and check for type validity
			// evaluates arguments
			case "car":
				if len(arguments) != 1 {
					return nilValue, newError("eval: wrong number of arguments passed to car")
				}
				var evaluatedArg Value
				evaluatedArg, err = Eval(arguments[0], frame)
				if err != nil {
					return nilValue, passUpError(err)
				}
				if evaluatedArg.Type != ConsCell {
					return nilValue, newError("car expected PAIR")
				}
				return *evaluatedArg.Car, nil
			// get the second element of a cons cell and check for type validty
			// evaluates arguments
			case "cdr":
				if len(arguments) != 1 {
					return nilValue, newError("wrong number of arguments passed to cdr")
				}
				var evaluatedArg Value
				evaluatedArg, err = Eval(arguments[0], frame)
				if err != nil {
					return nilValue, passUpError(err)
				}
				if evaluatedArg.Type != ConsCell {
					return nilValue, newError("cdr expected PAIR")
				}
				return *evaluatedArg.Cdr, nil
			// return a conditional output
			// has special evaluation rules, only evaluating until pred is true.
			// rest of the arguments are unevaluated.
			case "cond":
				if len(arguments) < 1 {
					return nilValue, newError("wrong number of arguments passed to cond")
				}
				for i, currentBranch := range arguments {
					branchList := toArray(currentBranch)
					if branchList[0].Type == Symbol && branchList[0].Value == "else" {
						if i+1 != len(arguments) {
							return nilValue, newError("else clause must be last in cond statement")
						}
						var result Value
						for _, toEvaluate := range branchList[1:] {
							result, err = Eval(toEvaluate, frame)
							if err != nil {
								return nilValue, passUpError(err)
							}
						}
						return result, nil
					}
					var predicate Value
					predicate, err = Eval(branchList[0], frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					if (predicate.Type == Boolean && predicate.Value.(bool)) ||
						(predicate.Type != Boolean && predicate.Type != Nil) {
						var result Value
						for _, toEvaluate := range branchList[1:] {
							result, err = Eval(toEvaluate, frame)
							if err != nil {
								return nilValue, passUpError(err)
							}
						}
						return result, nil
					}
				}
				return nilValue, nil
			// create a new cons cell, evaluates arguments
			case "cons":
				if len(arguments) != 2 {
					return nilValue, newError("wrong number of arguments passed to cons")
				}
				evaluatedArgs := make([]Value, 0)
				for _, arg := range arguments {
					var evaluatedArg Value
					evaluatedArg, err = Eval(arg, frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					evaluatedArgs = append(evaluatedArgs, evaluatedArg)
				}
				return Value{
					Car:   &evaluatedArgs[0],
					Cdr:   &evaluatedArgs[1],
					Type:  ConsCell,
					Value: nil,
				}, nil
			// check if 2 elements are structurally equal, evaluates arguments
			case "eq?":
				if len(arguments) != 2 {
					return nilValue, newError("wrong number of arguments passed to eq?")
				}
				evaluatedArgs := make([]Value, 0)
				for _, arg := range arguments {
					var evaluatedArg Value
					evaluatedArg, err = Eval(arg, frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					evaluatedArgs = append(evaluatedArgs, evaluatedArg)
				}
				return Value{
					Car:  nil,
					Cdr:  nil,
					Type: Boolean,
					Value: func() bool {
						return areEqual(evaluatedArgs[0], evaluatedArgs[1])
					}(),
				}, nil
			// creates a new binding for the base of the current frame, evaluates 2nd argument
			case "label":
				if len(arguments) != 2 {
					return nilValue, newError("wrong number of arguments passed to label")
				}
				if arguments[0].Type != Symbol {
					return nilValue, newError("label expected identifier")
				}
				var toBind Value
				toBind, err = Eval(arguments[1], frame)
				if err != nil {
					return nilValue, err
				}
				frame.bindings[arguments[0].Value.(string)] = toBind
				return nilValue, nil
			// creates a new function, does not evaluate arguments
			case "lambda":
				if len(arguments) < 2 {
					return nilValue, newError("lambda is ill-formed")
				}
				return Value{
					Car:   toEvaluate.Cdr.Car, // store arguments
					Cdr:   toEvaluate.Cdr.Cdr, // store body
					Type:  Function,
					Value: frame, // store lexical bindings
				}, nil
			// creates a new macro, does not evaluate arguments
			case "macro":
				if len(arguments) < 2 {
					return nilValue, newError("macro is ill-formed")
				}
				return Value{
					Car:   toEvaluate.Cdr.Car, // store arguments
					Cdr:   toEvaluate.Cdr.Cdr, // store body
					Type:  Macro,
					Value: frame, // store lexical bindings
				}, nil
			// returns its argument without evaluating it
			case "quote":
				if len(arguments) != 1 {
					return nilValue, newError("wrong number of arguments passed to quote")
				}
				return arguments[0], nil
			// arithmetic operators, evaluates arguments, checks types
			case "+", "-", "*", "/", "%":
				if len(arguments) < 2 {
					return nilValue, newError("wrong number of arguments passed to arithmetic operator")
				}
				evaluatedArguments := make([]Value, 0)
				for _, arg := range arguments {
					var evaluatedArg Value
					evaluatedArg, err = Eval(arg, frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					evaluatedArguments = append(evaluatedArguments, evaluatedArg)
					if evaluatedArguments[len(evaluatedArguments)-1].Type != Number {
						return nilValue, newError("cannot use non-numeric values for arithmetic")
					}
				}
				lValue := evaluatedArguments[0].Value.(float64)
				for _, rValue := range evaluatedArguments[1:] {
					rNum := rValue.Value.(float64)
					switch firstThing.Value.(string) {
					case "+":
						lValue += rNum
					case "-":
						lValue -= rNum
					case "*":
						lValue *= rNum
					case "/":
						lValue /= rNum
					case "%":
						lValue = lValue - float64(int(lValue/rNum))*rNum
					}
				}
				return Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Number,
					Value: lValue,
				}, nil
			// converts a string to a list of chars, evaluates argument
			case "char-list":
				if len(arguments) != 1 {
					return nilValue, newError("wrong number of arguments passed to char-list")
				}
				if arguments[0].Type != String {
					return nilValue, newError("cannot use non-string value for char-list")
				}
				chars := make([]Value, 0)
				for _, c := range arguments[0].Value.(string) {
					chars = append(chars, Value{
						Car:   nil,
						Cdr:   nil,
						Type:  String,
						Value: c,
					})
				}
				return toList(chars), nil
			// comparison operator, evaluates arguments, checks types, returns boolean
			case ">":
				if len(arguments) < 2 {
					return nilValue, newError("wrong number of arguments passed to >")
				}
				evaluatedArguments := make([]Value, 0)
				for _, arg := range arguments {
					var evaluatedArg Value
					evaluatedArg, err = Eval(arg, frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					evaluatedArguments = append(evaluatedArguments, evaluatedArg)
					if evaluatedArguments[len(evaluatedArguments)-1].Type != Number {
						return nilValue, newError("cannot use non-numeric values for >")
					}
				}
				allGood := true
				for i := 1; i < len(evaluatedArguments); i++ {
					if evaluatedArguments[i-1].Value.(float64) <= evaluatedArguments[i].Value.(float64) {
						allGood = false
					}
				}
				return Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Boolean,
					Value: allGood,
				}, nil
			// evaluates to a unique symbol, takes no arguments
			case "gensym":
				if len(arguments) != 0 {
					return nilValue, newError("wrong number of args passed to gensym")
				}
				result := Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Symbol,
					Value: fmt.Sprintf("G#%v", gensymCounter),
				}
				gensymCounter++
				return result, nil
			}
		// if the first argument in the list is a macro,
		// then pass the arguments un-evaluated to the macro body,
		// and then evaluate the output of the macro in the current frame
		case Macro:
			applyResult, err := apply(firstThing, arguments)
			if err != nil {
				return nilValue, passUpError(err)
			}
			return Eval(applyResult, frame)
		// if the first argument in the list is a function,
		// then pass the evaluated arguments to the function body,
		// and return the function output
		case Function:
			evaluatedArgs := make([]Value, 0)
			for _, arg := range arguments {
				var evaluatedArg Value
				evaluatedArg, err = Eval(arg, frame)
				if err != nil {
					return nilValue, passUpError(err)
				}
				evaluatedArgs = append(evaluatedArgs, evaluatedArg)
			}
			result, err := apply(firstThing, evaluatedArgs)
			return result, err
		default:
			return nilValue, newError("first argument in call must be a function, macro or special form")
		}
	default:
		return nilValue, newError("unknown value type")
	}

	panic("fell through default case on Eval function")
}

// reads a list in and creates a new value for it
// read list will call read repeatedly as it traverses
// its contents, until it hits a closing parens.
func readList(s string) (string, Value, error) {
	thisList := make([]Value, 0)
	remainingString := s
	appendAtom := false
	thingsToRightOfDot := -1
	for remainingString != "" {
		var nextValue Value
		var err error
		remainingString, nextValue, err = Read(remainingString)
		if err != nil {
			// this error occurs when we get to the end of the list
			if err.Error() == "unexpected ')'" {
				asList := toList(thisList)
				listTraverser := &asList
				// if append atom is true, go to the last cons cell,
				// the one whose cdr is null. Replace this with the
				// car of that cons cell.
				if appendAtom && thingsToRightOfDot != 1 {
					return remainingString, Value{
						Car:   nil,
						Cdr:   nil,
						Type:  Nil,
						Value: nil,
					}, errors.New("improperly placed . ")
				}
				if appendAtom {
					for listTraverser.Cdr.Type == ConsCell {
						listTraverser = listTraverser.Cdr
					}
					listTraverser.Value = listTraverser.Car.Value
					listTraverser.Type = listTraverser.Car.Type
					listTraverser.Car = nil
					listTraverser.Cdr = nil
				}
				return remainingString, asList, nil
				// when we encounter a .
				// we only want to see one ., and it needs to be
				// right before the last element. If it isn't,
				// we're going to generate an improperly placed . error
			} else if err.Error() == "unexpected '.'" {
				if appendAtom {
					return remainingString, Value{
						Car:   nil,
						Cdr:   nil,
						Type:  Nil,
						Value: nil,
					}, errors.New("improperly placed . ")
				}
				appendAtom = true
				// else this isn't an error we can catch, so propogate it up
			} else {
				return remainingString, Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Nil,
					Value: nil,
				}, err
			}
		}
		if appendAtom {
			thingsToRightOfDot++
		}
		if thingsToRightOfDot > 1 {
			return remainingString, Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Nil,
				Value: nil,
			}, errors.New("improperly placed . ")
		}
		thisList = append(thisList, nextValue)
	}
	// if we didnt return early and hit the end of the string,
	// then there was no matching closing parens and we need to generate an error
	return "", Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Nil,
		Value: nil,
	}, errors.New("could not find matching closing parens")
}

// read a string in and create a new atom for it
func readString(s string) (string, Value, error) {
	escape := false
	result := ""
	for i := range s {
		if escape {
			switch s[i] {
			case '"':
				result += "\""
			case 'n':
				result += "\n"
			case 't':
				result += "\t"
			case 'r':
				result += "\r"
			case '\\':
				result += "\\"
			}
			escape = false
		} else {
			if s[i] == '\\' {
				escape = true
			} else if s[i] == '"' {
				return s[i+1:], Value{
					Car:   nil,
					Cdr:   nil,
					Type:  String,
					Value: result,
				}, nil
			} else {
				result += string(s[i])
			}
		}
	}
	return "", Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Nil,
		Value: nil,
	}, errors.New("could not find matching closing double quote")
}

// read a number in and create a new symbol for it
func readNumber(s string) (string, Value, error) {
	var result float64
	fmt.Sscanf(s, "%f", &result)
	i := 0
	for i < len(s) && '0' <= s[i] && s[i] <= '9' {
		i++
	}
	return s[i:], Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Number,
		Value: result,
	}, nil
}

// reads a symbol in and creates a new atom for it
func readSymbol(s string) (string, Value, error) {
	result := ""
	i := 0
	for ; i < len(s) && s[i] != ' ' && s[i] != '\t' && s[i] != '\n' && s[i] != '(' && s[i] != ')'; i++ {
		result += string(s[i])
	}
	return s[i:], Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Symbol,
		Value: result,
	}, nil
}

// reads a string and converts it into a value
// lists are represented as linked lists of cons cells
// that descend on the cdr side, with each value held
// in the car of that specific cons cell.
// the string returned is the remainder of it after the read was done
// sometimes read may need to be applied multiple times to fully ingest a string,
// since read only consumes the first s-expression that it encounters.
func Read(s string) (string, Value, error) {
	// lists start with open paren
	if s[0] == '(' {
		return readList(s[1:])
		// strings start with quotes
	} else if s[0] == '"' {
		return readString(s[1:])
		// numbers start with a digit
	} else if '0' <= s[0] && s[0] <= '9' {
		return readNumber(s)
		// if its white space, skip forward and call read again when we get to something
		// thats not whitespace
	} else if s[0] == ' ' || s[0] == '\t' || s[0] == '\n' {
		skipForward := 1
		for skipForward < len(s) &&
			(s[skipForward] == ' ' || s[skipForward] == '\t' || s[skipForward] == '\n') {
			skipForward++
		}
		if skipForward == len(s) {
			return "", Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Nil,
				Value: nil,
			}, nil
		}
		return Read(s[skipForward:])
	} else if s[0] == '\'' {
		remaining, beingQuoted, err := Read(s[1:])
		return remaining, Value{
			Car: &Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Symbol,
				Value: "quote",
			},
			Cdr: &Value{
				Car: &beingQuoted,
				Cdr: &Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Nil,
					Value: nil,
				},
				Type:  ConsCell,
				Value: nil,
			},
			Type:  ConsCell,
			Value: nil,
		}, err
		// skip over comments
	} else if s[0] == ';' {
		skipForward := 1
		for skipForward < len(s) && s[skipForward] != '\n' {
			skipForward++
		}
		if skipForward == len(s) {
			return "", Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Nil,
				Value: nil,
			}, nil
		}
		return Read(s[skipForward:])
		// true and false have special representations
	} else if len(s) >= 2 && s[0:2] == "#f" {
		return s[2:], Value{
			Car:   nil,
			Cdr:   nil,
			Type:  Boolean,
			Value: false,
		}, nil
	} else if len(s) >= 2 && s[0:2] == "#t" {
		return s[2:], Value{
			Car:   nil,
			Cdr:   nil,
			Type:  Boolean,
			Value: true,
		}, nil
		// . is the marker for a cons pair, which means the thing to the right
		// is the cdr of the cons cell, not nil like it would be for a standard list
	} else if s[0] == '.' {
		return s[1:], Value{
			Car:   nil,
			Cdr:   nil,
			Type:  Nil,
			Value: nil,
		}, errors.New("unexpected '.'")
	} else if 1 == 0 {
		// reader macros

		// closing paren marks the end of a list, this is an error.
		// if its terminating a list, then read list will catch it
		// and the error wont bubble to the user.
	} else if s[0] == ')' {
		return s[1:], Value{
			Car:   nil,
			Cdr:   nil,
			Type:  Nil,
			Value: nil,
		}, errors.New("unexpected ')'")
	}
	// of none of these matched, then its a symbol, so read it in as that.
	return readSymbol(s)
}

// print out each element in the lis
// with special logic for cons pairs
func printList(v Value) string {
	result := Print(*v.Car)
	for v = *v.Cdr; v.Type == ConsCell; v = *v.Cdr {
		result += " "
		result += Print(*v.Car)
	}
	if v.Type == Nil {
		return result
	} else {
		return result + ". " + Print(v)
	}
}

// escapes special characters in string
func stringify(s string) string {
	s = strings.ReplaceAll(s, "\\", "\\\\")
	s = strings.ReplaceAll(s, "\"", "\\\"")
	s = strings.ReplaceAll(s, "\n", "\\n")
	s = strings.ReplaceAll(s, "\t", "\\t")
	s = strings.ReplaceAll(s, "\r", "\\r")
	return s
}

// converts a value to a string recursively,
// representing lists by wrapping them with parenthesis
func Print(v Value) string {
	switch v.Type {
	case Nil:
		return "()"
	case Boolean:
		return fmt.Sprintf("%v", v.Value.(bool))
	case Number:
		return fmt.Sprintf("%v", v.Value.(float64))
	case String:
		return "\"" + stringify(v.Value.(string)) + "\""
	case ConsCell:
		return "(" + printList(v) + ")"
	case Symbol:
		return v.Value.(string)
	default:
		return "Cannot print fn/macro/special-form"
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
