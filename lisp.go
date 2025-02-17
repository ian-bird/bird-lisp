package main

import (
	"errors"
	"fmt"
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

func toArray(value Value) []Value {
	result := make([]Value, 0)
	for value.Type == ConsCell {
		result = append(result, *value.Car)
	}
	return result
}

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

// applies a function or a macro
func apply(fn Value, args []Value, frame Frame) (Value, Frame, error) {
	nilValue := Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Nil,
		Value: nil,
	}
	fnStatements := toArray(fn)
	params := fnStatements[0]
	newFrame := Frame{
		bindings: make(map[string]Value, 0),
		parent:   &frame,
	}
	if params.Type == ConsCell {
		paramsList := toArray(params)
		if len(paramsList) != len(args) {
			return nilValue, frame, errors.New("wrong number of arguments passed")
		}
		for i, _ := range paramsList {
			newFrame.bindings[paramsList[i].Value.(string)] = args[i]
		}
	} else {
		newFrame.bindings[params.Value.(string)] = toList(args)
	}
	var result Value
	for _, toEvaluate := range fnStatements[1:] {
		var err error
		result, _, err = Eval(toEvaluate, newFrame)
		if err != nil {
			return nilValue, frame, err
		}
	}
	return result, frame, nil
}

// looks up the binding definition for a symbol
func lookup(frame Frame, value Value) Value {
	nilValue := Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Nil,
		Value: nil,
	}

	if value.Type != Symbol {
		return nilValue
	}

	v, ok := frame.bindings[value.Value.(string)]

	// if no definition found in this frame,
	// check the ones above it
	if !ok {
		if frame.parent == nil {
			return nilValue
		}
		return lookup(*frame.parent, value)
	}

	return v
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
			"atom?":  newSpecialForm("atom?"),
			"car":    newSpecialForm("car"),
			"cdr":    newSpecialForm("cdr"),
			"cond":   newSpecialForm("cond"),
			"cons":   newSpecialForm("cons"),
			"label":  newSpecialForm("label"),
			"lambda": newSpecialForm("lambda"),
			"macro":  newSpecialForm("macro"),
			"eq?":    newSpecialForm("eq?"),
			"quote":  newSpecialForm("quote"),
		},
	}
}

func Eval(toEvaluate Value, frame Frame) (Value, Frame, error) {
	nilValue := Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Nil,
		Value: nil,
	}
	switch toEvaluate.Type {
	case String:
		return toEvaluate, frame, nil
	case Number:
		return toEvaluate, frame, nil
	case Boolean:
		return toEvaluate, frame, nil
	case Symbol:
		return lookup(frame, toEvaluate), frame, nil
	case Nil:
		return nilValue, frame, nil
	case ConsCell:
		listElements := toArray(toEvaluate)
		firstThing, _, err := Eval(listElements[0], frame)
		arguments := listElements[1:]
		if err != nil {
			return nilValue, frame, err
		}
		switch firstThing.Type {
		case SpecialForm:
			switch firstThing.Value.(string) {
			case "atom?":
				if len(arguments) != 1 {
					return nilValue, frame, errors.New("wrong number of arguments passed to atom?")
				}
				evaluatedArg, _, err := Eval(arguments[0], frame)
				if err != nil {
					return nilValue, frame, err
				}
				return Value{
					Car:  nil,
					Cdr:  nil,
					Type: Boolean,
					Value: func() bool {
						return evaluatedArg.Type != ConsCell
					}(),
				}, frame, nil
			case "car":
				if len(arguments) != 1 {
					return nilValue, frame, errors.New("wrong number of arguments passed to car")
				}
				evaluatedArg, _, err := Eval(arguments[0], frame)
				if err != nil {
					return nilValue, frame, err
				}
				if evaluatedArg.Type != ConsCell {
					return nilValue, frame, errors.New("car expected PAIR")
				}
				return *evaluatedArg.Car, frame, nil
			case "cdr":
				if len(arguments) != 1 {
					return nilValue, frame, errors.New("wrong number of arguments passed to cdr")
				}
				evaluatedArg, _, err := Eval(arguments[0], frame)
				if err != nil {
					return nilValue, frame, err
				}
				if evaluatedArg.Type != ConsCell {
					return nilValue, frame, errors.New("cdr expected PAIR")
				}
				return *evaluatedArg.Cdr, frame, nil
			case "cond":
				if len(arguments) < 1 {
					return nilValue, frame, errors.New("wrong number of arguments passed to cond")
				}
				for i, currentBranch := range arguments {
					branchList := toArray(currentBranch)
					if branchList[0].Type == Symbol && branchList[0].Value == "else" {
						if i+1 != len(arguments) {
							return nilValue, frame, errors.New("else clause must be last in cond statement")
						}
						var result Value
						for _, toEvaluate := range branchList[1:] {
							result, _, err = Eval(toEvaluate, frame)
							if err != nil {
								return nilValue, frame, err
							}
						}
						return result, frame, nil
					}
					predicate, _, err := Eval(branchList[0], frame)
					if err != nil {
						return nilValue, frame, err
					}
					if predicate.Type == Boolean && !predicate.Value.(bool) {
						var result Value
						for _, toEvaluate := range branchList[1:] {
							result, _, err = Eval(toEvaluate, frame)
							if err != nil {
								return nilValue, frame, err
							}
						}
						return result, frame, nil
					}
				}
				return nilValue, frame, nil
			case "cons":
				if len(arguments) != 2 {
					return nilValue, frame, errors.New("wrong number of arguments passed to cons")
				}
				evaluatedArgs := make([]Value, 0)
				for _, arg := range arguments {
					evaluatedArg, _, err := Eval(arg, frame)
					if err != nil {
						return nilValue, frame, err
					}
					evaluatedArgs = append(evaluatedArgs, evaluatedArg)
				}
				return Value{
					Car:   &evaluatedArgs[0],
					Cdr:   &evaluatedArgs[1],
					Type:  ConsCell,
					Value: nil,
				}, frame, nil
			case "eq?":
				if len(arguments) != 2 {
					return nilValue, frame, errors.New("wrong number of arguments passed to eq?")
				}
				evaluatedArgs := make([]Value, 0)
				for _, arg := range arguments {
					evaluatedArg, _, err := Eval(arg, frame)
					if err != nil {
						return nilValue, frame, err
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
				}, frame, nil
			case "label":
				if len(arguments) != 2 {
					return nilValue, frame, errors.New("wrong number of arguments passed to label")
				}
				if arguments[0].Type != Symbol {
					return nilValue, frame, errors.New("label expected identifier")
				}
				toBind, _, err := Eval(arguments[1], frame)
				if err != nil {
					return nilValue, frame, err
				}
				nextFrame := Frame{
					parent:   frame.parent,
					bindings: make(map[string]Value, 0),
				}
				for k, v := range frame.bindings {
					nextFrame.bindings[k] = v
				}
				nextFrame.bindings[arguments[0].Value.(string)] = toBind
				return nilValue, nextFrame, nil
			case "lambda":
				if len(arguments) < 2 {
					return nilValue, frame, errors.New("lambda is ill-formed")
				}
				return Value{
					Car:   toEvaluate.Cdr.Car,
					Cdr:   toEvaluate.Cdr.Cdr,
					Type:  Function,
					Value: nil,
				}, frame, nil
			case "macro":
				if len(arguments) < 2 {
					return nilValue, frame, errors.New("lambda is ill-formed")
				}
				return Value{
					Car:   toEvaluate.Cdr.Car,
					Cdr:   toEvaluate.Cdr.Cdr,
					Type:  Macro,
					Value: nil,
				}, frame, nil
			case "quote":
				if len(arguments) != 1 {
					return nilValue, frame, errors.New("wrong number of arguments passed to quote")
				}
				return arguments[0], frame, nil
			}
		case Macro:
			applyResult, _, err := apply(firstThing, arguments, frame)
			if err != nil {
				return nilValue, frame, err
			}
			return Eval(applyResult, frame)
		case Function:
			evaluatedArgs := make([]Value, 0)
			for _, arg := range arguments {
				evaluatedArg, _, err := Eval(arg, frame)
				if err != nil {
					return nilValue, frame, err
				}
				evaluatedArgs = append(evaluatedArgs, evaluatedArg)
			}
			return apply(firstThing, evaluatedArgs, frame)
		default:
			return nilValue, frame, errors.New("First argument in call must be a function, macro or special form")
		}
	default:
		return nilValue, frame, errors.New("unknown value type")
	}

	panic("fell through default case on Eval function")
}

func readList(s string) (string, Value, error) {
	thisList := make([]Value, 0)
	remainingString := s
	appendAtom := false
	for remainingString != "" {
		var nextValue Value
		var err error
		remainingString, nextValue, err = Read(remainingString)
		if err != nil {
			if err.Error() == "unexpected ')'" {
				asList := toList(thisList)
				listTraverser := &asList
				// if append atom is true, go to the last cons cell,
				// the one whose cdr is null. Replace this with the
				// car of that cons cell.
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
			} else if err.Error() == "unexpected '.'" {
				if appendAtom {
					return remainingString, Value{
						Car:   nil,
						Cdr:   nil,
						Type:  Nil,
						Value: nil,
					}, errors.New("improperly placed .")
				}
				appendAtom = true
			}
			return remainingString, Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Nil,
				Value: nil,
			}, err
		}
		thisList = append(thisList, nextValue)
	}
	return "", Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Nil,
		Value: nil,
	}, errors.New("could not find matching closing parens")
}

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

func readNumber(s string) (string, Value, error) {
	var result float64
	fmt.Sscanf(s, "%f", &result)
	i := 0
	for '0' <= s[i] && s[i] <= '9' {
		i++
	}
	return s[i:], Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Number,
		Value: result,
	}, nil
}

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

func Read(s string) (string, Value, error) {
	if s[0] == '(' {
		return readList(s[1:])
	} else if s[0] == '"' {
		return readString(s[1:])
	} else if '0' <= s[0] && s[0] <= '9' {
		return readNumber(s)
	} else if s[0] == ' ' || s[0] == '\t' || s[0] == '\n' {
		skipForward := 1
		for skipForward < len(s) && (s[skipForward] == ' ' || s[skipForward] == '\t') {
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
	} else if s[0:1] == "#f" {
		return s[2:], Value{
			Car:   nil,
			Cdr:   nil,
			Type:  Boolean,
			Value: false,
		}, nil
	} else if s[0:1] == "#t" {
		return s[2:], Value{
			Car:   nil,
			Cdr:   nil,
			Type:  Boolean,
			Value: true,
		}, nil
	} else if 1 == 0 {
		// reader macros
	} else if s[0] == ')' {
		return s[1:], Value{
			Car:   nil,
			Cdr:   nil,
			Type:  Nil,
			Value: nil,
		}, errors.New("unexpected ')'")
	}
	return readSymbol(s)
}

func printList(v Value) string {
	result := Print(*v.Car)
	for v = *v.Cdr; v.Type == ConsCell; v = *v.Cdr {
		result += " "
		result += Print(*v.Car)
	}
	if v.Type == Nil {
		return result
	} else {
		return result + " . " + Print(v)
	}
}

func stringify(s string) string {
	s = strings.ReplaceAll(s, "\\", "\\\\")
	s = strings.ReplaceAll(s, "\"", "\\\"")
	s = strings.ReplaceAll(s, "\n", "\\n")
	s = strings.ReplaceAll(s, "\t", "\\t")
	s = strings.ReplaceAll(s, "\r", "\\r")
	return s
}

func Print(v Value) string {
	switch v.Type {
	case Nil:
		return ""
	case Boolean:
		return v.Value.(string)
	case Number:
		return fmt.Sprintf("%v", v.Value.(float64))
	case String:
		return "\"" + stringify(v.Value.(string)) + "\""
	case ConsCell:
		return "(" + printList(v) + ")"
	case Symbol:
		return v.Value.(string)
	}

	panic("fell through print statement. Perhaps trying to print function literal?")
}

func main() {
	remaining, val, err := Read("((lambda (n) (cond ((eq? n 1) \"test string with \\nescape \\\"sequences\\\"\") (else (* n (fac (- 1 n)))))) 5)")
	frame := NewTopLevelFrame()
	result, _, evalErr := Eval(val, frame)
	fmt.Printf("%v %v %v %v", remaining, result, err, evalErr)
}
