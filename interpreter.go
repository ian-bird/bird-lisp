package main

import (
	"fmt"
)

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
	aTodo := []Value{a}
	bTodo := []Value{b}

	for len(aTodo) > 0 {
		a := aTodo[0]
		b := bTodo[0]
		if a.Type != b.Type {
			return false
		}
		if a.Type != ConsCell && a.Value != b.Value {
			return false
		}
		if a.Type == ConsCell {
			aTodo = append(aTodo, *a.Car, *a.Cdr)
			bTodo = append(bTodo, *b.Car, *b.Cdr)
		}
		aTodo = aTodo[1:]
		bTodo = bTodo[1:]
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

	// doing this in a loop so macros can do tail calls
	for {

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
		case InterpretedFunction:
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
						Type:  InterpretedFunction,
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
				case "set!":
					if len(arguments) != 2 {
						return nilValue, newError("wrong number of arguments passed to set!")
					}
					// evaluate 2nd arg
					newValue, err := Eval(arguments[1], frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					// evaluate first arg and get string value of resulting
					// symbol
					toBindSymbol, err := Eval(arguments[0], frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					if toBindSymbol.Type != Symbol {
						return nilValue, newError("identifier expected")
					}
					toBind := toBindSymbol.Value.(string)
					for currentFrame := frame; currentFrame != nil; currentFrame = currentFrame.parent {
						_, inFrame := currentFrame.bindings[toBind]
						if inFrame {
							currentFrame.bindings[toBind] = newValue
							return nilValue, nil
						}
					}
					return nilValue, fmt.Errorf("lookup: no binding found for symbol '%v'", toBind)
				case "type":
					if len(arguments) != 1 {
						return nilValue, newError("wrong number of arguments passed to type")
					}
					evaledArg, err := Eval(arguments[0], frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					switch evaledArg.Type {
					case Nil:
						return Value{
							Car:   nil,
							Cdr:   nil,
							Type:  Symbol,
							Value: "nil",
						}, nil
					case String:
						return Value{
							Car:   nil,
							Cdr:   nil,
							Type:  Symbol,
							Value: "string",
						}, nil
					case Number:
						return Value{
							Car:   nil,
							Cdr:   nil,
							Type:  Symbol,
							Value: "number",
						}, nil
					case Symbol:
						return Value{
							Car:   nil,
							Cdr:   nil,
							Type:  Symbol,
							Value: "symbol",
						}, nil
					case Boolean:
						return Value{
							Car:   nil,
							Cdr:   nil,
							Type:  Symbol,
							Value: "boolean",
						}, nil
					case InterpretedFunction:
						return Value{
							Car:   nil,
							Cdr:   nil,
							Type:  Symbol,
							Value: "interpretedfunction",
						}, nil
					case CompiledFunction:
						return Value{
							Car:   nil,
							Cdr:   nil,
							Type:  Symbol,
							Value: "compiledfunction",
						}, nil
					case Macro:
						return Value{
							Car:   nil,
							Cdr:   nil,
							Type:  Symbol,
							Value: "macro",
						}, nil
					case SpecialForm:
						return Value{
							Car:   nil,
							Cdr:   nil,
							Type:  Symbol,
							Value: "specialform",
						}, nil
					case ConsCell:
						return Value{
							Car:   nil,
							Cdr:   nil,
							Type:  Symbol,
							Value: "conscell",
						}, nil
					default:
						return nilValue, nil
					}

				case "macroexpand-1":
					if len(arguments) != 1 {
						return nilValue, newError("wrong number of arguments passed to macroexpand-1")
					}
					var evaluatedArg Value
					evaluatedArg, err = Eval(arguments[0], frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					if evaluatedArg.Type != ConsCell {
						return nilValue, newError("argument to macroexpand-1 must be macro call")
					}
					macro, err := Eval(*evaluatedArg.Car, frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					if macro.Type != Macro {
						return nilValue, newError("argument to macroexpand-1 must be macro call")
					}
					macroArgs := toArray(*evaluatedArg.Cdr)
					result, err := apply(macro, macroArgs)
					if err != nil {
						return nilValue, passUpError(err)
					}
					return result, nil
				case "bound?":
					if len(arguments) != 1 {
						return nilValue, newError("wrong number of arguments passed to bound?")
					}
					var evaluatedArg Value
					evaluatedArg, err = Eval(arguments[0], frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					if evaluatedArg.Type != Symbol {
						return nilValue, newError("argument to bound? must be symbol")
					}
					_, found := lookup(*frame, evaluatedArg)
					if found != nil {
						return Value{
							Car:   nil,
							Cdr:   nil,
							Type:  Boolean,
							Value: false,
						}, nil
					}
					return Value{
						Car:   nil,
						Cdr:   nil,
						Type:  Boolean,
						Value: true,
					}, nil

				case "assemble":
					result, err := assemble(arguments)
					if err != nil {
						return nilValue, passUpError(err)
					}
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
				// update the thing to evaluate and loop again (tail call optimized)
				toEvaluate = applyResult
			// if the first argument in the list is a function,
			// then pass the evaluated arguments to the function body,
			// and return the function output
			case InterpretedFunction:
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
				if err != nil {
					return nilValue, passUpError(err)
				}
				return result, nil
			case CompiledFunction:

			default:
				return nilValue, newError("first argument in call must be a function, macro or special form")
			}
		default:
			return nilValue, newError("unknown value type")
		}
	}
}
