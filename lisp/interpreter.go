package lisp

import (
	"fmt"
	lisptype "test/m/lisp_type"
)

// this is a global counter used to prevent gensym collisions
var gensymCounter = 0

// converta a list of values (car points to element, cdr to tail)
// into an array of elements
func toArray(value lisptype.Value) []lisptype.Value {
	result := make([]lisptype.Value, 0)
	for value.Cdr != nil {
		result = append(result, *value.Car)
		value = *value.Cdr
	}
	return result
}

// converts an array of values into a list (car points to element, cdr to tail)
func toList(values []lisptype.Value) lisptype.Value {
	if len(values) == 0 {
		return lisptype.Value{
			Car:   nil,
			Cdr:   nil,
			Type:  lisptype.Nil,
			Value: nil,
		}
	} else {
		return lisptype.Value{
			Car:   &values[0],
			Cdr:   &[]lisptype.Value{toList(values[1:])}[0],
			Type:  lisptype.ConsCell,
			Value: nil,
		}
	}
}

// returns true if 2 values are equal, checks recursively
func areEqual(a, b lisptype.Value) bool {
	aTodo := []lisptype.Value{a}
	bTodo := []lisptype.Value{b}

	for len(aTodo) > 0 {
		a := aTodo[0]
		b := bTodo[0]
		if a.Type != b.Type {
			return false
		}
		if a.Type != lisptype.ConsCell && a.Value != b.Value {
			return false
		}
		if a.Type == lisptype.ConsCell {
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
func getBindings(frame lisptype.Frame) map[string]lisptype.Value {
	bindings := make(map[string]lisptype.Value)
	if frame.Parent != nil {
		bindings = getBindings(*frame.Parent)
	}
	for k, v := range frame.Bindings {
		bindings[k] = v
	}
	return bindings
}

// converts all the current bindings into a plain text representation
func printFrame(frame lisptype.Frame) string {
	bindings := getBindings(frame)
	result := ""
	for k, v := range bindings {
		result = fmt.Sprintf("%v%v => %v\n", result, k, Print(v))
	}
	return result
}

// applies a function or a macro
// argument evaluation will have already occured
func apply(fn lisptype.Value, args []lisptype.Value) (lisptype.Value, error) {
	nilValue := lisptype.Value{
		Car:   nil,
		Cdr:   nil,
		Type:  lisptype.Nil,
		Value: nil,
	}
	fnStatements := toArray(fn)
	params := fnStatements[0]

	argFrame := lisptype.Frame{
		Bindings: make(map[string]lisptype.Value, 0),
		// bindings is a map, maps are references,
		// parent is a pointer so it'll point to the same record
		// so copying is ok here
		Parent: fn.Value.(*lisptype.Frame),
	}
	defer func() { fn.Value = argFrame.Parent }()

	// create a new frame by binding the arguments passed in
	// to the parameters stored in the literal that we're applying
	// this is going to be the base level frame, i.e. its bindings will be checked
	// before lexical and dynamic variables.
	if params.Type == lisptype.ConsCell {
		paramsList := toArray(params)
		if len(paramsList) != len(args) {
			return nilValue, fmt.Errorf("apply %v: wrong number of arguments passed", fn)
		}
		for i := range paramsList {
			argFrame.Bindings[paramsList[i].Value.(string)] = args[i]
		}
	} else if params.Type == lisptype.Nil {
		if len(args) != 0 {
			return nilValue, fmt.Errorf("apply %v: wrong number of arguments passed", fn)
		}
	} else {
		argFrame.Bindings[params.Value.(string)] = toList(args)
	}

	// we only return the result of the last expression in the lambda / macro
	var result lisptype.Value
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
func lookup(frame lisptype.Frame, value lisptype.Value) (lisptype.Value, error) {
	nilValue := lisptype.Value{
		Car:   nil,
		Cdr:   nil,
		Type:  lisptype.Nil,
		Value: nil,
	}

	if value.Type != lisptype.Symbol {
		return nilValue, fmt.Errorf("lookup: cannot look up value for non-symbol '%v'", value)
	}

	v, ok := frame.Bindings[value.Value.(string)]

	// if no definition found in this frame,
	// check the ones above it
	if !ok {
		if frame.Parent == nil {
			return nilValue, fmt.Errorf("lookup: no binding found for symbol '%v'", value)
		}
		return lookup(*frame.Parent, value)
	}

	return v, nil
}

// evaluates an s-expression and returns its result
func Eval(toEvaluate lisptype.Value, frame *lisptype.Frame) (lisptype.Value, error) {
	// define some commonly used variables / patterns in the body
	nilValue := lisptype.Value{
		Car:   nil,
		Cdr:   nil,
		Type:  lisptype.Nil,
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
		case lisptype.String:
			return toEvaluate, nil
		case lisptype.Number:
			return toEvaluate, nil
		case lisptype.Boolean:
			return toEvaluate, nil
		// symbols evaluate to their binding. An error is generated if trying to evaluate an unbound symbol
		case lisptype.Symbol:
			lookedUpVal, err := lookup(*frame, toEvaluate)
			if err != nil {
				return nilValue, passUpError(err)
			}
			return lookedUpVal, nil
		case lisptype.Nil:
			return nilValue, nil
		case lisptype.InterpretedFunction:
			return toEvaluate, nil
		case lisptype.Macro:
			return toEvaluate, nil
		// cons cells represent function calls or special forms and need to be evaluated differently
		case lisptype.ConsCell:
			listElements := toArray(toEvaluate)
			var firstThing lisptype.Value
			var err error
			firstThing, err = Eval(listElements[0], frame)
			arguments := listElements[1:]
			if err != nil {
				return nilValue, passUpError(err)
			}

			// the first thing determines how to evaluate the rest of the list
			switch firstThing.Type {
			// if its a special form, then we need to follow specific rules depending on the form
			case lisptype.SpecialForm:
				switch firstThing.Value.(string) {
				// checks if the argument is an atom (i.e., not a list)
				// evaluates arguments
				case "atom?":
					if len(arguments) != 1 {
						return nilValue, newError("wrong number of arguments passed to atom?")
					}
					var evaluatedArg lisptype.Value
					evaluatedArg, err = Eval(arguments[0], frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					return lisptype.Value{
						Car:  nil,
						Cdr:  nil,
						Type: lisptype.Boolean,
						Value: func() bool {
							return evaluatedArg.Type != lisptype.ConsCell
						}(),
					}, nil
				// get the first element of a cons cell and check for type validity
				// evaluates arguments
				case "car":
					if len(arguments) != 1 {
						return nilValue, newError("eval: wrong number of arguments passed to car")
					}
					var evaluatedArg lisptype.Value
					evaluatedArg, err = Eval(arguments[0], frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					if evaluatedArg.Type != lisptype.ConsCell {
						return nilValue, newError("car expected PAIR")
					}
					return *evaluatedArg.Car, nil
				// get the second element of a cons cell and check for type validty
				// evaluates arguments
				case "cdr":
					if len(arguments) != 1 {
						return nilValue, newError("wrong number of arguments passed to cdr")
					}
					var evaluatedArg lisptype.Value
					evaluatedArg, err = Eval(arguments[0], frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					if evaluatedArg.Type != lisptype.ConsCell {
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
						if branchList[0].Type == lisptype.Symbol && branchList[0].Value == "else" {
							if i+1 != len(arguments) {
								return nilValue, newError("else clause must be last in cond statement")
							}
							var result lisptype.Value
							for _, toEvaluate := range branchList[1:] {
								result, err = Eval(toEvaluate, frame)
								if err != nil {
									return nilValue, passUpError(err)
								}
							}
							return result, nil
						}
						var predicate lisptype.Value
						predicate, err = Eval(branchList[0], frame)
						if err != nil {
							return nilValue, passUpError(err)
						}
						if (predicate.Type == lisptype.Boolean && predicate.Value.(bool)) ||
							(predicate.Type != lisptype.Boolean && predicate.Type != lisptype.Nil) {
							var result lisptype.Value
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
					evaluatedArgs := make([]lisptype.Value, 0)
					for _, arg := range arguments {
						var evaluatedArg lisptype.Value
						evaluatedArg, err = Eval(arg, frame)
						if err != nil {
							return nilValue, passUpError(err)
						}
						evaluatedArgs = append(evaluatedArgs, evaluatedArg)
					}
					return lisptype.Value{
						Car:   &evaluatedArgs[0],
						Cdr:   &evaluatedArgs[1],
						Type:  lisptype.ConsCell,
						Value: nil,
					}, nil
				// check if 2 elements are structurally equal, evaluates arguments
				case "eq?":
					if len(arguments) != 2 {
						return nilValue, newError("wrong number of arguments passed to eq?")
					}
					evaluatedArgs := make([]lisptype.Value, 0)
					for _, arg := range arguments {
						var evaluatedArg lisptype.Value
						evaluatedArg, err = Eval(arg, frame)
						if err != nil {
							return nilValue, passUpError(err)
						}
						evaluatedArgs = append(evaluatedArgs, evaluatedArg)
					}
					return lisptype.Value{
						Car:  nil,
						Cdr:  nil,
						Type: lisptype.Boolean,
						Value: func() bool {
							return areEqual(evaluatedArgs[0], evaluatedArgs[1])
						}(),
					}, nil
				// creates a new binding for the base of the current frame, evaluates 2nd argument
				case "label":
					if len(arguments) != 2 {
						return nilValue, newError("wrong number of arguments passed to label")
					}
					if arguments[0].Type != lisptype.Symbol {
						return nilValue, newError("label expected identifier")
					}
					var toBind lisptype.Value
					toBind, err = Eval(arguments[1], frame)
					if err != nil {
						return nilValue, err
					}
					frame.Bindings[arguments[0].Value.(string)] = toBind
					return nilValue, nil
				// creates a new function, does not evaluate arguments
				case "lambda":
					if len(arguments) < 2 {
						return nilValue, newError("lambda is ill-formed")
					}
					return lisptype.Value{
						Car:   toEvaluate.Cdr.Car, // store arguments
						Cdr:   toEvaluate.Cdr.Cdr, // store body
						Type:  lisptype.InterpretedFunction,
						Value: frame, // store lexical bindings
					}, nil
				// creates a new macro, does not evaluate arguments
				case "macro":
					if len(arguments) < 2 {
						return nilValue, newError("macro is ill-formed")
					}
					return lisptype.Value{
						Car:   toEvaluate.Cdr.Car, // store arguments
						Cdr:   toEvaluate.Cdr.Cdr, // store body
						Type:  lisptype.Macro,
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
					evaluatedArguments := make([]lisptype.Value, 0)
					for _, arg := range arguments {
						var evaluatedArg lisptype.Value
						evaluatedArg, err = Eval(arg, frame)
						if err != nil {
							return nilValue, passUpError(err)
						}
						evaluatedArguments = append(evaluatedArguments, evaluatedArg)
						if evaluatedArguments[len(evaluatedArguments)-1].Type != lisptype.Number {
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
					return lisptype.Value{
						Car:   nil,
						Cdr:   nil,
						Type:  lisptype.Number,
						Value: lValue,
					}, nil
				// comparison operator, evaluates arguments, checks types, returns boolean
				case ">":
					if len(arguments) < 2 {
						return nilValue, newError("wrong number of arguments passed to >")
					}
					evaluatedArguments := make([]lisptype.Value, 0)
					for _, arg := range arguments {
						var evaluatedArg lisptype.Value
						evaluatedArg, err = Eval(arg, frame)
						if err != nil {
							return nilValue, passUpError(err)
						}
						evaluatedArguments = append(evaluatedArguments, evaluatedArg)
						if evaluatedArguments[len(evaluatedArguments)-1].Type != lisptype.Number {
							return nilValue, newError("cannot use non-numeric values for >")
						}
					}
					allGood := true
					for i := 1; i < len(evaluatedArguments); i++ {
						if evaluatedArguments[i-1].Value.(float64) <= evaluatedArguments[i].Value.(float64) {
							allGood = false
						}
					}
					return lisptype.Value{
						Car:   nil,
						Cdr:   nil,
						Type:  lisptype.Boolean,
						Value: allGood,
					}, nil
				// evaluates to a unique symbol, takes no arguments
				case "gensym":
					if len(arguments) != 0 {
						return nilValue, newError("wrong number of args passed to gensym")
					}
					result := lisptype.Value{
						Car:   nil,
						Cdr:   nil,
						Type:  lisptype.Symbol,
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
					if toBindSymbol.Type != lisptype.Symbol {
						return nilValue, newError("identifier expected")
					}
					toBind := toBindSymbol.Value.(string)
					for currentFrame := frame; currentFrame != nil; currentFrame = currentFrame.Parent {
						_, inFrame := currentFrame.Bindings[toBind]
						if inFrame {
							currentFrame.Bindings[toBind] = newValue
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
					case lisptype.Nil:
						return lisptype.Value{
							Car:   nil,
							Cdr:   nil,
							Type:  lisptype.Symbol,
							Value: "nil",
						}, nil
					case lisptype.String:
						return lisptype.Value{
							Car:   nil,
							Cdr:   nil,
							Type:  lisptype.Symbol,
							Value: "string",
						}, nil
					case lisptype.Number:
						return lisptype.Value{
							Car:   nil,
							Cdr:   nil,
							Type:  lisptype.Symbol,
							Value: "number",
						}, nil
					case lisptype.Symbol:
						return lisptype.Value{
							Car:   nil,
							Cdr:   nil,
							Type:  lisptype.Symbol,
							Value: "symbol",
						}, nil
					case lisptype.Boolean:
						return lisptype.Value{
							Car:   nil,
							Cdr:   nil,
							Type:  lisptype.Symbol,
							Value: "boolean",
						}, nil
					case lisptype.InterpretedFunction:
						return lisptype.Value{
							Car:   nil,
							Cdr:   nil,
							Type:  lisptype.Symbol,
							Value: "interpretedfunction",
						}, nil
					case lisptype.CompiledFunction:
						return lisptype.Value{
							Car:   nil,
							Cdr:   nil,
							Type:  lisptype.Symbol,
							Value: "compiledfunction",
						}, nil
					case lisptype.Macro:
						return lisptype.Value{
							Car:   nil,
							Cdr:   nil,
							Type:  lisptype.Symbol,
							Value: "macro",
						}, nil
					case lisptype.SpecialForm:
						return lisptype.Value{
							Car:   nil,
							Cdr:   nil,
							Type:  lisptype.Symbol,
							Value: "specialform",
						}, nil
					case lisptype.ConsCell:
						return lisptype.Value{
							Car:   nil,
							Cdr:   nil,
							Type:  lisptype.Symbol,
							Value: "conscell",
						}, nil
					default:
						return nilValue, nil
					}

				case "macroexpand-1":
					if len(arguments) != 1 {
						return nilValue, newError("wrong number of arguments passed to macroexpand-1")
					}
					var evaluatedArg lisptype.Value
					evaluatedArg, err = Eval(arguments[0], frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					if evaluatedArg.Type != lisptype.ConsCell {
						return nilValue, newError("argument to macroexpand-1 must be macro call")
					}
					macro, err := Eval(*evaluatedArg.Car, frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					if macro.Type != lisptype.Macro {
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
					var evaluatedArg lisptype.Value
					evaluatedArg, err = Eval(arguments[0], frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					if evaluatedArg.Type != lisptype.Symbol {
						return nilValue, newError("argument to bound? must be symbol")
					}
					_, found := lookup(*frame, evaluatedArg)
					if found != nil {
						return lisptype.Value{
							Car:   nil,
							Cdr:   nil,
							Type:  lisptype.Boolean,
							Value: false,
						}, nil
					}
					return lisptype.Value{
						Car:   nil,
						Cdr:   nil,
						Type:  lisptype.Boolean,
						Value: true,
					}, nil

				case "assemble":
					if len(arguments) != 1 {
						return nilValue, newError("wrong number of arguments passed to assemble")
					}
					var evaluatedArg lisptype.Value
					evaluatedArg, err = Eval(arguments[0], frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					result, err := assemble(toArray(evaluatedArg), frame)
					if err != nil {
						return nilValue, passUpError(err)
					}
					return result, nil
				}
			// if the first argument in the list is a macro,
			// then pass the arguments un-evaluated to the macro body,
			// and then evaluate the output of the macro in the current frame
			case lisptype.Macro:
				applyResult, err := apply(firstThing, arguments)
				if err != nil {
					return nilValue, passUpError(err)
				}
				// update the thing to evaluate and loop again (tail call optimized)
				toEvaluate = applyResult
			// if the first argument in the list is a function,
			// then pass the evaluated arguments to the function body,
			// and return the function output
			case lisptype.InterpretedFunction:
				evaluatedArgs := make([]lisptype.Value, 0)
				for _, arg := range arguments {
					var evaluatedArg lisptype.Value
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
			case lisptype.CompiledFunction:
				instructions := firstThing.Cdr.Value.([]lisptype.Instruction)
				workspace := make([]lisptype.Value, len(instructions)/2+1)
				if instructions[0].Class == lisptype.VarArgFlag {
					var evaluatedArgs []lisptype.Value
					for _, arg := range arguments {
						var evaluatedArg lisptype.Value
						evaluatedArg, err = Eval(arg, frame)
						if err != nil {
							return nilValue, passUpError(err)
						}
						evaluatedArgs = append(evaluatedArgs, evaluatedArg)
					}
					workspace[0] = toList(evaluatedArgs)
				} else {
					for i, arg := range arguments {
						var evaluatedArg lisptype.Value
						evaluatedArg, err = Eval(arg, frame)
						if err != nil {
							return nilValue, passUpError(err)
						}
						workspace[i] = evaluatedArg
					}
				}

				newStackFrame := lisptype.StackFrame{
					Workspace:           workspace,
					CallerIsInterpreted: true,
					ReturnLine:          0,
					CallerCode:          []lisptype.Instruction{},
					CallerEnv:           lisptype.Frame{},
				}
				stack = append(stack, newStackFrame)
				result, err := exec(firstThing)
				if err != nil {
					return nilValue, passUpError(err)
				}
				return result, nil
			default:
				return nilValue, newError("first argument in call must be a function, macro or special form")
			}
		default:
			return nilValue, newError("unknown value type")
		}
	}
}
