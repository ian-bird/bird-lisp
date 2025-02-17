package main

import "fmt"

const (
	Nil ValueType = iota
	String
	Number
	Symbol
	Bool
	Function
	Macro
	SpecialForm
	ConsCell
)

type struct Value {
	Car *Value
	Cdr *Value
	Type ValueType
	Value any
};

type struct Frame {
	parent *Frame
	bindings map[string]Value
};

func toArray(value Value) []Value {
	result := make([]Value)
	for value.Type == ConsCell {
		result = append(result, value.Car)
	}
	return result
}

func toList(values []Value) Value {
	if len(values) == 0 {
		return Value {
			Car: nil,
			Cdr: nil,
			Type: Nil,
			Value: nil,
		}
	} else {
		return Value {
			Car: values[0],
			Cdr: toList(values[1:]),
			Type: ConsCell,
			Value: nil,
		}
	}
}

func areEqual(a, b Value) bool {
	if a.Type != b.Type {
		return false
	}
	if a.Type != ConsCell && a.Value != b.Value {
		return false
	}
	if a.Type == ConsCell {
		return areEqual(a.Car, b.Car) && areEqual(a.Cdr, b.Cdr)
	}
	return true
}

func apply(fn Value, args []Value, frame Frame) (Value, Frame, error) {
	nilValue :=  Value {
		Car: nil,
		Cdr: nil,
		Type: Nil,
		Value: nil,
	}
	fnStatements := toArray(fn)
	params := fnStatements[0]
	newFrame := Frame {
		bindings: make(map[string]Value, 0),
		parent: &frame,
	}
	if params.Type == ConsCell {
		paramList := toArray(params)
		if len(paramsList) != len(args) {
			return nilValue, frame, "wrong number of arguments passed"
		}
		for i, _ := range paramsList {
			newFrame.bindings[string(paramsList[i].Value)] = args[i]
		}
	} else {
		newFrame.bindings[string(params.Value)] = toList(args)
	}
	var result Value
	for _, toEvaluate := range fnStatements[1:] {
		result, _, err := Eval(toEvaluate, newFrame)
		if err != nil {
			return nilValue, frame, err
		}
	}
	return result, frame, nil
}
	
func lookup(frame Frame, value Value) Value {
	if value.Type != Symbol || frame.bindings[string(value.Value)] == nil {
		return Value {
			Car: nil,
				Cdr: nil,
				Type: Nil,
				Value: nil,
			}
	}
	return frame.Bindings[string(value.Value)]
}

func NewTopLevelFrame() Frame {
	newSpecialForm := func(s string) Value {
		return Value {
			Car: nil,
			Cdr: nil,
			Type: SpecialForm,
			Value: s,
		}
	}
	return Frame {
		parent: nil,
		bindings: map[string]{
			"atom?": newSpecialForm("atom?"),
			"car": newSpecialForm("car"),
			"cdr": newSpecialForm("cdr"),
			"cond": newSpecialForm("cond"),
			"cons": newSpecialForm("cons"),
			"label": newSpecialForm("label"),
			"lambda": newSpecialForm("lambda"),
			"macro": newSpecialForm("macro"),
			"eq?": newSpecialForm("eq?"),
			"quote": newSpecialForm("quote"),
		}
	}
}

func Eval(toEvalueate Value, frame Frame) (Value, Frame, error) {
	nilValue :=  Value {
		Car: nil,
		Cdr: nil,
		Type: Nil,
		Value: nil,
	}
	switch toEvaluate.Type {
	case String:
		return string(toEvaluate.Value), frame, nil
	case Number:
		return float64(toEvaluate.Value), frame, nil
	case Boolean:
		return boolean(toEvaluate.Value), frame, nil
	case Symbol:
		return lookup(frame, toEvaluate.Value), frame, nil
	case Nil:
		return nilValue, frame, nil
	case ConsCell:
		listElements := toArray(toEvaluate)
		firstThing, err := Eval(listElements[0])
		arguments := listElements[1:]
		if err != nil {
			return nilValue, frame, err
		}
		switch firstThing.Type {
		case SpecialForm:
			switch string(firstThing.Value) {
			case "atom?":
				if length(arguments) != 1 {
					return nilValue, frame, "wrong number of arguments passed to atom?"
				}
				evaluatedArg, _, err := Eval(arguments[0], frame)
				if err != nil {
					return nilValue, frame, err
				}
				return Value {
					Car: nil,
					Cdr: nil,
					Type: Boolean,
					Value: func()boolean{
						return evaluatedArg.Type != ConsCell
					}(),
				}, frame, nil
			case "car":
				if length(arguments) != 1 {
					return nilValue, frame, "wrong number of arguments passed to car"
				}
				evaluatedArg, _, err := Eval(arguments[0], frame)
				if err != nil {
					return nilValue, frame, err
				}
				if evaluatedArg.Type != ConsCell {
					return nilValue, frame, "car expected PAIR"
				}
				return evalutedArg.Car, frame, nil
			case "cdr":
				if length(arguments) != 1 {
					return nilValue, frame, "wrong number of arguments passed to cdr"
				}
				evaluatedArg, _, err := Eval(arguments[0], frame)
				if err != nil {
					return nilValue, frame, err
				}
				if evaluatedArg.Type != ConsCell {
					return nilValue, frame, "cdr expected PAIR"
				}
				return evalutedArg.Cdr, frame, nil
			case "cond":
				if length(arguments) < 1 {
					return nilValue, frame, "wrong number of arguments passed to cond"
				}
				for i, currentBranch := range arguments {
					branchList := toArray(currentBranch)
					if branchList[0].Type == Symbol && brancList[0].Value == "else" {
						if i + 1 != length(arguments) {
							return nilValue, frame, "else clause must be last in cond statement"
						}
						var result Value
						for _, toEvaluate := range branchList[1:] {
							result, _, err := Eval(toEvaluate, frame)
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
					if predicate.Type == Boolean && !boolean(predicate.Value) {
						var result Value
						for _, toEvaluate := range branchList[1:] {
							result, _, err := Eval(toEvaluate, frame)
							if err != nil {
								return nilValue, frame, err
							}
						}
						return result, frame, nil
					}
				}
				return nilValue, frame, nil
			case "cons":
				if length(arguments) != 2 {
					return nilValue, frame, "wrong number of arguments passed to cons"
				}
				evaluatedArgs := make([]Value, 0)
				for _, arg := range arguments {
					evaluatedArg, _, err := Eval(arg, frame)
					if err != nil {
						return nilValue, frame, err
					}
					evaluatedArgs = append(evaluatedArgs, evaluatedArg)
				}
				return Value {
					car: &evaluatedArgs[0],
					cdr: &evaluatedArgs[1],
					Type: ConsCell,
					Value: nil,
				}, frame, nil
			case "eq?":
				if length(arguments) != 2 {
					return nilValue, frame, "wrong number of arguments passed to eq?"
				}
				evaluatedArgs := make([]Value, 0)
				for _, arg := range arguments {
					evaluatedArg, _, err := Eval(arg, frame)
					if err != nil {
						return nilValue, frame, err
					}
					evaluatedArgs = append(evaluatedArgs, evaluatedArg)
				}
				return Value {
					Car: nil,
						Cdr: nil,
						Type: Boolean,
						Value: func(){
							return areEqual(evaluatedArgs[0], evaluatedArgs[1])
						}(),
					}, frame, nil
			case "label":
				if length(arguments) != 2 {
					return nilValue, frame, "wrong number of arguments passed to label"
				}
				if arguments[0].Type != Symbol {
					return nilValue, frame, "label expected identifier"
				}
				toBind, _, err := Eval(arguments[1], frame)
				if err != nil {
					return nilValue, frame, err
				}
				nextFrame := Frame {
					parent: frame.parent,
					bindings: make(map[string]Value,0),
				}
				for k,v := range frame.bindings {
					nextFrame[k] = v
				}
				nextFrame.Bindings[string(arguments[0].Value)] = toBind
				return nilValue, nextFrame, nil
			case "lambda":
				if length(arguments) < 2  {
					return nilValue, frame, "lambda is ill-formed"
				}
				return Value {
					car: toEvaluate.Cdr.Car,
						cdr: toEvaluate.Cdr.Cdr,
						Type: Function,
						Value: nil,
					}, frame, nil
			case "macro":
				if length(arguments) < 2  {
					return nilValue, frame, "lambda is ill-formed"
				}
				return Value {
					car: toEvaluate.Cdr.Car,
						cdr: toEvaluate.Cdr.Cdr,
						Type: Macro,
						Value: nil,
					}, frame, nil
			case "quote":
				if length(arguments) != 1 {
					return nilValue, frame, "wrong number of arguments passed to quote"
				}
				return arguments[0], frame, nil
			}
		case Macro:
			return Eval(apply(firstThing, arguments, frame), frame)
		case Function:
			evaluatedArgs := make([]Value,0)
			for _, arg := range arguments {
				evaluatedArg, _, err := Eval(arg, frame)
				if err != nil {
					return nilValue, frame, err
				}
				evaluatedArgs = append(evaluatedArgs, evaluatedArg)
			}
			return apply(firstThing, evaluatedArgs, frame)
		default:
			return nilValue, "First argument in call must be a function, macro or special form"
		}
	}
}

func Read(s string)  (Value, error) {
	
}