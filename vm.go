package main

import "fmt"

type InstructionClass int

const (
	Halt InstructionClass = iota
	Funcall
	Tailcall
	Branch
	Label
	Literal
	Assign
	Atom
	Car
	Cdr
	Cons
	Eq
	GreaterThan
	Plus
	Minus
	Times
	Div
	Mod
	Gensym
	Set
	Type
	Macroexpand
	Bound
)

type InstructionValueClass int

const (
	Arg InstructionValueClass = iota
	Stack
	Const
	ReturnRegister
)

type Instruction struct {
	class        InstructionClass
	values       []Value
	valueClasses []InstructionValueClass
}

var returnValue Value

// the stackframe contains all information
// regarding the working state of a single function
type StackFrame struct {
	workspace           []Value       // this is where in progress variables are placed, and where args are loaded
	callerIsInterpreted bool          // do we need to load the caller at the end or halt?
	returnLine          int           //  if we're loading the caller, what line do we set the IP to?
	callerCode          []Instruction // what block of code needs to be loaded?
}

var stack []StackFrame

func accessValues(i Instruction) []Value {
	result := make([]Value, len(i.values))
	for vNum := 0; vNum < len(i.values); vNum++ {
		valueType := i.valueClasses[vNum]
		value := i.values[vNum]
		switch valueType {
		// args are loaded onto the stack before calling
		case Arg:
			result[vNum] = stack[len(stack)-1].workspace[int(value.Value.(float64))]
		case Stack:
			result[vNum] = stack[len(stack)-1].workspace[int(value.Value.(float64))]
		case Const:
			result[vNum] = value
		case ReturnRegister:
			result[vNum] = returnValue
		default:
			result[vNum] = Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Nil,
				Value: nil,
			}
		}
	}
	return result
}

func exec(instructions []Instruction, env *Frame) (Value, error) {
	nilValue := Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Nil,
		Value: nil,
	}
	instructionPointer := 0

	if len(instructions) == 0 {
		return nilValue, fmt.Errorf("exec: tried to execute empty compiled function")
	}

	for len(stack) > 0 {
		instruction := instructions[instructionPointer]
		values := accessValues(instruction)

		switch instruction.class {
		case Halt:
			// exiting this function.

			// if the caller is interpreted,
			// we have to exit execution so that control passes back to the
			// caller.
			if stack[len(stack)-1].callerIsInterpreted {
				stack = stack[:len(stack)-1]
				break
			}

			// if the stack length is 1 we need to exit as well so the value is returned
			if len(stack) == 1 {
				stack = []StackFrame{}
				break
			}

			// otherwise, control passes to calling compiled function
			instructions = stack[len(stack)-1].callerCode
			instructionPointer = stack[len(stack)-1].returnLine
			stack = stack[:len(stack)-1]
		case Funcall:
			// this call will use the stack if its a compiled function
			// first we need to figure out what's going to be ran, though.
			function := values[0]
			switch function.Type {
			case CompiledFunction:
				// do nothing
			case Symbol:
				toCall, err := lookup(*env, function)
				if err != nil {
					return nilValue, fmt.Errorf("exec funcall: %v", err)
				}
				function = toCall
			default:
				return nilValue, fmt.Errorf("exec: invalid funcall")
			}

			switch function.Type {
			case CompiledFunction:
				stackWorkspace := make([]Value, len(instructions) / 2 + 1)
				copy(stackWorkspace, values[1:])

				newFrame := StackFrame{
					workspace:           stackWorkspace,
					callerIsInterpreted: false,
					returnLine:          instructionPointer,
					callerCode:          instructions,
				}
				instructionPointer = 0
				instructions = function.Value.([]Instruction)
				stack = append(stack, newFrame)
			case InterpretedFunction:
				args := values[1:]

				var err error
				returnValue, err = apply(function, args)
				if err != nil {
					return nilValue, fmt.Errorf("exec: %v", err)
				}
			default:
				return nilValue, fmt.Errorf("exec: invalid funcall")
			}
		case Tailcall:
			// this call will not extend the stack if its a compiled function
			// first we need to figure out what's going to be ran, though.
			function := values[0]
			switch function.Type {
			case CompiledFunction:
				// do nothing
			case Symbol:
				// look up the binding
				toCall, err := lookup(*env, function)
				if err != nil {
					return nilValue, fmt.Errorf("exec funcall: %v", err)
				}
				function = toCall
			default:
				return nilValue, fmt.Errorf("exec: invalid funcall")
			}

			switch function.Type {
			case CompiledFunction:
				// compiled function can be ran without consuming stack.
				// return data remains unchanged, but blow away stack variables
				stackWorkspace := make([]Value, len(instructions) / 2 + 1)
				copy(stackWorkspace, values[1:])

				instructionPointer = -1
				instructions = function.Value.([]Instruction)
				stack[len(stack)-1].workspace = stackWorkspace
			case InterpretedFunction:
				// can't avoid cost of making a call to an interpreted function
				args := values[1:]

				var err error
				returnValue, err = apply(function, args)
				if err != nil {
					return nilValue, fmt.Errorf("exec: %v", err)
				}
			default:
				return nilValue, fmt.Errorf("exec: invalid funcall")
			}
		case Branch:
			// if the predicate is not nil or false,
			// then jump the instruction pointer to the target line
			predicate := values[0]
			if predicate.Type != Nil && (predicate.Type != Boolean || predicate.Value.(bool)) {
				instructionPointer = int(instruction.values[1].Value.(float64)) - 1 // -1 because its added at the end of processing
			}
		case Label:
			// update the environment
			sym := values[0]
			env.bindings[sym.Value.(string)] = values[1]
			returnValue = nilValue
		case Literal:
			// literals can just be loaded into the return reg
			// and then we continue on
			returnValue = values[0]
		case Assign:
			// load the value into the correct slot in the stack workspace
			val := values[0]
			whichSlot := int(instruction.values[1].Value.(float64))
			stack[len(stack)-1].workspace[whichSlot] = val
		case Atom:
			returnValue = Value{
				Car:  nil,
				Cdr:  nil,
				Type: Boolean,
				Value: func() bool {
					return values[0].Type != ConsCell
				}(),
			}
		case Car:
			val := values[0]
			if val.Type != ConsCell {
				return nilValue, fmt.Errorf("exec: car expected PAIR")
			}
			returnValue = *val.Car
		case Cdr:
			val := values[0]
			if val.Type != ConsCell {
				return nilValue, fmt.Errorf("exec: cdr expected PAIR")
			}
			returnValue = *val.Cdr
		case Cons:
			returnValue = Value{
				Car:   &values[0],
				Cdr:   &values[1],
				Type:  ConsCell,
				Value: nil,
			}
		case Eq:
			returnValue = Value{
				Car:  nil,
				Cdr:  nil,
				Type: Boolean,
				Value: func() bool {
					return areEqual(values[0], values[1])
				}(),
			}
		case GreaterThan:
			maxValue := values[0].Value.(float64)
			result := true
			for _, value := range values[1:] {
				if value.Value.(float64) >= maxValue {
					result = false
				}
			}
			returnValue = Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Boolean,
				Value: result,
			}
		case Plus:
			var result float64
			for _, value := range values {
				result += value.Value.(float64)
			}
			returnValue = Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Number,
				Value: result,
			}
		case Minus:
			result := values[0].Value.(float64)
			for _, value := range values[1:] {
				result -= value.Value.(float64)
			}
			returnValue = Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Number,
				Value: result,
			}
		case Times:
			var result float64
			for _, value := range values {
				result *= value.Value.(float64)
			}
			returnValue = Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Number,
				Value: result,
			}
		case Div:
			result := values[0].Value.(float64)
			for _, value := range values[1:] {
				result /= value.Value.(float64)
			}
			returnValue = Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Number,
				Value: result,
			}
		case Mod:
			var result float64
			for _, value := range values {
				result = result - float64(int(result/value.Value.(float64)))*value.Value.(float64)
			}
			returnValue = Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Number,
				Value: result,
			}
		case Gensym:
			returnValue = Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Symbol,
				Value: fmt.Sprintf("G#%v", gensymCounter),
			}
			gensymCounter++
		case Set:
			toBind := values[0].Value.(string)
			for currentFrame := env; currentFrame != nil; currentFrame = currentFrame.parent {
				_, inFrame := currentFrame.bindings[toBind]
				if inFrame {
					currentFrame.bindings[toBind] = values[1]
					break
				}
			}
			returnValue = nilValue
		case Type:
			switch values[0].Type {
			case Nil:
				returnValue = Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Symbol,
					Value: "nil",
				}
			case String:
				returnValue = Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Symbol,
					Value: "string",
				}
			case Number:
				returnValue = Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Symbol,
					Value: "number",
				}
			case Symbol:
				returnValue = Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Symbol,
					Value: "symbol",
				}
			case Boolean:
				returnValue = Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Symbol,
					Value: "boolean",
				}
			case InterpretedFunction:
				returnValue = Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Symbol,
					Value: "interpretedfunction",
				}
			case CompiledFunction:
				returnValue = Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Symbol,
					Value: "compiledfunction",
				}
			case Macro:
				returnValue = Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Symbol,
					Value: "macro",
				}
			case SpecialForm:
				returnValue = Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Symbol,
					Value: "specialform",
				}
			case ConsCell:
				returnValue = Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Symbol,
					Value: "conscell",
				}
			default:
				returnValue = nilValue
			}
		case Macroexpand:
			cell := []Value{{
				Car:   nil,
				Cdr:   nil,
				Type:  Symbol,
				Value: "macroexpand-1",
			}, toList(values)}
			// make this an interpreted call
			result, err := Eval(Value{
				Car:   &cell[0],
				Cdr:   &cell[1],
				Type:  ConsCell,
				Value: nil,
			}, env)

			if err != nil {
				return nilValue, fmt.Errorf("exec macroexpand-1: %v", err)
			}

			returnValue = result
		case Bound:
			_, lookupError := lookup(*env, values[0])
			returnValue = Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Boolean,
				Value: lookupError == nil,
			}
		default:
			return nilValue, fmt.Errorf("exec: unknown instruction encountered")
		}

		instructionPointer++
	}
	return returnValue, nil
}
