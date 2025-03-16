package lisp

import (
	"fmt"
	lisptype "test/m/lisp_type"
)

var returnValue lisptype.Value

var stack []lisptype.StackFrame

func exec(compiledFunction lisptype.Value) (lisptype.Value, error) {
	instructions := compiledFunction.Cdr.Value.([]lisptype.Instruction)

	newEnv := func(compiledFunction lisptype.Value) lisptype.Frame {
		return lisptype.Frame{
			Parent:   compiledFunction.Value.(*lisptype.Frame),
			Bindings: nil,
		}
	}

	accessValues := func(i lisptype.Instruction) []lisptype.Value {
		result := make([]lisptype.Value, len(i.Values))
		for vNum := 0; vNum < len(i.Values); vNum++ {
			valueType := i.ValueClasses[vNum]
			value := i.Values[vNum]
			switch valueType {
			// args are loaded onto the stack before calling
			case lisptype.Arg:
				result[vNum] = stack[len(stack)-1].Workspace[int(value.Value.(float64))]
			case lisptype.Stack:
				result[vNum] = stack[len(stack)-1].Workspace[int(value.Value.(float64))]
			case lisptype.Const:
				result[vNum] = value
			case lisptype.ReturnRegister:
				result[vNum] = returnValue
			default:
				result[vNum] = lisptype.Value{
					Car:   nil,
					Cdr:   nil,
					Type:  lisptype.Nil,
					Value: nil,
				}
			}
		}
		return result
	}

	env := newEnv(compiledFunction)

	nilValue := lisptype.Value{
		Car:   nil,
		Cdr:   nil,
		Type:  lisptype.Nil,
		Value: nil,
	}
	instructionPointer := 0

	if len(instructions) == 0 {
		return nilValue, fmt.Errorf("exec: tried to execute empty compiled function")
	}

executionLoop:
	for len(stack) > 0 {
		instruction := instructions[instructionPointer]
		values := accessValues(instruction)

		switch instruction.Class {
		case lisptype.Halt:
			// exiting this function.

			// if the caller is interpreted,
			// we have to exit execution so that control passes back to the
			// caller.
			if stack[len(stack)-1].CallerIsInterpreted {
				stack = stack[:len(stack)-1]
				break executionLoop
			}

			// if the stack length is 1 we need to exit as well so the value is returned
			if len(stack) == 1 {
				stack = []lisptype.StackFrame{}
				break executionLoop
			}

			// otherwise, control passes to calling compiled function
			instructions = stack[len(stack)-1].CallerCode
			instructionPointer = stack[len(stack)-1].ReturnLine

			env = stack[len(stack)-1].CallerEnv
			stack = stack[:len(stack)-1]
		case lisptype.Funcall:
			// this call will use the stack if its a compiled function
			// first we need to figure out what's going to be ran, though.
			function := values[0]
			args := values[1:]

			var evaledArgs []lisptype.Value
			for i, arg := range args {
				if arg.Type == lisptype.Symbol && instruction.ValueClasses[i+1] == lisptype.Const {
					evaledArg, err := lookup(env, arg)
					if err != nil || evaledArg.Type != lisptype.SpecialForm {
						evaledArgs = append(evaledArgs, arg)
					} else {
						evaledArgs = append(evaledArgs, evaledArg)
					}
				} else {
					evaledArgs = append(evaledArgs, arg)
				}
			}
			switch function.Type {
			case lisptype.InterpretedFunction, lisptype.CompiledFunction, lisptype.SpecialForm:
				// do nothing
			case lisptype.Symbol:
				toCall, err := lookup(env, function)
				if err != nil {
					return nilValue, fmt.Errorf("exec funcall: %v", err)
				}
				function = toCall
			default:
				return nilValue, fmt.Errorf("exec: invalid funcall")
			}

			switch function.Type {
			case lisptype.CompiledFunction:
				// we can't load the code just yet since we need to construct
				// our continuation first. So create the new workspace
				newFrame := lisptype.StackFrame{
					Workspace: func() []lisptype.Value {
						result := make([]lisptype.Value, len(function.Cdr.Value.([]lisptype.Instruction))/2+1)
						if function.Cdr.Value.([]lisptype.Instruction)[0].Class == lisptype.VarArgFlag {
							result[0] = toList(evaledArgs)
						} else {
							copy(result, evaledArgs)
						}
						return result
					}(),
					CallerIsInterpreted: false,
					ReturnLine:          instructionPointer,
					CallerCode:          instructions,
					CallerEnv:           env,
				}
				instructionPointer = -1
				instructions = function.Cdr.Value.([]lisptype.Instruction)
				stack = append(stack, newFrame)
				env = newEnv(function)

			case lisptype.InterpretedFunction:
				var err error
				returnValue, err = apply(function, evaledArgs)
				if err != nil {
					return nilValue, fmt.Errorf("exec: %v", err)
				}
			case lisptype.SpecialForm:
				// need to quote the special form so that eval doesnt explode
				var err error

				quotedValues := make([]lisptype.Value, 0, len(values))
				for _, value := range values {
					quotedValues = append(quotedValues, toList([]lisptype.Value{{
						Car:   nil,
						Cdr:   nil,
						Type:  lisptype.Symbol,
						Value: "quote",
					}, value}))
				}
				returnValue, err = Eval(toList(quotedValues), &env)
				if err != nil {
					return nilValue, fmt.Errorf("exec: %v", err)
				}
			default:
				return nilValue, fmt.Errorf("exec: invalid funcall")
			}
		case lisptype.Tailcall:
			// this call will not extend the stack if its a compiled function
			// first we need to figure out what's going to be ran, though.
			function := values[0]
			args := values[1:]
			var evaledArgs []lisptype.Value
			for i, arg := range args {
				if arg.Type == lisptype.Symbol {
					evaledArg, err := lookup(env, arg)
					if err != nil || evaledArg.Type != lisptype.SpecialForm || instruction.ValueClasses[i+1] != lisptype.Const {
						evaledArgs = append(evaledArgs, arg)
					} else {
						evaledArgs = append(evaledArgs, evaledArg)
					}
				} else {
					evaledArgs = append(evaledArgs, arg)
				}
			}
			switch function.Type {
			case lisptype.InterpretedFunction, lisptype.CompiledFunction, lisptype.SpecialForm:
				// do nothing
			case lisptype.Symbol:
				// look up the binding
				toCall, err := lookup(env, function)
				if err != nil {
					return nilValue, fmt.Errorf("exec funcall: %v", err)
				}
				function = toCall
			default:
				return nilValue, fmt.Errorf("exec: invalid funcall")
			}

			switch function.Type {
			case lisptype.CompiledFunction:
				instructionPointer = -1
				instructions = function.Cdr.Value.([]lisptype.Instruction)
				// compiled function can be ran without consuming stack.
				// return data remains unchanged, but blow away stack variables
				stackWorkspace := make([]lisptype.Value, len(instructions)/2+1)
				if instructions[0].Class == lisptype.VarArgFlag {
					stackWorkspace[0] = toList(evaledArgs)
				} else {
					copy(stackWorkspace, evaledArgs)
				}
				stack[len(stack)-1].Workspace = stackWorkspace

				env = newEnv(function)
			case lisptype.InterpretedFunction:
				// can't avoid cost of making a call to an interpreted function
				var err error
				returnValue, err = apply(function, evaledArgs)
				if err != nil {
					return nilValue, fmt.Errorf("exec: %v", err)
				}
			case lisptype.SpecialForm:
				// need to quote the special form so that eval doesnt explode
				var err error

				quotedValues := make([]lisptype.Value, 0, len(values))
				for _, value := range values {
					quotedValues = append(quotedValues, toList([]lisptype.Value{{
						Car:   nil,
						Cdr:   nil,
						Type:  lisptype.Symbol,
						Value: "quote",
					}, value}))
				}
				returnValue, err = Eval(toList(quotedValues), &env)
				if err != nil {
					return nilValue, fmt.Errorf("exec: %v", err)
				}
			default:
				return nilValue, fmt.Errorf("exec: invalid funcall")
			}
		case lisptype.Branch:
			// if the predicate is not nil or false,
			// then jump the instruction pointer to the target line
			predicate := values[0]
			if predicate.Type != lisptype.Nil && (predicate.Type != lisptype.Boolean || predicate.Value.(bool)) {
				instructionPointer = int(instruction.Values[1].Value.(float64)) - 1
				// -1 because its added at the end of processing
			}
		case lisptype.Label:
			// if bindings are nilled out fix that now that we need it
			if env.Bindings == nil {
				env.Bindings = make(map[string]lisptype.Value)
			}

			if values[0].Type != lisptype.Symbol {
				return nilValue, fmt.Errorf("exec: cannot label non-symbol")
			}
			env.Bindings[values[0].Value.(string)] = values[1]

			returnValue = nilValue
		case lisptype.MakeClosure:
			function := values[0]
			environment := values[1]
			returnValue = lisptype.Value{
				Car:   nil,
				Cdr:   function.Cdr,
				Type:  lisptype.CompiledFunction,
				Value: environment.Value,
			}
		case lisptype.EnvRef:
			var err error
			returnValue, err = lookup(env, values[0])
			if err != nil {
				return nilValue, fmt.Errorf("exec: failed envref lookup, this should never happen")
			}
		case lisptype.MakeEnv:
			// if bindings are nilled out fix that now that we need it
			if env.Bindings == nil {
				env.Bindings = make(map[string]lisptype.Value)
			}

			i := 0
			for i < len(values) {
				// if we get a stack value we need to add it to the env
				if instruction.ValueClasses[i] == lisptype.Stack {
					label := values[i+1].Value.(string)
					env.Bindings[label] = values[i]
					i += 2
				} else {
					i++
				}
			}

			returnValue = lisptype.Value{
				Car:   nil,
				Cdr:   nil,
				Type:  lisptype.Nil,
				Value: &[]lisptype.Frame{env}[0], // copy env and get pointer
			}
		case lisptype.Literal:
			returnValue = values[0]
		case lisptype.Assign:
			// load the value into the correct slot in the stack workspace
			val := values[0]
			whichSlot := int(instruction.Values[1].Value.(float64))
			stack[len(stack)-1].Workspace[whichSlot] = val
		case lisptype.Atom:
			returnValue = lisptype.Value{
				Car:  nil,
				Cdr:  nil,
				Type: lisptype.Boolean,
				Value: func() bool {
					return values[0].Type != lisptype.ConsCell
				}(),
			}
		case lisptype.Car:
			val := values[0]
			if val.Type != lisptype.ConsCell {
				return nilValue, fmt.Errorf("exec: car expected PAIR")
			}
			returnValue = *val.Car
		case lisptype.Cdr:
			val := values[0]
			if val.Type != lisptype.ConsCell {
				return nilValue, fmt.Errorf("exec: cdr expected PAIR")
			}
			returnValue = *val.Cdr
		case lisptype.Cons:
			returnValue = lisptype.Value{
				Car:   &values[0],
				Cdr:   &values[1],
				Type:  lisptype.ConsCell,
				Value: nil,
			}
		case lisptype.Eq:
			returnValue = lisptype.Value{
				Car:  nil,
				Cdr:  nil,
				Type: lisptype.Boolean,
				Value: func() bool {
					return areEqual(values[0], values[1])
				}(),
			}
		case lisptype.GreaterThan:
			maxValue := values[0].Value.(float64)
			result := true
			for _, value := range values[1:] {
				if value.Value.(float64) >= maxValue {
					result = false
				}
			}
			returnValue = lisptype.Value{
				Car:   nil,
				Cdr:   nil,
				Type:  lisptype.Boolean,
				Value: result,
			}
		case lisptype.Plus:
			var result float64
			for _, value := range values {
				result += value.Value.(float64)
			}
			returnValue = lisptype.Value{
				Car:   nil,
				Cdr:   nil,
				Type:  lisptype.Number,
				Value: result,
			}
		case lisptype.Minus:
			result := values[0].Value.(float64)
			for _, value := range values[1:] {
				result -= value.Value.(float64)
			}
			returnValue = lisptype.Value{
				Car:   nil,
				Cdr:   nil,
				Type:  lisptype.Number,
				Value: result,
			}
		case lisptype.Times:
			result := values[0].Value.(float64)
			for _, value := range values[1:] {
				result *= value.Value.(float64)
			}
			returnValue = lisptype.Value{
				Car:   nil,
				Cdr:   nil,
				Type:  lisptype.Number,
				Value: result,
			}
		case lisptype.Div:
			result := values[0].Value.(float64)
			for _, value := range values[1:] {
				result /= value.Value.(float64)
			}
			returnValue = lisptype.Value{
				Car:   nil,
				Cdr:   nil,
				Type:  lisptype.Number,
				Value: result,
			}
		case lisptype.Mod:
			var result float64
			for _, value := range values {
				result = result - float64(int(result/value.Value.(float64)))*value.Value.(float64)
			}
			returnValue = lisptype.Value{
				Car:   nil,
				Cdr:   nil,
				Type:  lisptype.Number,
				Value: result,
			}
		case lisptype.Gensym:
			returnValue = lisptype.Value{
				Car:   nil,
				Cdr:   nil,
				Type:  lisptype.Symbol,
				Value: fmt.Sprintf("G#%v", gensymCounter),
			}
			gensymCounter++
		case lisptype.Set:
			toBind := values[0].Value.(string)
			for currentFrame := &env; currentFrame != nil; currentFrame = currentFrame.Parent {
				_, inFrame := currentFrame.Bindings[toBind]
				if inFrame {
					currentFrame.Bindings[toBind] = values[1]
					break
				}
			}
			returnValue = nilValue
		case lisptype.Type:
			switch values[0].Type {
			case lisptype.Nil:
				returnValue = lisptype.Value{
					Car:   nil,
					Cdr:   nil,
					Type:  lisptype.Symbol,
					Value: "nil",
				}
			case lisptype.String:
				returnValue = lisptype.Value{
					Car:   nil,
					Cdr:   nil,
					Type:  lisptype.Symbol,
					Value: "string",
				}
			case lisptype.Number:
				returnValue = lisptype.Value{
					Car:   nil,
					Cdr:   nil,
					Type:  lisptype.Symbol,
					Value: "number",
				}
			case lisptype.Symbol:
				returnValue = lisptype.Value{
					Car:   nil,
					Cdr:   nil,
					Type:  lisptype.Symbol,
					Value: "symbol",
				}
			case lisptype.Boolean:
				returnValue = lisptype.Value{
					Car:   nil,
					Cdr:   nil,
					Type:  lisptype.Symbol,
					Value: "boolean",
				}
			case lisptype.InterpretedFunction:
				returnValue = lisptype.Value{
					Car:   nil,
					Cdr:   nil,
					Type:  lisptype.Symbol,
					Value: "interpretedfunction",
				}
			case lisptype.CompiledFunction:
				returnValue = lisptype.Value{
					Car:   nil,
					Cdr:   nil,
					Type:  lisptype.Symbol,
					Value: "compiledfunction",
				}
			case lisptype.Macro:
				returnValue = lisptype.Value{
					Car:   nil,
					Cdr:   nil,
					Type:  lisptype.Symbol,
					Value: "macro",
				}
			case lisptype.SpecialForm:
				returnValue = lisptype.Value{
					Car:   nil,
					Cdr:   nil,
					Type:  lisptype.Symbol,
					Value: "specialform",
				}
			case lisptype.ConsCell:
				returnValue = lisptype.Value{
					Car:   nil,
					Cdr:   nil,
					Type:  lisptype.Symbol,
					Value: "conscell",
				}
			default:
				returnValue = nilValue
			}
		case lisptype.Macroexpand:
			cell := []lisptype.Value{{
				Car:   nil,
				Cdr:   nil,
				Type:  lisptype.Symbol,
				Value: "macroexpand-1",
			}, toList(values)}
			// make this an interpreted call
			result, err := Eval(lisptype.Value{
				Car:   &cell[0],
				Cdr:   &cell[1],
				Type:  lisptype.ConsCell,
				Value: nil,
			}, &env)

			if err != nil {
				return nilValue, fmt.Errorf("exec macroexpand-1: %v", err)
			}

			returnValue = result
		case lisptype.Bound:
			_, lookupError := lookup(env, values[0])
			returnValue = lisptype.Value{
				Car:   nil,
				Cdr:   nil,
				Type:  lisptype.Boolean,
				Value: lookupError == nil,
			}
		case lisptype.VarArgFlag:
			returnValue = nilValue
		case lisptype.Assemble:
			var err error
			returnValue, err = assemble(toArray(values[0]), &env)
			if err != nil {
				return nilValue, fmt.Errorf("exec assemble: %v", err)
			}
		default:
			return nilValue, fmt.Errorf("exec: unknown instruction encountered")
		}

		instructionPointer++
	}
	return returnValue, nil
}
