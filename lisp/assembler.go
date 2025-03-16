package lisp

import (
	"fmt"
	lisptype "test/m/lisp_type"
)

func assemble(code []lisptype.Value, env *lisptype.Frame) (lisptype.Value, error) {
	instructions := make([]lisptype.Instruction, 0, len(code))

	parseArgs := func(lineValues []lisptype.Value) ([]lisptype.Value, []lisptype.InstructionValueClass) {
		values := make([]lisptype.Value, 0)
		valueClasses := make([]lisptype.InstructionValueClass, 0)
		i := 0
		for i < len(lineValues) {
			currentValue := lineValues[i]
			if currentValue.Type == lisptype.Symbol {
				// some values have prefixes in the assembly with special meanings
				switch currentValue.Value.(string) {
				// $ prefixes a load from the stack. So we need to store the offset
				// and mark it as being a stack value
				case "$":
					values = append(values, lineValues[i+1])
					valueClasses = append(valueClasses, lisptype.Stack)
					i += 2
				// arg prefixes a load from an argument
				case "arg#":
					values = append(values, lineValues[i+1])
					valueClasses = append(valueClasses, lisptype.Arg)
					i += 2
					// prev indicates using the value in the return register
				case "prev":
					values = append(values, currentValue)
					valueClasses = append(valueClasses, lisptype.ReturnRegister)
					i += 1
				default:
					values = append(values, currentValue)
					valueClasses = append(valueClasses, lisptype.Const)
					i += 1
				}
			} else {
				values = append(values, currentValue)
				valueClasses = append(valueClasses, lisptype.Const)
				i += 1
			}
		}
		return values, valueClasses
	}

	for _, line := range code {
		if line.Type == lisptype.ConsCell {
			lineValues := toArray(line)
			if lineValues[0].Type == lisptype.Symbol {
				switch lineValues[0].Value.(string) {
				case "gosub":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Funcall,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "tailcall":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Tailcall,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "branch":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Branch,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "assign":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Assign,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "code":
					codeInstructions, err := assemble(lineValues[1:], env)
					if err != nil {
						return lisptype.Value{
							Car:   nil,
							Cdr:   nil,
							Type:  lisptype.Nil,
							Value: nil,
						}, fmt.Errorf("exec error compiling internal fn: %v", err)
					}
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Literal,
						Values:       []lisptype.Value{codeInstructions},
						ValueClasses: []lisptype.InstructionValueClass{lisptype.Const},
					})
				case "$":
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Literal,
						Values:       []lisptype.Value{lineValues[1]},
						ValueClasses: []lisptype.InstructionValueClass{lisptype.Stack},
					})
				case "atom?":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Atom,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "car":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Car,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "cdr":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Cdr,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "cons":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Cons,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "label":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Label,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "eq?":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Eq,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case ">":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.GreaterThan,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "+":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Plus,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "-":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Minus,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "*":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Times,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "/":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Div,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "%":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Mod,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "gensym":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Gensym,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "set!":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Set,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "type":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Type,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "macroexpand-1":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Macroexpand,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "bound?":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Bound,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "quote":
					values := lineValues[1:]
					valueClasses := []lisptype.InstructionValueClass{lisptype.Const}
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Literal,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "assemble":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.Assemble,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "make-closure":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.MakeClosure,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "make-env":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.MakeEnv,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "env-ref":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.EnvRef,
						Values:       values,
						ValueClasses: valueClasses,
					})
				case "var-arg":
					instructions = append(instructions, lisptype.Instruction{
						Class:        lisptype.VarArgFlag,
						Values:       []lisptype.Value{},
						ValueClasses: []lisptype.InstructionValueClass{},
					})
				default:
					return lisptype.Value{
						Car:   nil,
						Cdr:   nil,
						Type:  lisptype.Nil,
						Value: nil,
					}, fmt.Errorf("unknown assembly instruction %v", lineValues[0].Value.(string))
				}
			} else {
				instructions = append(instructions, lisptype.Instruction{
					Class:        lisptype.Literal,
					Values:       []lisptype.Value{line},
					ValueClasses: []lisptype.InstructionValueClass{lisptype.Const},
				})
			}
		} else {
			if line.Type == lisptype.Symbol && line.Value.(string) == "halt" {
				instructions = append(instructions, lisptype.Instruction{
					Class:        lisptype.Halt,
					Values:       nil,
					ValueClasses: nil,
				})
			} else {
				instructions = append(instructions, lisptype.Instruction{
					Class:        lisptype.Literal,
					Values:       []lisptype.Value{line},
					ValueClasses: []lisptype.InstructionValueClass{lisptype.Const},
				})
			}
		}
	}
	// we're following as closely as possible the structure
	// of interpreted functions:
	//
	// bindings go in car (not needed for compiled fns, data is
	// passed in directly on the stack), lexically scoped env
	// pointer goes in value, and the code goes in cdr.
	return lisptype.Value{
		// store duplicate of the environment in the car
		// so that we have some way of updating bindings
		Car: nil,
		Cdr: &lisptype.Value{
			Car:   nil,
			Cdr:   nil,
			Type:  lisptype.Nil,
			Value: instructions,
		},
		Type:  lisptype.CompiledFunction,
		Value: env,
	}, nil
}
