package main

import "fmt"

func assemble(code []Value) (Value, error) {
	instructions := make([]Instruction, 0, len(code))

	parseArgs := func(lineValues []Value) ([]Value, []InstructionValueClass) {
		values := make([]Value, 0)
		valueClasses := make([]InstructionValueClass, 0)
		i := 0
		for i < len(lineValues) {
			currentValue := lineValues[i]
			if currentValue.Type == Symbol {
				// some values have prefixes in the assembly with special meanings
				switch currentValue.Value.(string) {
				// $ prefixes a load from the stack. So we need to store the offset
				// and mark it as being a stack value
				case "$":
					values = append(values, lineValues[i+1])
					valueClasses = append(valueClasses, Stack)
					i += 2
				// arg prefixes a load from an argument
				case "arg":
					values = append(values, lineValues[i+1])
					valueClasses = append(valueClasses, Arg)
					i += 2
					// prev indicates using the value in the return register
				case "prev":
					values = append(values, currentValue)
					valueClasses = append(valueClasses, ReturnRegister)
					i += 1
					// everything else is a constant value
				default:
					values = append(values, currentValue)
					valueClasses = append(valueClasses, Const)
					i += 1
				}
			} else {
				values = append(values, currentValue)
				valueClasses = append(valueClasses, Const)
			}
		}
		return values, valueClasses
	}

	for _, line := range code {
		if line.Type == ConsCell {
			lineValues := toArray(line)
			if lineValues[0].Type == Symbol {
				switch lineValues[0].Value.(string) {
				case "gosub":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Funcall,
						values:       values,
						valueClasses: valueClasses,
					})
				case "tailcall":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Tailcall,
						values:       values,
						valueClasses: valueClasses,
					})
				case "branch":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Branch,
						values:       values,
						valueClasses: valueClasses,
					})
				case "assign":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Assign,
						values:       values,
						valueClasses: valueClasses,
					})
				case "code":
					codeInstructions, err := assemble(lineValues[1:])
					if err != nil {
						return Value{
							Car:   nil,
							Cdr:   nil,
							Type:  Nil,
							Value: nil,
						}, fmt.Errorf("exec error compiling internal fn: %v", err)
					}
					instructions = append(instructions, Instruction{
						class:        Literal,
						values:       []Value{codeInstructions},
						valueClasses: []InstructionValueClass{Const},
					})
				case "$":
					instructions = append(instructions, Instruction{
						class:        Literal,
						values:       []Value{lineValues[1]},
						valueClasses: []InstructionValueClass{Stack},
					})
				case "atom?":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Atom,
						values:       values,
						valueClasses: valueClasses,
					})
				case "car":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Car,
						values:       values,
						valueClasses: valueClasses,
					})
				case "cdr":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Cdr,
						values:       values,
						valueClasses: valueClasses,
					})
				case "cons":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Cons,
						values:       values,
						valueClasses: valueClasses,
					})
				case "label":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Label,
						values:       values,
						valueClasses: valueClasses,
					})
				case "eq?":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Eq,
						values:       values,
						valueClasses: valueClasses,
					})
				case ">":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        GreaterThan,
						values:       values,
						valueClasses: valueClasses,
					})
				case "+":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Plus,
						values:       values,
						valueClasses: valueClasses,
					})
				case "-":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Minus,
						values:       values,
						valueClasses: valueClasses,
					})
				case "*":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Times,
						values:       values,
						valueClasses: valueClasses,
					})
				case "/":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Div,
						values:       values,
						valueClasses: valueClasses,
					})
				case "%":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Mod,
						values:       values,
						valueClasses: valueClasses,
					})
				case "gensym":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Gensym,
						values:       values,
						valueClasses: valueClasses,
					})
				case "set!":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Set,
						values:       values,
						valueClasses: valueClasses,
					})
				case "type":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Type,
						values:       values,
						valueClasses: valueClasses,
					})
				case "macroexpand-1":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Macroexpand,
						values:       values,
						valueClasses: valueClasses,
					})
				case "bound?":
					values, valueClasses := parseArgs(lineValues[1:])
					instructions = append(instructions, Instruction{
						class:        Bound,
						values:       values,
						valueClasses: valueClasses,
					})
				default:
					return Value{
						Car:   nil,
						Cdr:   nil,
						Type:  Nil,
						Value: nil,
					}, fmt.Errorf("unknown assembly instruction %v", lineValues[0].Value.(string))
				}
			} else {
				instructions = append(instructions, Instruction{
					class:        Literal,
					values:       []Value{line},
					valueClasses: []InstructionValueClass{Const},
				})
			}
		} else {
			if line.Type == Symbol && line.Value.(string) == "halt" {
				instructions = append(instructions, Instruction{
					class:        Halt,
					values:       nil,
					valueClasses: nil,
				})
			} else {
				instructions = append(instructions, Instruction{
					class:        Literal,
					values:       []Value{line},
					valueClasses: []InstructionValueClass{Const},
				})
			}
		}
	}
	return Value{
		Car:   nil,
		Cdr:   nil,
		Type:  CompiledFunction,
		Value: instructions,
	}, nil
}
