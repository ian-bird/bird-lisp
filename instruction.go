package main

type Instruction struct {
	class        InstructionClass
	values       []Value
	valueClasses []InstructionValueClass
}