package lisptype

type Instruction struct {
	Class        InstructionClass
	Values       []Value
	ValueClasses []InstructionValueClass
}
