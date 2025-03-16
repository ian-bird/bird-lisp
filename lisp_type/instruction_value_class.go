package lisptype

type InstructionValueClass int

const (
	Arg InstructionValueClass = iota
	Stack
	Const
	ReturnRegister
)
