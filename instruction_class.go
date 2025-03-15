package main

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
	MakeClosure
	MakeEnv
	EnvRef
	VarArgFlag
)
