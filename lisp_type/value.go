package lisptype

// this is the type enum for values
type ValueType int

// these are all the valid types for a value
const (
	Nil                 ValueType = iota // a nil value, for end of list etc
	String                               // a string of characters
	Number                               // a float64
	Symbol                               // sort of like a string, but atomic. Also used for bindings.
	Boolean                              // true or false
	InterpretedFunction                  // a callable function
	CompiledFunction                     // a compiled, callable function
	Macro                                // a callable macro
	SpecialForm                          // one of the forms handled by the underlying system, not the interpreter
	ConsCell                             // links together other values
)

// this is a value struct.
// it can contain two pointers and no data
// or it can contain a value
type Value struct {
	Car   *Value    // left pointer of a consCell
	Cdr   *Value    // right pointer of a consCell
	Type  ValueType // the type of this value
	Value any       // the value (if any)
}
