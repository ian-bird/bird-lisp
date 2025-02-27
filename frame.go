package main

// a frame contains bindings that associate
// certain strings (the value of symbol Values)
// with other values
type Frame struct {
	parent   *Frame           // the frame above this one
	bindings map[string]Value // its bindings
}
