package lisptype

// a frame contains bindings that associate
// certain strings (the value of symbol Values)
// with other values
type Frame struct {
	Parent   *Frame           // the frame above this one
	Bindings map[string]Value // its bindings
}
