package main 

// the stackframe contains all information
// regarding the working state of a single function
type StackFrame struct {
	workspace           []Value       // this is where in progress variables are placed, and where args are loaded
	callerIsInterpreted bool          // do we need to load the caller at the end or halt?
	returnLine          int           //  if we're loading the caller, what line do we set the IP to?
	callerCode          []Instruction // what block of code needs to be loaded?
	callerEnv           Frame         //what env to load on exit
}