package lisptype

// the stackframe contains all information
// regarding the working state of a single function
type StackFrame struct {
	Workspace           []Value       // this is where in progress variables are placed, and where args are loaded
	CallerIsInterpreted bool          // do we need to load the caller at the end or halt?
	ReturnLine          int           //  if we're loading the caller, what line do we set the IP to?
	CallerCode          []Instruction // what block of code needs to be loaded?
	CallerEnv           Frame         //what env to load on exit
}
