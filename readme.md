# notes
Bird Lisp is a self-hosted, compiled and interpreted lisp dialect. Its embedded within go, and there's plans to add a foreign function interface to allow go code to easily convert back and forth between cons cells.

The functions currently exposed to the go environment are:
* Read: takes an s-expression formatted string and returns  lisp `Value`, and an error type.
* Eval: takes a value and a pointer to a "Frame", the terminology used for lisp environments. It returns the result of  evaluating the value (i.e., the cons cell) as another value, and has the side effect of mutating the environment also passed in. It also returns an error type.
* Print: takes a value and returns an s-expression formatted string
* NewTopLevelFrame: generates a new global env with all special forms bound to it
* LoadFile: takes a file path and evaluates all the s-expressions in it, and returns an updated environment

I've written out a repl file that loads the standard library and a compiled version of it. The standard library has a lot of commonly used functions and additionally a fully working compiler for the language, which targets a bespoke virtual machine also bundled with the code. Compiling functions results in a 10x speedup for regular code, and an *up to 40x speedup* for code using lots of macros. Average speedup is around 20x. the compiler can be invoked by calling compile on a quoted lambda form, or by defining a function with def-comp. You can also use to-assembly to inspect the generated code without having it converted to structured machine code and placed in a compiledFunction object.

The compiler is pretty simple, but provides 2 useful optimizations:
1. **Compile-time macro resolution.**
	Macros are expanded when a function is compiled, which means further calls to the macro don't invoke the cost of expanding the macro. Interpreted code will expand every time, which does offer the benefit of reflecting macro re-definition without having to re-define the calling functions.
2. **tail-call optimization.**
	If the last thing a function does before returning is call another function, that call is made without consuming additional stack space. This allows for infinite looping via recursion with finite memory usage, and optimization of mutually recursive functions.

# restrictions

There aren't very many for interpreted code. This is a pretty fully featured scheme dialect. Perhaps notably, all numbers are represented internally as floats, there isn't support for lazy evaluation (though perhaps this could be hacked on using the macro facilities available...), nor is there any model for continuations.

The only major restriction for compiled code is that it cannot make a direct call to eval. I can't work out how to have eval available without resorting to making it a special form. Another smaller restriction is that macros must be capable of expanding at compile time. Macros called in interpreted functions evaluate later and present no additional cost over a regular call to an interpreted function, which allows for some tricks that aren't possible otherwise.

# additions 

Bird Lisp supports first-class macros for interpreted code. They can be passed around, stored in lists, selected dynamically just like lambdas. This isn't available for compiled functions.

