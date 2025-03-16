package main

import (
	"fmt"
	lisp "test/m/lisp"
)

func main() {
	frame := lisp.NewTopLevelFrame()
	frame, loadErr := lisp.LoadFile("lisp_src/stdlib.lisp", frame)
	if loadErr != nil {
		fmt.Printf("error loading file: %v", loadErr)
		return
	}

	frame, loadErr = lisp.LoadFile("lisp_src/compiler.lisp", frame)
	if loadErr != nil {
		fmt.Printf("error loading file: %v", loadErr)
		return
	}

	frame, loadErr = lisp.LoadFile("lisp_src/compiled-lib.lisp", frame)
	if loadErr != nil {
		fmt.Printf("error loading file: %v", loadErr)
		return
	}

	lisp.Repl(&frame)
}
