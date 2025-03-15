package main

import (
	"fmt"
	"strings"
)

// print out each element in the lis
// with special logic for cons pairs
func printList(v Value) string {
	result := printValue(*v.Car)
	for v = *v.Cdr; v.Type == ConsCell; v = *v.Cdr {
		result += " "
		result += printValue(*v.Car)
	}
	if v.Type == Nil {
		return result
	} else {
		return result + ". " + printValue(v)
	}
}

// escapes special characters in string
func stringify(s string) string {
	s = strings.ReplaceAll(s, "\\", "\\\\")
	s = strings.ReplaceAll(s, "\"", "\\\"")
	s = strings.ReplaceAll(s, "\n", "\\n")
	s = strings.ReplaceAll(s, "\t", "\\t")
	s = strings.ReplaceAll(s, "\r", "\\r")
	return s
}

// converts a value to a string recursively,
// representing lists by wrapping them with parenthesis
func printValue(v Value) string {
	switch v.Type {
	case Nil:
		return "()"
	case Boolean:
		if v.Value.(bool) {
			return "#t"
		} else {
			return "#f"
		}
	case Number:
		return fmt.Sprintf("%v", v.Value.(float64))
	case String:
		return "\"" + stringify(v.Value.(string)) + "\""
	case ConsCell:
		return "(" + printList(v) + ")"
	case Symbol:
		return v.Value.(string)
	default:
		return "Cannot print fn/macro/special-form"
	}
}

func Print(v Value) string {
	unbrokenStr := printValue(v)
	var outputStr string
	for len(unbrokenStr) > 10000 {
		outputStr += unbrokenStr[:10000] + "\n"
		unbrokenStr = unbrokenStr[10000:]
	}
	return outputStr + unbrokenStr
}
