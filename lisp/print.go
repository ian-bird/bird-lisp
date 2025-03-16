package lisp

import (
	"fmt"
	"strings"
	lisptype "test/m/lisp_type"
)

// print out each element in the lis
// with special logic for cons pairs
func printList(v lisptype.Value) string {
	result := printValue(*v.Car)
	for v = *v.Cdr; v.Type == lisptype.ConsCell; v = *v.Cdr {
		result += " "
		result += printValue(*v.Car)
	}
	if v.Type == lisptype.Nil {
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
func printValue(v lisptype.Value) string {
	switch v.Type {
	case lisptype.Nil:
		return "()"
	case lisptype.Boolean:
		if v.Value.(bool) {
			return "#t"
		} else {
			return "#f"
		}
	case lisptype.Number:
		return fmt.Sprintf("%v", v.Value.(float64))
	case lisptype.String:
		return "\"" + stringify(v.Value.(string)) + "\""
	case lisptype.ConsCell:
		return "(" + printList(v) + ")"
	case lisptype.Symbol:
		return v.Value.(string)
	default:
		return "Cannot print fn/macro/special-form"
	}
}

func Print(v lisptype.Value) string {
	unbrokenStr := printValue(v)
	var outputStr string
	for len(unbrokenStr) > 10000 {
		outputStr += unbrokenStr[:10000] + "\n"
		unbrokenStr = unbrokenStr[10000:]
	}
	return outputStr + unbrokenStr
}
