package main

import (
	"errors"
	"fmt"
)

// reads a list in and creates a new value for it
// read list will call read repeatedly as it traverses
// its contents, until it hits a closing parens.
func readList(s string) (string, Value, error) {
	thisList := make([]Value, 0)
	remainingString := s
	appendAtom := false
	thingsToRightOfDot := -1
	for remainingString != "" {
		var nextValue Value
		var err error
		remainingString, nextValue, err = Read(remainingString)
		if err != nil {
			// this error occurs when we get to the end of the list
			if err.Error() == "unexpected ')'" {
				asList := toList(thisList)
				listTraverser := &asList
				// if append atom is true, go to the last cons cell,
				// the one whose cdr is null. Replace this with the
				// car of that cons cell.
				if appendAtom && thingsToRightOfDot != 1 {
					return remainingString, Value{
						Car:   nil,
						Cdr:   nil,
						Type:  Nil,
						Value: nil,
					}, errors.New("improperly placed . ")
				}
				if appendAtom {
					for listTraverser.Cdr.Type == ConsCell {
						listTraverser = listTraverser.Cdr
					}
					listTraverser.Value = listTraverser.Car.Value
					listTraverser.Type = listTraverser.Car.Type
					listTraverser.Car = nil
					listTraverser.Cdr = nil
				}
				return remainingString, asList, nil
				// when we encounter a .
				// we only want to see one ., and it needs to be
				// right before the last element. If it isn't,
				// we're going to generate an improperly placed . error
			} else if err.Error() == "unexpected '.'" {
				if appendAtom {
					return remainingString, Value{
						Car:   nil,
						Cdr:   nil,
						Type:  Nil,
						Value: nil,
					}, errors.New("improperly placed . ")
				}
				appendAtom = true
				// else this isn't an error we can catch, so propogate it up
			} else {
				return remainingString, Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Nil,
					Value: nil,
				}, err
			}
		}
		if appendAtom {
			thingsToRightOfDot++
		}
		if thingsToRightOfDot > 1 {
			return remainingString, Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Nil,
				Value: nil,
			}, errors.New("improperly placed . ")
		}
		thisList = append(thisList, nextValue)
	}
	// if we didnt return early and hit the end of the string,
	// then there was no matching closing parens and we need to generate an error
	return "", Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Nil,
		Value: nil,
	}, errors.New("could not find matching closing parens")
}

// read a string in and create a new atom for it
func readString(s string) (string, Value, error) {
	escape := false
	result := ""
	for i := range s {
		if escape {
			switch s[i] {
			case '"':
				result += "\""
			case 'n':
				result += "\n"
			case 't':
				result += "\t"
			case 'r':
				result += "\r"
			case '\\':
				result += "\\"
			}
			escape = false
		} else {
			if s[i] == '\\' {
				escape = true
			} else if s[i] == '"' {
				return s[i+1:], Value{
					Car:   nil,
					Cdr:   nil,
					Type:  String,
					Value: result,
				}, nil
			} else {
				result += string(s[i])
			}
		}
	}
	return "", Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Nil,
		Value: nil,
	}, errors.New("could not find matching closing double quote")
}

// read a number in and create a new symbol for it
func readNumber(s string) (string, Value, error) {
	var result float64
	fmt.Sscanf(s, "%f", &result)
	i := 0
	for i < len(s) && (('0' <= s[i] && s[i] <= '9' || s[i] == '.') || (i == 0 && s[i] == '-')) {
		i++
	}
	return s[i:], Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Number,
		Value: result,
	}, nil
}

// reads a symbol in and creates a new atom for it
func readSymbol(s string) (string, Value, error) {
	result := ""
	i := 0
	for ; i < len(s) && s[i] != ' ' && s[i] != '\t' && s[i] != '\n' && s[i] != '(' && s[i] != ')'; i++ {
		result += string(s[i])
	}
	return s[i:], Value{
		Car:   nil,
		Cdr:   nil,
		Type:  Symbol,
		Value: result,
	}, nil
}

// reads a string and converts it into a value
// lists are represented as linked lists of cons cells
// that descend on the cdr side, with each value held
// in the car of that specific cons cell.
// the string returned is the remainder of it after the read was done
// sometimes read may need to be applied multiple times to fully ingest a string,
// since read only consumes the first s-expression that it encounters.
func Read(s string) (string, Value, error) {
	// lists start with open paren
	if s[0] == '(' {
		return readList(s[1:])
		// strings start with quotes
	} else if s[0] == '"' {
		return readString(s[1:])
		// numbers start with a digit
	} else if ('0' <= s[0] && s[0] <= '9') || (s[0] == '-' && len(s) >= 2 &&
		'0' <= s[1] && s[1] <= '9') {
		return readNumber(s)
		// if its white space, skip forward and call read again when we get to something
		// thats not whitespace
	} else if s[0] == ' ' || s[0] == '\t' || s[0] == '\n' {
		skipForward := 1
		for skipForward < len(s) &&
			(s[skipForward] == ' ' || s[skipForward] == '\t' || s[skipForward] == '\n') {
			skipForward++
		}
		if skipForward == len(s) {
			return "", Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Nil,
				Value: nil,
			}, nil
		}
		return Read(s[skipForward:])
	} else if s[0] == '\'' {
		remaining, beingQuoted, err := Read(s[1:])
		return remaining, Value{
			Car: &Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Symbol,
				Value: "quote",
			},
			Cdr: &Value{
				Car: &beingQuoted,
				Cdr: &Value{
					Car:   nil,
					Cdr:   nil,
					Type:  Nil,
					Value: nil,
				},
				Type:  ConsCell,
				Value: nil,
			},
			Type:  ConsCell,
			Value: nil,
		}, err
		// skip over comments
	} else if s[0] == ';' {
		skipForward := 1
		for skipForward < len(s) && s[skipForward] != '\n' {
			skipForward++
		}
		if skipForward == len(s) {
			return "", Value{
				Car:   nil,
				Cdr:   nil,
				Type:  Nil,
				Value: nil,
			}, nil
		}
		return Read(s[skipForward:])
		// true and false have special representations
	} else if len(s) >= 2 && s[0:2] == "#f" {
		return s[2:], Value{
			Car:   nil,
			Cdr:   nil,
			Type:  Boolean,
			Value: false,
		}, nil
	} else if len(s) >= 2 && s[0:2] == "#t" {
		return s[2:], Value{
			Car:   nil,
			Cdr:   nil,
			Type:  Boolean,
			Value: true,
		}, nil
		// . is the marker for a cons pair, which means the thing to the right
		// is the cdr of the cons cell, not nil like it would be for a standard list
	} else if s[0] == '.' {
		return s[1:], Value{
			Car:   nil,
			Cdr:   nil,
			Type:  Nil,
			Value: nil,
		}, errors.New("unexpected '.'")
	} else if 1 == 0 {
		// reader macros

		// closing paren marks the end of a list, this is an error.
		// if its terminating a list, then read list will catch it
		// and the error wont bubble to the user.
	} else if s[0] == ')' {
		return s[1:], Value{
			Car:   nil,
			Cdr:   nil,
			Type:  Nil,
			Value: nil,
		}, errors.New("unexpected ')'")
	}
	// of none of these matched, then its a symbol, so read it in as that.
	return readSymbol(s)
}
