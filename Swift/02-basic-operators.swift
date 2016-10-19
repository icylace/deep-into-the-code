// -----------------------------------------------------------------------------
//  Operator - A symbol or phrase that processes one or more values.
//  Operand - A value targeted by an operator.
//  Unary operator - An operator that works on a single operand.
//  Unary prefix operator - An operator that appears just before its operand.
//  Unary postfix operator - An operator that appears just after its operand.
//  Binary operator - An operator that works on two operands.
//  Ternary operator - An operator that works on three operands.
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
//  Assignment operator (`=`) - The operator that either initializes a constant
//                              or variable, or updates a variable.
// -----------------------------------------------------------------------------

let p1 = 10
var p2 = 5
p2 = p1
assert(p2 == 10)

// -----------------------------------------------------------------------------
//  Arithmetic operator - A operator that manipulates numbers.
// -----------------------------------------------------------------------------

// The four standard arithmetic operators are available for all number types.

assert(1 + 2 == 3)              // Addition.
assert(5 - 3 == 2)              // Subtraction.
assert(2 * 3 == 6)              // Multiplication.
assert(10.0 / 2.5 == 4.0)       // Division.

// These operators do not allow values to overflow.  Overflow behavior is
// trapped and reported as an error.  However, you can opt in to value
// overflow behavior by using overflow operators.
//
// See "Advanced Operators".

// -----------------------------------------------------------------------------
//  Remainder operator (`%`) - The operator that figures how many times
//                             a number will fit inside another number
//                             and then it returns the value left over
//                             (known as the remainder).
// -----------------------------------------------------------------------------

// The remainder operator is also known as a modulo operator in other languages.
// However, its behavior in Swift for negative numbers means that it is,
// strictly speaking, a remainder rather than a modulo operation.

assert(-9 % 4 == -1)

// `a % b` and `a % -b` will always give the same answer because
// the sign of `b` is ignored.

assert(9 % 4 == 1)
assert(9 % -4 == 1)

// -----------------------------------------------------------------------------
//  Unary minus operator (`-`) - The operator that toggles the sign of a number.
// -----------------------------------------------------------------------------

let p3 = 3
let p4 = -p3
assert(p4 == -3)
let p5 = -p4
assert(p5 == 3)

// -----------------------------------------------------------------------------
//  Unary plus operator (`+`) - The operator that returns its operand as is.
// -----------------------------------------------------------------------------

let p6 = -6
let p7 = +p6
assert(p7 == -6)

// -----------------------------------------------------------------------------
//  Compound assignment operator - An operator combining assignment
//                                 with another operation.
// -----------------------------------------------------------------------------

// Example use of the addition assignment operator (`+=`).
var p8 = 1
p8 += 2
assert(p8 == 3)

// For more compound assignment operators from the Swift Standard Library, see:
// https://developer.apple.com/reference/swift/1851035-swift_standard_library_operators

// -----------------------------------------------------------------------------
//  Comparison operator - A binary operator that figures out the
//                        relationship between its operands.
// -----------------------------------------------------------------------------

// Each of the comparison operators returns a Boolean value to indicate whether
// or not the statement is true.

assert(1 == 1)        // Equal to.
assert(2 != 1)        // Not equal to.
assert(2 > 1)         // Greater than.
assert(1 < 2)         // Less than.
assert(1 >= 1)        // Greater than or equal to.
assert(1 <= 2)        // Less than or equal to.

// If the following is uncommented it will produce a runtime error:
/*
assert(2 <= 1)
*/














// TODO




/*


// TODO
// - can tuples be added together?



// You can also compare tuples that have the same number of values, as long
// as each of the values in the tuple can be compared.

// For example, both Int
// and String can be compared, which means tuples of the type (Int, String)
// can be compared. In contrast, Bool can’t be compared, which means tuples
// that contain a Boolean value can’t be compared.

// Tuples are compared from left to right, one value at a time, until the
// comparison finds two values that aren’t equal. If all the elements are
// equal, then the tuples themselves are equal. For example:

(1, "zebra") < (2, "apple")   // true because 1 is less than 2
(3, "apple") < (3, "bird")    // true because 3 is equal to 3, and "apple" is less than "bird"
(4, "dog") == (4, "dog")      // true because 4 is equal to 4, and "dog" is equal to "dog"

// To compare tuples with seven or more elements, you must implement the
// comparison operators yourself.

// -----------------------------------------------------------------------------
//  Ternary conditional operator (`?:`) - The operator that checks a condition
//                                        and evaluates one of two expressions
//                                        based on its result.
// -----------------------------------------------------------------------------

let contentHeight = 40
let hasHeader = true
let rowHeight = contentHeight + (hasHeader ? 50 : 20)
assert(rowHeight == 90)







// -----------------------------------------------------------------------------
//  Nil-coalescing operator (`??`) - The operator that returns either the value
//                                   of a non-nil optional or a default value.
// -----------------------------------------------------------------------------

// `(a ?? b)`
// The expression a is always of an optional type.
// The expression b must match the type that is stored inside a.

// The nil-coalescing operator is shorthand for the code below:

a != nil ? a! : b


The code above uses the ternary conditional operator and forced unwrapping (a!)
to access the value wrapped inside a when a is not nil, and to return b
otherwise. The nil-coalescing operator provides a more elegant way to
encapsulate this conditional checking and unwrapping in a concise and
readable form.

NOTE

If the value of a is non-nil, the value of b is not evaluated. This is an example of
short-circuit evaluation.

let defaultColorName = "red"
var userDefinedColorName: String?   // defaults to nil

var colorNameToUse = userDefinedColorName ?? defaultColorName
// userDefinedColorName is nil, so colorNameToUse is set to the default of "red"

If you assign a non-nil value to userDefinedColorName and perform the nil-coalescing
operator check again, the value wrapped inside userDefinedColorName is used instead of the default:

userDefinedColorName = "green"
colorNameToUse = userDefinedColorName ?? defaultColorName
// userDefinedColorName is not nil, so colorNameToUse is set to "green"












// -----------------------------------------------------------------------------
//  Range operator - An operator that expresses a range of values.
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
//  Closed range operator (`...`) - The range operator whose range includes the
//                                  value defining the range's upper-limit.
// -----------------------------------------------------------------------------

let r1 = -11...12
assert(r1 == -11..<13)

// A closed range's lower-limit must not be greater than its upper-limit.

// -----------------------------------------------------------------------------
//  Half-open range operator (`..<`) - The range operator whose range omits the
//                                     value defining the range's upper-limit.
// -----------------------------------------------------------------------------

let r2 = -11..<13
assert(r1 == -11...12)

// A half-open range's lower-limit must not be greater than its upper-limit.
// However, if they are equal the resulting range is empty.

// -----------------------------------------------------------------------------
//  Logical operator - An operator that modifies or combines the
//                     Boolean logic values true and false.
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
//  Logical NOT operator (`!`) - The operator that turns a true value into
//                               a false one and vice versa.
// -----------------------------------------------------------------------------

let allowedEntry = false
assert(!allowedEntry == true)

// `!x` is read as "not x".  So, `!allowedEntry` can be read as
// "not allowed entry".

// -----------------------------------------------------------------------------
//  Logical NOT operator (`&&`) - The operator that evaluates to true if both
//                                its operands evaluate to true, otherwise it
//                                evaluates to false.
// -----------------------------------------------------------------------------

let enteredDoorCode = true
let passedRetinaScan = false
assert(enteredDoorCode && passedRetinaScan == false)

// If the first operand is false the second won't be evaluated because it's
// unnecessary since the overall expression will be false.  This is known
// as short-circuit evaluation.

// -----------------------------------------------------------------------------
//  Logical OR operator (`||`) - The operator that evaluates to true if either
//                               its operands evaluate to true, otherwise it
//                               evaluates to false.
// -----------------------------------------------------------------------------

// If the first operand is true the second won't be evaluated because it's
// unnecessary since the overall expression will be true.  This is also an
// example of short-circuit evaluation.

let hasDoorKey = false
let knowsOverridePassword = true
assert(hasDoorKey || knowsOverridePassword == true)

// -----------------------------------------------------------------------------
//  Combining Logical Operators
// -----------------------------------------------------------------------------

// You can combine multiple logical operators to create longer
// compound expressions.

assert(enteredDoorCode && passedRetinaScan || hasDoorKey || knowsOverridePassword == true)



NOTE

The Swift logical operators && and || are left-associative, meaning that
compound expressions with multiple logical operators evaluate the leftmost subexpression first.









// -----------------------------------------------------------------------------

Explicit Parentheses

It is sometimes useful to include parentheses when they are not strictly needed, to make the intention of a complex expression easier to read. In the door access example above, it is useful to add parentheses around the first part of the compound expression to make its intent explicit:

if (enteredDoorCode && passedRetinaScan) || hasDoorKey || knowsOverridePassword {
  print("Welcome!")
} else {
  print("ACCESS DENIED")
}
// Prints "Welcome!"


// The parentheses make it clear that the first two values are considered as
// part of a separate possible state in the overall logic. The output of the
// compound expression doesn’t change, but the overall intention is clearer to the reader. Readability is always preferred over brevity; use parentheses where they help to make your intentions clear.


*/


// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - Basic Operators
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/BasicOperators.html
// =============================================================================
