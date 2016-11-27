// -----------------------------------------------------------------------------
//  Operator - A symbol or phrase that processes one or more values.
//  Operand - A value targeted by an operator.
//  Unary operator - An operator that works on a single operand.
//  Unary prefix operator - An operator that appears just before its operand.
//  Unary postfix operator - An operator that appears just after its operand.
//  Binary operator - An operator that works on two operands.
//  Ternary operator - An operator that works on three operands.
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

assert(-(-(-(-2.0))) == 2.0)

// -----------------------------------------------------------------------------
//  Unary plus operator (`+`) - The operator that returns its operand as is.
// -----------------------------------------------------------------------------

let p6 = -6
let p7 = +p6
assert(p7 == -6)

assert(+(+(+(+2.0))) == 2.0)

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

// Uncommenting this leads to a runtime error:
/*
assert(2 <= 1)
*/

// -----------------------------------------------------------------------------

// Boolean values can only be compared for equality or inequality.
assert(true == true)
assert(true != false)
assert(false == false)
assert(false != true)

// Uncommenting this leads to a compile-time error:
/*
assert(true < false)
*/

// -----------------------------------------------------------------------------

// Tuples can be compared if the following are true:
// - They have the same number of values.
// - Each value in them can be compared.

// Tuples are compared from left to right, one value at a time, until two values
// are found that aren't equal.

assert((1, "zebra") < (2, "apple"))
// Works because 1 is less than 2.

assert((3, "apple") < (3, "bird"))
// Works because 3 is equal to 3 and "apple" is less than "bird".

// If all the elements are equal then the tuples themselves are equal.

assert((4, "dog") == (4, "dog"))
// Works because 4 is equal to 4 and "dog" is equal to "dog".

// -----------------------------------------------------------------------------

// Tuples with Boolean values can only be compared for equality or inequality.

assert((true, false) == (true, false))
assert((true, false) != (false, true))
assert((true, 1) == (true, 1))
assert((true, 2) != (true, 3))

// Uncommenting this leads to a compile-time error:
/*
assert((2, true) < (3, true))
*/

// -----------------------------------------------------------------------------

// Tuples that have a different number of values cannot be compared.

// Uncommenting this leads to a compile-time error:
/*
assert((1, 2) < (3, 4, 5))
*/

// -----------------------------------------------------------------------------

// Tuples having similar element types in a different order cannot be compared.

// Uncommenting this leads to a compile-time error:
/*
assert((4, "dog") == ("dog", 4))
*/

// -----------------------------------------------------------------------------

// To compare tuples with seven or more elements, you must implement the
// comparison operators yourself.

// -----------------------------------------------------------------------------
//  Ternary conditional operator (`?:`) - The operator that checks a condition
//                                        and evaluates one of two expressions
//                                        based on its result.
// -----------------------------------------------------------------------------

let p9 = false ? 5 : 2
assert(p9 == 2)

let p10 = 4 + (true ? 5 : 2)
assert(p10 == 9)

// -----------------------------------------------------------------------------
//  Nil-coalescing operator (`??`) - The operator that returns either the value
//                                   of a non-nil optional or a default value.
// -----------------------------------------------------------------------------

// The nil-coalescing operator requires its first operand be an optional and its
// second operand to be of the same type as the type that could be stored in the
// first operand.

// If the first operand is non-nil the second operand is not evaluated.
// This is an example of short-circuit evaluation.

var p11 = Int?(2)
let p12 = 4
let p13 = p11 ?? p12
assert(p13 == 2)

// -----------------------------------------------------------------------------

// If the first operand of a nil-coalescing operator is nil then the second operand.

p11 = nil
let p14 = p11 ?? p12
assert(p14 == p12)
assert(p14 == 4)

// -----------------------------------------------------------------------------

// This ternary conditional acts the same as the nil-coalescing operator.
let p15 = p11 != nil ? p11! : p12
assert(p14 == p15)

// -----------------------------------------------------------------------------
//  Range operator - An operator that expresses a range of values.
//  Closed range operator (`...`) - The range operator whose range includes the
//                                  value defining the range's upper limit.
// -----------------------------------------------------------------------------

let p16 = -11...12
assert(p16 == CountableClosedRange(-11...12))
assert(type(of: p16) == CountableClosedRange<Int>.self)
assert(p16.isEmpty == false)
assert(p16.count == 24)
assert(p16.lowerBound == -11)
assert(p16.upperBound == 12)
assert(p16.contains(6) == true)

// -----------------------------------------------------------------------------

// If a closed range's lower limit equals the upper limit the resulting range
// has only one element.

let p17 = 12...12
assert(p17.isEmpty == false)
assert(p17.count == 1)
assert(p17.contains(12) == true)
assert(p17.contains(6) == false)

// -----------------------------------------------------------------------------

// A closed range's lower limit must not be greater than its upper limit.

// Uncommenting this leads to a compile-time error:
/*
let p18 = 12...-11
*/

// -----------------------------------------------------------------------------
//  Half-open range operator (`..<`) - The range operator whose range omits the
//                                     value defining the range's upper limit.
// -----------------------------------------------------------------------------

let p19 = -11..<12
assert(p19 == CountableRange(-11..<12))
assert(type(of: p19) == CountableRange<Int>.self)
assert(p19.isEmpty == false)
assert(p19.count == 23)
assert(p19.lowerBound == -11)
assert(p19.upperBound == 12)
assert(p19.contains(6) == true)

// -----------------------------------------------------------------------------

// If a half-open range's lower limit equals the upper limit the resulting range
// is empty.

let p20 = 12..<12
assert(p20.isEmpty == true)
assert(p20.count == 0)
assert(p20.contains(12) == false)
assert(p20.contains(6) == false)

// -----------------------------------------------------------------------------

// A half-open range's lower limit must not be greater than its upper limit.

// Uncommenting this leads to a compile-time error:
/*
let p21 = 12..<(-11)
*/

// -----------------------------------------------------------------------------
//  Logical operator - An operator that works with Boolean values.
// -----------------------------------------------------------------------------
//  Logical NOT operator (`!`) - The operator that turns a true value into
//                               a false one and vice versa.
// -----------------------------------------------------------------------------

assert(!false == true)
assert(!true == false)

let p22 = false
assert(!p22 == true)
// `!p22` is read as "not p22".

let p23 = true
assert(!p23 == false)

// -----------------------------------------------------------------------------
//  Logical AND operator (`&&`) - The operator that evaluates to true if both
//                                its operands evaluate to true, otherwise it
//                                evaluates to false.
// -----------------------------------------------------------------------------

assert((false && false) == false)
assert((false && true) == false)
assert((true && false) == false)
assert((true && true) == true)

// -----------------------------------------------------------------------------

// If the first operand is false the second won't be evaluated because it's
// unnecessary since the overall expression will be false.  This is an
// example of short-circuit evaluation.

let hasDoorCode = false
let passesEyeScan = true
assert((hasDoorCode && passesEyeScan) == false)
// `passesEyeScan` was not evaluated.

// -----------------------------------------------------------------------------
//  Logical OR operator (`||`) - The operator that evaluates to true if either
//                               its operands evaluate to true, otherwise it
//                               evaluates to false.
// -----------------------------------------------------------------------------

assert((false || false) == false)
assert((false || true) == true)
assert((true || false) == true)
assert((true || true) == true)

// -----------------------------------------------------------------------------

// If the first operand is true the second won't be evaluated because it's
// unnecessary since the overall expression will be true.  This is an
// example of short-circuit evaluation.

let hasDoorKey = true
let knowsOverride = false
assert(hasDoorKey || knowsOverride == true)
// `knowsOverride` was not evaluated.

// -----------------------------------------------------------------------------
//  Compound Expressions
// -----------------------------------------------------------------------------

// Multiple logical operators can be combined to create compound expressions.

assert((hasDoorCode && passesEyeScan || hasDoorKey || knowsOverride) == true)

// The Swift logical operators `&&` and `||` are left-associative, meaning that
// compound expressions with multiple logical operators evaluate the leftmost
// subexpression first.

// -----------------------------------------------------------------------------
//  Explicit Parentheses
// -----------------------------------------------------------------------------

// Sometimes it's helpful to use parentheses when they're not strictly needed,
// to make the intention of a complex expression easier to understand.

assert(((hasDoorCode && passesEyeScan) || hasDoorKey || knowsOverride) == true)
// The parentheses surrounding `hasDoorCode && passesEyeScan` make it clear that
// those values are considered as part of a separate possible state in the
// overall logic.  The output of the compound expression is unaffected.


// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - Basic Operators
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/BasicOperators.html
// =============================================================================
