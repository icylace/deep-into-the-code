// This is a single-line comment.

// -----------------------------------------------------------------------------
//  Comment - Text meant to be read by developers and ignored by the compiler.
//  Single-line comment - A comment that cannot be on more than one line.
//  Block comment - A comment that can be on more than one consecutive lines.
// -----------------------------------------------------------------------------

// Comments serve to allow you to have helpful text with your code.

/* Block comments are
usually seen spanning
multiple lines. */

/* Though, block comments can be used like single-line comments. */

/* Also, block comments can be */  /* placed next to each other. */

/* Not that you
would *//* want
to do that. */

/**
 * Block comments that span multiple lines
 * are usually formatted a bit better.
 */

/**
 * Also, block comments...
 *
 * /* ...may be nested. */
 *
 */

      // Single-line comments don't need to start at the beginning of the line.

      /* Block comments don't need to start at the beginning of the line. */

// -----------------------------------------------------------------------------
//  Statement - The smallest standalone element of a language that
//              expresses some task to execute.
// -----------------------------------------------------------------------------

assert(true)        // Single-line comments can be placed after statements.

assert(true)        /* Block comments can be placed after statements. */

/* Unlike single-line comments, */assert(true)
assert/* block comments can */(true)
assert(/* be placed in weird places */true)
assert(true/* but please don't do that! */)

// Note: `assert(_:_:file:line:)` is covered in a later section.


// =============================================================================
//  The Basics
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html
// =============================================================================

// -----------------------------------------------------------------------------
//  Expression - A group of one or more language elements that gets evaluated.
//  Evaluation - The process by which an expression is computed.
//  Type - The kind of data that can be processed.
//  Value - A typed expression that cannot be evaluated any further.
//  Literal value (literal) - A value appearing in the code as is.
// -----------------------------------------------------------------------------

// Using literals only by themselves will produce compile-time warnings which
// is why the following lines are commented out.
/*
42          // An integer literal.
"hi"        // A string literal.
3.0         // A floating-point number literal.
*/

// -----------------------------------------------------------------------------
//  Immutability - The quality of a value staying unchanged once it is set.
//  Constant - An association between a name and an immutable value.
// -----------------------------------------------------------------------------

// Declare a constant named `answer` and assign it the literal value 42.
let answer = 42

// -----------------------------------------------------------------------------
//  Mutability - The quality of a value staying changeable once it is set.
//  Variable - An association between a name and a mutable value.
// -----------------------------------------------------------------------------

// A variable named `myAnswer`.
var myAnswer = 42

// -----------------------------------------------------------------------------
//  Type annotation - A label for type of value a constant or variable can hold.
// -----------------------------------------------------------------------------

// Declare a constant named `theAnswer` that is of type `Int` (integer) and
// assign it the literal value 42.
let theAnswer: Int = 42

// -----------------------------------------------------------------------------
//  Type inference - The behavior that determines type given the values in use.
// -----------------------------------------------------------------------------

// If there's no explicit type annotation the type of the constant or variable
// is inferred from its initial assigned value.

var k = "hello"
// `k` is inferred to be of type `String`.

// -----------------------------------------------------------------------------
//  Type safety - The behavior that enforces consistent use of types.
// -----------------------------------------------------------------------------

// Swift is a type-safe language.  Type safety encourages you to be clear about
// the types of values your code can work with.  Type checks happen as part of
// the compilation process and any type mismatches are flagged as
// compile-time errors.

// Since `k` is of type `String` we can only `String` values to it from now on.
k = "bonjour"

// If the following is uncommented it will produce an error:
/*
k = 2
*/

// -----------------------------------------------------------------------------

// Type annotation is required if there's no initial assignment because
// there's no value to infer the type from.

let lateAnswer: Int
lateAnswer = 42

var hello: String
hello = "hi"
hello = "hello"

// -----------------------------------------------------------------------------

// Multiple constants or multiple variables can be declared on a single line...

// ...with inferred types.
let a = 0, b = 1, c = true
var d = 3, e = 4, f = ""

// ...with the same explicit type.
let first, second, third: Int
var red, green, blue: Double

// ...with different explicit types.
let uno: String, dos, tres: Int
var quatro, cinco: Double, seis: Int
var un: Double, deux: Int, trois: Double

// -----------------------------------------------------------------------------

// Constant and variable names can contain almost any character,
// including Unicode characters.

let π = 3.14159
let 你好 = "你好世界"
let 🐶🐮 = "dogcow"

// Constant and variable names cannot contain:
// - whitespace characters
// - mathematical symbols
// - arrows
// - private-use (or invalid) Unicode code points
// - line-drawing characters
// - box-drawing characters

// Constant and variable names cannot begin with a number, although numbers may
// be included elsewhere within the name.

// -----------------------------------------------------------------------------

// If you need to give a constant or variable the same name as a reserved Swift
// keyword, surround the keyword with backticks (`) when using it as a name.

let `let` = "i'm a constant"
assert(`let` == "i'm a constant")

// Backticks can also be used with constant or variable names that would
// otherwise be valid but it's pointless.

var `x2` = 112
assert(x2 == 112)










// TODO
// - backticks are for all identifiers, correct ?
class `var` {}


// -----------------------------------------------------------------------------

// You can print the current value of a constant or variable with the
// global `print(_:separator:terminator:)` function:
// - https://developer.apple.com/reference/swift/1541053-print

print(answer)
// Output:
// 42

// -----------------------------------------------------------------------------
//  Semicolon - The symbol that separates statements on a line.
// -----------------------------------------------------------------------------

// Semicolons are...
// ...required when multiple statements are on the same line.

let i = 1; var j = 2

// ...optional when only a single statement is on the line.

let g = 1
var h = 2;

// -----------------------------------------------------------------------------
//  Assertion - A runtime check that a given condition evaluates to true.
// -----------------------------------------------------------------------------

// If an assertion is triggered (its condition evaluates to false), code
// execution ends and your app is terminated.

// An assertion is checked with the global `assert(_:_:file:line:)` function:
// - https://developer.apple.com/reference/swift/1541112-assert

let age = 3
// This assertion is not triggered because `age` equals 3.
assert(age == 3)

// If the following is uncommented it will produce an error:
/*
// This causes the assertion to trigger, because `age` is not greater than 5.
assert(age > 5)
*/

// `assert(_:_:file:line:)` also lets you provide a suitable debug message that
// is displayed if the assertion is triggered.

// If the following is uncommented it will produce an error:
/*
// assert(age > 5, "Age needs to be greater than 5.")
*/

// -----------------------------------------------------------------------------

// If an assertion is triggered while running in a debug environment, such as
// within Xcode, you can see exactly where the invalid state occurred and
// query the state of your app at the time that the assertion
// was triggered.

// Assertions are disabled when your code is compiled with optimizations,
// such as when building with an app target's default Release
// configuration in Xcode.
