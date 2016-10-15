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

      /* Neither do block comments. */

// -----------------------------------------------------------------------------
//  Statement - The smallest standalone element of a language that
//              expresses some task to execute.
// -----------------------------------------------------------------------------

assert(true)        // Single-line comments can be placed after statements.

assert(true)        /* So can block comments. */

/* Unlike single-line comments, */assert(true)
assert/* block comments can */(true)
assert(/* be placed in weird places */true)
assert(true/* but please don't do that! */)

// -----------------------------------------------------------------------------
//  Assertion - A runtime check verifying that a given condition evaluates to
//              true and if it evaluates to false (i.e. gets triggererd),
//              code execution ends and your app is terminated.
// -----------------------------------------------------------------------------

// An assertion is checked with the `assert(_:_:file:line:)` global function:
// https://developer.apple.com/reference/swift/1541112-assert

// This assertion is not triggered because `age` equals 3.
assert(3 < 5)

// If the following is uncommented it will produce a runtime error:
/*
assert(3 > 5)
*/

// -----------------------------------------------------------------------------

// If an assertion is triggered while running in a debug environment, such
// as within Xcode, you can see exactly where the invalid state occurred
// and query the state of your app at the time of the triggering.

// Assertions are disabled when your code is compiled with optimizations, such
// as when building with an app target's default Xcode Release configuration.

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
//  Variable - An association between a name and a value that is mutable,
//             meaning it always can be assigned another value anytime.
// -----------------------------------------------------------------------------

// Declare a variable named `answer1` and assign it the literal value 42.
var answer1 = 42

// Reassign `answer1` with the value 123.
answer1 = 123

// -----------------------------------------------------------------------------
//  Constant - An association between a name and a value that is immutable,
//             meaning it can only be assigned a value once.
// -----------------------------------------------------------------------------

// Declare a constant named `answer2` and assign it the literal value 42.
let answer2 = 42

// If the following is uncommented it will produce a compile-time error:
/*
answer2 = 0
*/

// -----------------------------------------------------------------------------
//  Type annotation - A label for type of value a constant or variable can hold.
// -----------------------------------------------------------------------------

// Declare a constant named `answer3` that is of type `Int` (integer) and
// assign it the literal value 42.
let answer3: Int = 42

// -----------------------------------------------------------------------------
//  Type inference - The behavior that determines type given the values in use.
// -----------------------------------------------------------------------------

// If there's no explicit type annotation the type of the constant or variable
// is inferred from its initial assigned value.

var answer4 = 42
// `answer4` is inferred to be of type `Int`.

// -----------------------------------------------------------------------------

// You can use the `type(of:)` global function to get the type of an expression:
// https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Types.html#//apple_ref/doc/uid/TP40014097-CH31-ID455

// A type's `self` property can be used to refer to the type itself.

// Verify that `answer4` is of type `Int`.
assert(type(of: answer4) == Int.self)

// -----------------------------------------------------------------------------

// You can use initializer syntax to create a new value of a certain type.

answer4 = Int()
assert(answer4 == 0)
answer4 = Int(42)
assert(answer4 == 42)

// -----------------------------------------------------------------------------
//  Type safety - The behavior that enforces consistent use of types.
// -----------------------------------------------------------------------------

// Type checks happen as part of the compilation process and any type mismatches
// are flagged as compile-time errors.

// Since `answer4` is of type `Int` we can only assign `Int` values
// to it from now on.
answer4 = 16

// If the following is uncommented it will produce a compile-time error:
/*
// Attempt to assign a `String` value to `answer4`.
answer4 = "hello"
*/

// -----------------------------------------------------------------------------

// Type annotation is required if there's no initial assignment because
// there's no value to infer the type from.

let myAnswer: Int
myAnswer = 42

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
let uno: String, dos, tres: Bool
var quatro, cinco: Double, seis: Int
var un: Double, deux: Int, trois: Double

// -----------------------------------------------------------------------------
//  Identifier - A name given to a constant, variable, function, type, protocol,
//               custom operator, or label.
// -----------------------------------------------------------------------------

// Identifiers can contain almost any character including Unicode characters.

let π = 3.14159
let 你好 = "你好世界"
var 🐶🐮 = "dogcow"

// Identifiers cannot contain:
// - whitespace characters
// - mathematical symbols
// - arrows
// - private-use (or invalid) Unicode code points
// - line-drawing characters
// - box-drawing characters
// - numbers at the beginning but numbers may be included elsewhere.

// -----------------------------------------------------------------------------

// Any Swift keyword can be an identifier by surrounding it with backticks (`).

let `let` = "i'm a constant"
assert(`let` == "i'm a constant")

// Backticks can be used with normal identifiers but it's pointless.

var `x1` = 112
assert(x1 == 112)

// Backticks used in this way can't be nested.

// If the following is uncommented it will produce a runtime error:
/*
var ``x2`` = 212
assert(x2 == 212)

var ```x3``` = 143
assert(x3 == 143)
*/

// -----------------------------------------------------------------------------

// You can print the current value of a constant or variable with the
// `print(_:separator:terminator:)` global function:
// https://developer.apple.com/reference/swift/1541053-print

print(answer2)
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
