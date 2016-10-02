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

assert(true)        // Single-line comments can be placed after statements.

assert(true)        /* Block comments can be placed after statements. */

/* Unlike single-line comments, */assert(true)
assert/* block comments can */(true)
assert(/* be placed in weird places */true)
assert(true/* but please don't do that! */)

// Note: `assert()` is covered in a later section.


// =============================================================================
//  The Basics
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html
// =============================================================================

// -----------------------------------------------------------------------------
//  Literal value (literal) - A value that appears directly in the code as is.
// -----------------------------------------------------------------------------

// Using literals only by themselves will produce compile-time warnings which
// is why the following lines are commented out.
/*
42          // An integer literal.
"hi"        // A string literal.
3.0         // A floating-point number literal.
*/

// -----------------------------------------------------------------------------
//  Constant - An association between a name and a value that is immutable,
//             meaning it can only be assigned a value once.
// -----------------------------------------------------------------------------

// Declare a constant named `answer` and assign it the literal value 42.
let answer = 42

// -----------------------------------------------------------------------------
//  Variable - An association between a name and a value that is mutable,
//             meaning it can be assigned another value anytime.
// -----------------------------------------------------------------------------

// A variable named `myAnswer`.
var myAnswer = 42

// -----------------------------------------------------------------------------
//  Type - The kind of data that can be processed.
//  Type annotation - A label for the type a constant or variable can hold.
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
// there's nothing to infer the type from.

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
//  Assertion - A runtime check that a given condition evaluates to true.
// -----------------------------------------------------------------------------

// If an assertion is triggered (its condition evaluates to false), cod
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

// If an assertion is triggered while running in a debug environment,
// such as within Xcode, you can see exactly where the invalid state
// occurred and query the state of your app at the time that the
// assertion was triggered.

// Assertions are disabled when your code is compiled with optimizations,
// such as when building with an app target's default Release
// configuration in Xcode.

// -----------------------------------------------------------------------------
//  Integer - A whole number with no fractional component that is either signed
//            (positive, zero, or negative) or unsigned (positive or zero).
// -----------------------------------------------------------------------------

// Integer types can be either signed or unsigned in 8, 16, 32,
// and 64-bit forms.

var i1: Int         // A signed integer with default bit form.
var i2: Int8        // An 8-bit signed integer.
var i3: Int16       // A 16-bit signed integer.
var i4: Int32       // A 32-bit signed integer.
var i5: Int64       // A 64-bit signed integer.

var i6: UInt          // An unsigned integer with default bit form.
var i7: UInt8         // An 8-bit unsigned integer.
var i8: UInt16        // A 16-bit unsigned integer.
var i9: UInt32        // A 32-bit unsigned integer.
var i10: UInt64       // A 64-bit unsigned integer.

// In most cases you don't need to pick a specific size of integer to use.
// `Int` and `UInt` has the same size as the current platform's native
// word size:
// - On a 32-bit platform, `Int` is the same size as `Int32` and
//   `UInt` is the same size as `UInt32`.
// - On a 64-bit platform, `Int` is the same size as `Int64` and
//   `UInt` is the same size as `UInt64`.

// -----------------------------------------------------------------------------

// The minimum and maximum values of each integer type are accessed
// through their `min` and `max` properties.

let minValue = UInt8.min
// `minValue` is inferred to be of type `UInt8`.
assert(minValue == 0)

let maxValue = UInt8.max
// `maxValue` is inferred to be of type `UInt8`.
assert(maxValue == 255)

// -----------------------------------------------------------------------------
//  Floating-point number - A number with a fractional component.
// -----------------------------------------------------------------------------

// Floating-point types can represent a much wider range of values than
// integer types and can store numbers that are much larger or smaller
// than can be stored in an integer.

// There are no unsigned floating-point types but there are three signed ones:
// - `Float` (a.k.a. `Float32`) represents a 32-bit single-precision
//   floating-point number and can have a precision as little as 6
//   decimal digits.
// - `Double` (a.k.a. `Float64`) represents a 64-bit double-precision
//   floating-point number and has a precision of at least 15
//   decimal digits.
// - `Float80` represents an 80-bit extended-precision floating-point number.

let pi = 3.14159
// `pi` is inferred to be of type `Double`.

// If you combine integer and floating-point literals in an expression,
// a type of `Double` will be inferred from the context.

let myPi = 3 + 0.14159
// `myPi` is inferred to be of type `Double`.

assert(pi == myPi)

// -----------------------------------------------------------------------------
//  Numeric literal - A literal value that represents a number.
// -----------------------------------------------------------------------------

// An integer literal can be represented in...
var l = 17                  // ...decimal notation.
assert(l == 0b10001)        // ...binary notation.
assert(l == 0o21)           // ...octal notation.
assert(l == 0x11)           // ...hexadecimal notation.

// -----------------------------------------------------------------------------

// A floating-point literal...
                          // ...cannot have its decimal point be its first
                          //    or last character.
var n = 3.0               // ...can be represented in decimal notation.
assert(n == 0x3p0)        // ...can be represented in hexadecimal notation.

// Decimal floats can have an exponent.  For those with an exponent of x,
// the base number is multiplied by 10^x (10 raised to the x power).

// This is 1.25 x 10^2.
n = 1.25e2
assert(n == 1.25E2)
assert(n == 125.0)

// This is 1.25 x 10^-2.
n = 1.25e-2
assert(n == 1.25E-2)
assert(n == 0.0125)

// -----------------------------------------------------------------------------

// Hexadecimal floats must have an exponent.  For those with an exponent of x,
// the base number is multiplied by 2^x.

// This equals 15 x 2^2 which equals 60.0.
n = 0xFp2
assert(n == 0xFP2)
assert(n == 60.0)

// This equals 15 x 2^-2 which equals 3.75.
n = 0xFp-2
assert(n == 0xFP-2)
assert(n == 3.75)

n = 12.1875
assert(n == 1.21875e1)
assert(n == 0xC.3p0)

// -----------------------------------------------------------------------------

// Numeric literals can contain extra formatting to make them easier to read.
// Both integers and floats can be padded with extra zeros and can contain
// underscores.  Neither type of formatting affects the underlying value
// of the literal.

n = 000123.456
l = 1_000_000
n = 1_000_000.000_000_1
n = 10_.0
l = 2___3
l = 0b1_010_10_101
l = 0o34__53
l = 0xFEED_FACE___
n = 0xFEED.FACE___p1_
n = 0x0000000FEED.FACE___p1_

// Underscores cannot be put directly after the prefix for binary, octal, and
// hexadecimal numbers.  Attempting to do so results in a compile-time error.

// If the following is uncommented it will produce an error:
/*
l = 0b_01_010_10_101
l = 0o_34__53
l = 0x_FEED_FACE___
n = 0x_FEED.FACE___p1_
*/

// -----------------------------------------------------------------------------
//  Numeric Type Conversion
// -----------------------------------------------------------------------------

// The range of numbers that can be stored in an integer constant or
// variable is different for each numeric type.  A number that will
// not fit into a constant or variable of a sized integer type is
// flagged as a compile-time error:

// `UInt8` cannot store negative numbers.

// If the following is uncommented it will produce an error:
/*
let cannotBeNegative: UInt8 = -1
*/

// `Int8` cannot store a number larger than its maximum value.

// If the following is uncommented it will produce an error:
/*
let tooBig: Int8 = Int8.max + 1
*/

// -----------------------------------------------------------------------------

// Conversion is necessary for us to work with values of different types.

// Because each numeric type can store a different range of values, you must
// opt in to numeric type conversion on a case-by-case basis.  This approach
// prevents hidden conversion errors and helps make type conversion
// intentions explicit.

let twoThousand: UInt16 = 2_000
let one: UInt8 = 1

// To convert one specific number type to another, you initialize a new number
// of the desired type with the existing value.

// The conversion here is possible because `UInt16` has an initializer that
// accepts a `Uint8` value.
let twoThousandAndOne = twoThousand + UInt16(one)
// `twoThousandAndOne` will be inferred to be of type `UInt`.















// TODO

// -----------------------------------------------------------------------------

let three = 3
let pointOneFourOneFiveNine = 0.14159
let myPi2 = Double(three) + pointOneFourOneFiveNine
// `myPi2` is inferred to be of type `Double`.
assert(myPi2 == 3.14159)

// Floating-point values are always truncated when used to initialize
// a new integer value.

let integerPi = Int(myPi2)
assert(integerPi == 3)

let floatAsInt1 = Int(4.75)
assert(floatAsInt1 == 4)

let floatAsInt2 = Int(-3.9)
assert(floatAsInt2 == -3)

// The rules for combining numeric constants and variables are different from
// the rules for numeric literals.  The literal value 3 can be added directly
// to the literal value 0.14159 because number literals do not have an
// explicit type in and of themselves.  Their type is inferred only at
// the point that they are evaluated by the compiler.

// -----------------------------------------------------------------------------
//  Type alias - An alternative name for an existing type.
// -----------------------------------------------------------------------------

// Type aliases are useful when you want to refer to an existing type by a name
// that is contextually more appropriate, such as when working with data of a
// specific size from an external source.

typealias AudioSample = UInt16

// Once you define a type alias you can use the alias anywhere you might
// use the original name.

var maxAmplitudeFound = AudioSample.min
assert(maxAmplitudeFound == 0)

// -----------------------------------------------------------------------------
//  Boolean - A value that can only ever be true or false.
// -----------------------------------------------------------------------------

let orangesAreOrange = true
let todayIsYesterday = false

// -----------------------------------------------------------------------------
//  Tuple - A compound value that groups together multiple values.
// -----------------------------------------------------------------------------

// The values within a tuple can be of any type and do not have to be of the
// same type as each other.  For example, `(Int, Int, Int)` or `(String,
// Bool)` or any other permutation you require.

var httpStatus = (404, "Not Found")
// `httpStatus` is of type `(Int, String)`, and equals `(404, "Not Found")`

// -----------------------------------------------------------------------------

// You can decompose a tuple's contents into separate constants or variables
// which you then access as usual.

let (code, message) = httpStatus
assert(code == 404)
assert(message == "Not Found")

// -----------------------------------------------------------------------------

// If you only need some of the tuple's values ignore the unwanted parts with
// an underscore (`_`) during decomposition.

let (statusCode, _) = httpStatus
assert(statusCode == 404)

// -----------------------------------------------------------------------------

// Alternatively, access the individual element values in a tuple using index
// numbers starting at zero.

assert(httpStatus.0 == 404)
assert(httpStatus.1 == "Not Found")

// -----------------------------------------------------------------------------

// You can name the individual elements in a tuple when the tuple is defined which
// you can use to access the values of those elements.

let httpOkay = (code: 200, description: "OK")
assert(httpOkay.code == 200)
assert(httpOkay.description == "OK")

// -----------------------------------------------------------------------------
//  Optional - A value having either an underlying value of a particular type
//             or the lack of any value at all.
// -----------------------------------------------------------------------------

// You use optionals in situations where a value might be absent.

let possibleNumber = "123"
let convertedNumber = Int(possibleNumber)
// `convertedNumber` is inferred to be of type `Int?`, or "optional `Int`",
// because `Int` can accept a `String` that may or may not represent
// a number.

// -----------------------------------------------------------------------------

// An optional variable can be in a valueless state by assigning it the
// special value nil.

var responseCode: Int? = 404
// `responseCode` contains an actual `Int` value of 404.
responseCode = nil
// `responseCode` now contains no value.

// An optional variable defined without an explicit default value will
// be set to nil.

var surveyAnswer: String?
assert(surveyAnswer == nil)

// Another way to write an optional using generator syntax.

var thisAnswer: Optional<String>
assert(thisAnswer == nil)

// nil cannot be used with nonoptional constants and variables.




// TODO
// Does this work?
var x: String?



// -----------------------------------------------------------------------------
//  Forced unwrapping - An attempt at accessing an optional's underlying value.
// -----------------------------------------------------------------------------

let possibleString: String? = "An optional string."

// We know that `possibleString` has a string, so it's safe to force-unwrap it.
let forcedString: String = possibleString!
assert(forcedString == "An optional string.")

// Trying to use `!` to access a nonexistent optional value triggers a runtime
// error.

// -----------------------------------------------------------------------------
//  Implicitly unwrapped optional - An optional treated as if it has a value.
// -----------------------------------------------------------------------------

// Implicitly unwrapped optionals are useful when an optional's value is
// confirmed to exist immediately after the optional is first defined
// and can definitely be assumed to exist at every point thereafter.
// The primary use of implicitly unwrapped optionals in Swift is
// during class initialization.

// An implicitly unwrapped optional is a normal optional behind the scenes, but
// can also be used like a nonoptional value, without the need to unwrap the
// optional each time it is accessed.

let assumedString: String! = "An implicitly unwrapped optional string."
let implicitString: String = assumedString

// You can think of an implicitly unwrapped optional as giving permission for
// the optional to be unwrapped automatically whenever it is used.

// If an implicitly unwrapped optional is nil and you try to access its wrapped
// value, you'll trigger a runtime error.  The result is exactly the same as if
// you force-unwrap a normal optional that doesn't contain a value.
















// -----------------------------------------------------------------------------
//  Semicolon - The symbol that separates statements on a line.
// -----------------------------------------------------------------------------

// Semicolons are...
// ...required when multiple statements are on the same line.

let i = 1; var j = 2

// ...optional when only a single statement is on the line.

let g = 1
var h = 2;
