// =============================================================================
//  The Basics
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html
// =============================================================================


// -----------------------------------------------------------------------------
//  Constant - An association between a name and a value of a specific type that
//             is immutable, meaning it can only be assigned a value once.
//  Variable - An association between a name and a value of a specific type that
//             is mutable, meaning it can be assigned another value anytime.
//  Literal value (literal) - A value that appears directly in the code as is.
// -----------------------------------------------------------------------------

// Declare a constant named `answer` and assign it the literal value 42.
let answer = 42

// A variable named `myAnswer`.
var myAnswer = 42

// Multiple constants or multiple variables can be declared on a single line.

let a = 0, b = 1, c = 2
var x = 3, y = 4, z = 5


// -----------------------------------------------------------------------------
//  Type annotation - A label that describes the kind of value a constant
//                    or variable can hold.
// -----------------------------------------------------------------------------

// TODO

// Define a constant named `theAnswer` that is of type `Int` (integer) and
// assign it the value 42.
let theAnswer: Int = 42

// If no type annotation is provided the type of the constant or variable
// is inferred from its initial assigned value.

var hello = "hello"
// `hello` is of type `String`.

// Type annotation is required if there's no initial assignment.

let lateAnswer: Int
lateAnswer = 42

var greeting: String
greeting = "hi"
greeting = "hello"

// Multiple constants or multiple variables of the same type can be
// declared on a single line.

let one, two, three: Int
var red, green, blue: Double

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

// -----------------------------------------------------------------------------

// You can print the current value of a constant or variable with the
// global `print(_:separator:terminator:)` function from the Swift
// Standard Library.

print(answer)
// Prints "42".


// -----------------------------------------------------------------------------
//  Comment - Text meant to be read by developers and ignored by the compiler.
// -----------------------------------------------------------------------------

// As you've probably already noticed, single-line comments begin with double
// forward slashes.  Comments serve to allow you to have helpful text with
// your code.

/* Here is a block comment.
Block comments can span
multiple lines. */

/* Block comments can be used like single-line comments. */

/* Block comments can be *//* placed next to each other. */

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

print(0)        // We can put single-line comments at the end of a line.

print(1)        /* Same goes for block comments. */

/* Unlike single-line comments, */print(2)
print/* block comments can */(3)
print(/* be placed in weird places */4)
print(5/* but please don't do that! */)


// -----------------------------------------------------------------------------
//  Semicolon - The special symbol that separates statements on a line.
// -----------------------------------------------------------------------------

// Semicolons are optional when only a single statement is on the line.
let p = 1
var q = 2;

// Semicolons are required when multiple statements are on the same line.
let r = 1; var s = 2


// -----------------------------------------------------------------------------
//  Integer - A whole number with no fractional component and is either signed
//            (positive, zero, or negative) or unsigned (positive or zero).
// -----------------------------------------------------------------------------

// Integer types can be either signed or unsigned in 8, 16, 32, and 64 bit forms.

var i1: Int = 0         // A signed integer with default bit form.
var i2: Int8 = 0        // An 8-bit signed integer.
var i3: Int16 = 0       // A 16-bit signed integer.
var i4: Int32 = 0       // A 32-bit signed integer.
var i5: Int64 = 0       // A 64-bit signed integer.

var i6: UInt = 0          // An unsigned integer with default bit form.
var i7: UInt8 = 0         // An 8-bit unsigned integer.
var i8: UInt16 = 0        // A 16-bit unsigned integer.
var i9: UInt32 = 0        // A 32-bit unsigned integer.
var i10: UInt64 = 0       // A 64-bit unsigned integer.

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
let minValue = UInt8.min    // `minValue` is set to 0, and is of type `UInt8`.
let maxValue = UInt8.max    // `maxValue` is set to 255, and is of type `UInt8`.


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


// -----------------------------------------------------------------------------
//  Type Safety and Type Inference
// -----------------------------------------------------------------------------

// Swift is a type-safe language.  Type safety encourages you to be clear about
// the types of values your code can work with.  If part of your code expects
// a `String`, you can't use an `Int` with it by mistake.

// Type checks happen as part of the compilation process and any type
// mismatches are flagged as compile-time errors.

// Type inference is used to determine the type of something if it isn't
// specified explicitly.  This is done by examining the values in use.

let meaningOfLife = 42
// `meaningOfLife` is inferred to be of type `Int`.

let pi = 3.14159
// `pi` is inferred to be of type `Double`.

// If you combine integer and floating-point literals in an expression,
// a type of `Double` will be inferred from the context.
let anotherPi = 3 + 0.14159
// `anotherPi` is also inferred to be of type `Double`.


// -----------------------------------------------------------------------------
//  Numeric literal - A literal value that represents a number.
// -----------------------------------------------------------------------------

let decimalInteger = 17
let binaryInteger = 0b10001       // 17 in binary notation.
let octalInteger = 0o21           // 17 in octal notation.
let hexadecimalInteger = 0x11     // 17 in hexadecimal notation.

// -----------------------------------------------------------------------------

// Floating-point literals can be decimal (with no prefix) or hexadecimal (with
// a `0x` prefix).  They must always have a decimal or hexadecimal number on
// both sides of the decimal point.  Decimal floats can also have an optional
// exponent, indicated by an uppercase or lowercase `e`; hexadecimal floats
// must have an exponent, indicated by an uppercase or lowercase `p`.

// For decimal numbers with an exponent of exp, the base number
// is multiplied by 10^exp (10 raised to the exp power).

let n1 = 1.25e2         // This equals 1.25 x 10^2 which equals 125.0.
let n2 = 1.25e-2        // This equals 1.25 x 10^-2 which equals 0.0125.

// For hexadecimal numbers with an exponent of exp, the base number
// is multiplied by 2^exp.

let n3 = 0xFp2        // This equals 15 x 2^2 which equals 60.0.
let n4 = 0xFp-2       // This equals 15 x 2^-2 which equals 3.75.

// These are all equal:
let decimalDouble = 12.1875
let exponentDouble = 1.21875e1
let hexadecimalDouble = 0xC.3p0

// -----------------------------------------------------------------------------

// Numeric literals can contain extra formatting to make them easier to read.
// Both integers and floats can be padded with extra zeros and can contain
// underscores to help with readability.  Neither type of formatting
// affects the underlying value of the literal.

let n5 = 000123.456
let n6 = 1_000_000
let n7 = 1_000_000.000_000_1
let n8 = 10_.0
let n9 = 2___3
let n10 = 0b1_010_10_101
let n11 = 0o34__53
let n12 = 0xFEED_FACE___
let n13 = 0xFEED.FACE___p1_

// Underscores cannot be put directly after the prefix for binary, octal, and
// hexadecimal numbers.  Attempting to do so results in a compile-time error.
// let n14 = 0b_01_010_10_101
// let n15 = 0o_34__53
// let n16 = 0x_FEED_FACE___
// let n17 = 0x_FEED.FACE___p1_


// -----------------------------------------------------------------------------
//  Numeric Type Conversion
// -----------------------------------------------------------------------------

// The range of numbers that can be stored in an integer constant or variable
// is different for each numeric type.  A number that will not fit into an
// integer type is flagged as a compile-time error:

// `UInt8` cannot store negative numbers, so this will report an error:
// let cannotBeNegative: UInt8 = -1

// `Int8` cannot store a number larger than its maximum value, so this
// will also report an error:
// let tooBig: Int8 = Int8.max + 1

// Because each numeric type can store a different range of values, you must
// opt in to numeric type conversion on a case-by-case basis.  This approach
// prevents hidden conversion errors and helps make type conversion
// intentions explicit.

// To convert one specific number type to another, you initialize a new number
// of the desired type with the existing value.

let twoThousand: UInt16 = 2_000
let one: UInt8 = 1

// Conversion is necessary for us to work with values of different types.
let twoThousandAndOne = twoThousand + UInt16(one)
// `twoThousandAndOne` will be inferred to be of type `UInt`.

// The conversion was possible because `UInt16` has an initializer that
// accepts a `Uint8` value.

// -----------------------------------------------------------------------------

// Floating-point values are always truncated when used to initialize a new
// integer value in this way.

let three = 3
let pointOneFourOneFiveNine = 0.14159
let myPi = Double(three) + pointOneFourOneFiveNine
// `myPi` equals 3.14159, and is inferred to be of type `Double`.
let integerPi = Int(myPi)
// `integerPi` equals 3, and is inferred to be of type `Int`.

let floatAsInt1 = Int(4.75)       // `floatAsInt1` is set to 4.
let floatAsInt2 = Int(-3.9)       // `floatAsInt2` is set to -3.

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
// `maxAmplitudeFound` is now 0


// -----------------------------------------------------------------------------
//  Booleans
// -----------------------------------------------------------------------------

// Boolean values are referred to as logical because
// they can only ever be true or false.

let orangesAreOrang = true
let todayIsYesterday = false


// -----------------------------------------------------------------------------
//  Tuple - A compound value that groups together multiple values.
// -----------------------------------------------------------------------------

// The values within a tuple can be of any type and do not have to be of the
// same type as each other.  For example, `(Int, Int, Int)` or `(String,
// Bool)` or any other permutation you require.

let http404Error = (404, "Not Found")
// `http404Error` is of type `(Int, String)`, and equals `(404, "Not Found")`

// -----------------------------------------------------------------------------

// You can decompose a tuple's contents into separate constants or variables
// which you then access as usual.

let (code, message) = http404Error
print("The status code is \(code)")
// Prints "The status code is 404".
print("The status message is \(message)")
// Prints "The status message is Not Found".

// -----------------------------------------------------------------------------

// If you only need some of the tuple's values ignore the unwanted parts with
// an underscore (_) during decomposition.

let (statusCode, _) = http404Error
print("The status code is \(statusCode)")

// -----------------------------------------------------------------------------

// Alternatively, access the individual element values in a tuple using index
// numbers starting at zero.

print("The status code is \(http404Error.0)")
// Prints "The status code is 404"
print("The status message is \(http404Error.1)")
// Prints "The status message is Not Found"

// -----------------------------------------------------------------------------

// You can name the individual elements in a tuple when the tuple is defined which
// you can use to access the values of those elements.

let http200Status = (code: 200, description: "OK")
print("The status code is \(http200Status.code)")
// Prints "The status code is 200"
print("The status message is \(http200Status.description)")
// Prints "The status message is OK"


// -----------------------------------------------------------------------------
//  Optional - A type that says there is a value of a particular type and
//             it equals x or there isn't a value at all.
// -----------------------------------------------------------------------------

// You use optionals in situations where a value might be absent.

let possibleNumber = "123"
let convertedNumber = Int(possibleNumber)
// `convertedNumber` is inferred to be of type `Int?`, or "optional `Int`",
// because `Int` has an initializer that accepts a `String` that may or may
// not represent a number.

// You set an optional variable to a valueless state by assigning it the
// special value nil.

var serverResponseCode: Int? = 404
// `serverResponseCode` contains an actual `Int` value of 404.
serverResponseCode = nil
// `serverResponseCode` now contains no value.

// nil cannot be used with nonoptional constants and variables.  If a
// constant or variable in your code needs to work with the absence of
// a value under certain conditions, always declare it as an optional
// value of the appropriate type.

// An optional variable defined without an explicit default value will
// be set to nil.

var surveyAnswer: String?
// `surveyAnswer` is automatically set to nil.









// -----------------------------------------------------------------------------
//  Assertion - A runtime check that a given condition evaluates to true.
// -----------------------------------------------------------------------------

// If an assertion evaluates to false, code execution ends and your
// app is terminated.

// An assertion is written by calling the Swift Standard Library's global
// `assert(_:_:file:line:)` function.

let age = 3

// This causes the assertion to trigger, because `age` is not greater than 5.
// assert(age > 5)

// An assertion also lets you provide a suitable debug message that is
// displayed if the result of the condition is false.

// assert(age > 5, "Age needs to be greater than 5.")

// -----------------------------------------------------------------------------

// If an assertion is triggered while running in a debug environment,
// such as within Xcode, you can see exactly where the invalid state
// occurred and query the state of your app at the time that the
// assertion was triggered.

// Assertions are disabled when your code is compiled with optimizations,
// such as when building with an app target's default Release
// configuration in Xcode.
