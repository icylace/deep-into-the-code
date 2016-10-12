// =============================================================================
//  The Basics
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html
// =============================================================================

// -----------------------------------------------------------------------------
//  Integer - A whole number with no fractional component that is either signed
//            (positive, zero, or negative) or unsigned (positive or zero).
// -----------------------------------------------------------------------------

// Integer types can be either signed or unsigned in 8, 16, 32,
// and 64-bit forms.

let i1: Int         // A signed integer with default bit form.
let i2: Int8        // An 8-bit signed integer.
let i3: Int16       // A 16-bit signed integer.
let i4: Int32       // A 32-bit signed integer.
let i5: Int64       // A 64-bit signed integer.

let i6: UInt          // An unsigned integer with default bit form.
let i7: UInt8         // An 8-bit unsigned integer.
let i8: UInt16        // A 16-bit unsigned integer.
let i9: UInt32        // A 32-bit unsigned integer.
let i10: UInt64       // A 64-bit unsigned integer.

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
assert(type(of: minValue) == UInt8.self)
assert(minValue == 0)

let maxValue = UInt8.max
assert(type(of: maxValue) == UInt8.self)
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
assert(type(of: pi) == Double.self)

// If you combine integer and floating-point literals in an expression,
// a type of `Double` will be inferred from the context.

let myPi = 3 + 0.14159
assert(type(of: myPi) == Double.self)

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

n = 10_.0                    ; assert(n == 10.0)
n = 000123.456               ; assert(n == 123.456)
n = 1_000_000.000_000_1      ; assert(n == 1000000.0000001)
n = 0001__000.000_000_1      ; assert(n == 1000.0000001)
l = 1_000_000                ; assert(l == 1000000)
l = 2___3__                  ; assert(l == 23)
l = 00000000004___5          ; assert(l == 45)
l = 0b1_010_10_101           ; assert(l == 0b101010101)
l = 0o34__53                 ; assert(l == 0o3453)
l = 0xFEED_FACE___           ; assert(l == 0xFEEDFACE)
n = 0xFEED.FACE___p1_        ; assert(n == 0xFEED.FACEp1)
n = 0x0000000FEED.FACE___p1_ ; assert(n == 0xFEED.FACEp1)

// Underscores cannot be put directly after the prefix for binary, octal, and
// hexadecimal numbers.  Attempting to do so results in a compile-time error.

// If the following is uncommented it will produce a runtime error:
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
// flagged as a compile-time error.

// `UInt8` cannot store negative numbers.

// If the following is uncommented it will produce a runtime error:
/*
let cannotBeNegative: UInt8 = -1
*/

// `Int8` cannot store a number larger than its maximum value.

// If the following is uncommented it will produce a runtime error:
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
assert(type(of: twoThousandAndOne) == UInt16.self)
assert(twoThousandAndOne == 2001)

let three = 3
let pointOneFourOneFiveNine = 0.14159
let myPi2 = Double(three) + pointOneFourOneFiveNine
assert(type(of: myPi2) == Double.self)
assert(myPi2 == 3.14159)

// -----------------------------------------------------------------------------

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
