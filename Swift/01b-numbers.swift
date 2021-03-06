// -----------------------------------------------------------------------------
//  Integer - A whole number with no fractional component that is either signed
//            (positive, zero, or negative) or unsigned (positive or zero).
// -----------------------------------------------------------------------------

// Integers can be either signed or unsigned in 8, 16, 32, and 64-bit forms.

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

// Usually you don't need to pick a specific size of integer to use.  `Int` and
// `UInt` have the same size as the current platform's native word size.  For
// platforms that are...
// ...32-bit, `Int` is as big as `Int32` and `UInt` is as big as `UInt32`.
// ...64-bit, `Int` is as big as `Int64` and `UInt` is as big as `UInt64`.

// -----------------------------------------------------------------------------

// The minimum and maximum values of each integer type are accessed
// through their `min` and `max` properties, respectively.

let minValue = UInt8.min
assert(type(of: minValue) == UInt8.self)
assert(minValue == 0)

let maxValue = UInt8.max
assert(type(of: maxValue) == UInt8.self)
assert(maxValue == 255)

// -----------------------------------------------------------------------------
//  Floating-point number - A number with a fractional component.
// -----------------------------------------------------------------------------



// TODO

// Floating-point types represent a much wider range of values than integer
// types and can store numbers that are much larger or smaller than can be
// stored in an integer.

// There are no unsigned floating-point types but there are three signed ones:
// - `Float` (or `Float32`) represents a 32-bit single-precision floating-point
//   number and can have a precision as little as 6 decimal digits.
// - `Double` (or `Float64`) represents a 64-bit double-precision floating-point
//   number and has a precision of at least 15 decimal digits.
// - `Float80` represents an 80-bit extended-precision floating-point number.

let pi = 3.14159
assert(type(of: pi) == Double.self)

// If you combine integer and floating-point literals in an expression,
// a type of `Double` will be inferred from the context.

let pi2 = 3 + 0.14159
assert(type(of: pi2) == Double.self)

assert(pi == pi2)









// -----------------------------------------------------------------------------
//  Numeric literal - A literal value that represents a number.
// -----------------------------------------------------------------------------

// An integer literal can be represented in decimal, binary, octal,
// or hexadecimal notation.

var l = 17
assert(l == 0b10001)
assert(l == 0o21)
assert(l == 0x11)

// -----------------------------------------------------------------------------

// A floating-point literal...
// ...cannot have its decimal point be its first
//    or last character.
// ...can be represented in decimal notation.
// ...can be represented in hexadecimal notation.

var n = 3.0
assert(n == 0x3p0)

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

// A numeric literal can contain extra formatting that make it easier to read
// while retaining its underlying value.

assert(10_.0                    == 10.0)
assert(000123.456               == 123.456)
assert(1_000_000.000_000_1      == 1000000.0000001)
assert(0001__000.000_000_1      == 1000.0000001)
assert(1_000_000                == 1000000)
assert(2___3__                  == 23)
assert(00000000004___5          == 45)
assert(0b1_010_10_101           == 0b101010101)
assert(0o34__53                 == 0o3453)
assert(0xFEED_FACE___           == 0xFEEDFACE)
assert(0xFEED.FACE___p1_        == 0xFEED.FACEp1)
assert(0x0000000FEED.FACE___p1_ == 0xFEED.FACEp1)

// Underscores cannot be put directly after the prefix for binary, octal, and
// hexadecimal numbers.

// Uncommenting this leads to a compile-time error:
/*
l = 0b_01_010_10_101
l = 0o_34__53
l = 0x_FEED_FACE___
n = 0x_FEED.FACE___p1_
*/

// -----------------------------------------------------------------------------
//  Numeric type conversion - The behavior of changing the type of a numeric
//                            value into another numeric type.
// -----------------------------------------------------------------------------

// The range of numbers that can be stored in an integer constant/variable
// is different for each numeric type.  A number that will not fit into
// a constant/variable of a sized integer type is flagged as a
// compile-time error.

// Uncommenting this leads to a compile-time error:
/*
let cannotBeNegative: UInt8 = -1
let tooBig: Int8 = Int8.max + 1
*/

// -----------------------------------------------------------------------------

// Conversion is necessary for us to work with values of different types.

// Because each numeric type can store a different range of values, you must
// opt in to numeric type conversion on a case-by-case basis.  This approach
// prevents hidden conversion errors and helps make type conversion
// intentions explicit.

// To convert one specific number type to another, you initialize a new number
// of the desired type with the existing value.

// The conversion here is possible because `UInt16` has an initializer that
// accepts a `Uint8` value.
let one: UInt8 = 1
let twoK: UInt16 = 2_000
let twoKAndOne = twoK + UInt16(one)
assert(type(of: twoKAndOne) == UInt16.self)
assert(twoKAndOne == 2001)

let three = 3
let pointOneFourOneFiveNine = 0.14159
let pi3 = Double(three) + pointOneFourOneFiveNine
assert(type(of: pi3) == Double.self)
assert(pi3 == 3.14159)

// -----------------------------------------------------------------------------

// Floating-point values are truncated when used to initialize an `Int` value.

let integerPi = Int(pi3)
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


// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - The Basics
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html
// =============================================================================
