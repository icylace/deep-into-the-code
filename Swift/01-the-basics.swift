// =============================================================================
//  The Basics
//  https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html
// =============================================================================


// -----------------------------------------------------------------------------
//  Constants and Variables
// -----------------------------------------------------------------------------

// Constants and variables associate a name with a value of a particular type.
// A constant is immutable which means it cannot have its value changed once it
// is set, whereas a variable is mutable which means it can be set to a
// different value in the future.

// Declare a constant named `answer` and assign it the literal value 42.
let answer = 42

// Declare a variable named `myAnswer` and assign it the literal value 42.
var myAnswer = 42

// A literal value (or literal) is a value that appears directly in your source
// code, such as 42.

// Multiple constants or multiple variables declared on a single line are
// separated by commas.
let a = 0, b = 1, c = 2
var x = 3, y = 4, z = 5

// STYLE:
// If a stored value in your code is not going to change, always declare it as
// a constant with the let keyword. Use variables only for storing values that
// need to be able to change.
// - https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-NoLink_27

// -----------------------------------------------------------------------------

// A type annotation can be used when you declare a constant or variable to be
// clear about the kind of values the constant or variable can store.

// Define a constant named `theAnswer` that is of type `Int` (integer) and assign
// it the value 42.
let theAnswer: Int = 42

// The type annotation is required if there's no initial assignment.
let lateAnswer: Int

// Constants can be assigned to only once.
lateAnswer = 42

// Trying to assign another value to a constant will cause a compilation error.
// lateAnswer = 1

// Declare a variable named `message` that is of type `String`.
var message: String

// `message` can now be assigned string values.
message = "hi"
message = "hello"

// Trying to assign a non-string value to `message` will cause
// a compilation error.
// message = 1

// If no type annotation is provided the type of the constant or variable is
// inferred from its initial assigned value.
var hello = "hello"

// -----------------------------------------------------------------------------

// Multiple variables of the same type can be declared on a single line,
// separated by commas, with a single type annotation after the final
// variable name.
var red, green, blue: Double

// -----------------------------------------------------------------------------

// Constant and variable names can contain almost any character,
// including Unicode characters.
let π = 3.14159
let 你好 = "你好世界"
let 🐶🐮 = "dogcow"

// Constant and variable names cannot contain whitespace characters,
// mathematical symbols, arrows, private-use (or invalid) Unicode code points,
// or line-drawing and box-drawing characters.  Nor can they begin with a
// number, although numbers may be included elsewhere within the name.

// -----------------------------------------------------------------------------

// If you need to give a constant or variable the same name as a reserved Swift
// keyword, surround the keyword with backticks (`) when using it as a name.
let `let` = "i'm a constant"

// STYLE:
// Avoid using keywords as constant or variable names unless you have
// absolutely no choice.
// - https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-NoLink_29

// -----------------------------------------------------------------------------

// You can print the current value of a constant or variable with the global
// `print(_:separator:terminator:)` function from the Swift Standard Library.
print(answer)
// Prints "42".


// -----------------------------------------------------------------------------
//  Comments
// -----------------------------------------------------------------------------

// As you've probably already noticed, single-line comments begin with double
// forward slashes.  Comments are ignored by the compiler but serve to allow
// you to have helpful text with your code.

/* Here is a block comment.
Block comments can span
multiple lines. */

/* Block comments can be used like single-line comments. */

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
//  Semicolons
// -----------------------------------------------------------------------------

// Semicolons are optional when only a single statement is on the line.
let p = 1
var q = 2;

// Semicolons are required when multiple statements are on the same line.
let r = 1; var s = 2


// -----------------------------------------------------------------------------
//  Integers
// -----------------------------------------------------------------------------

// Integers are whole numbers with no fractional component and are either
// signed (positive, zero, or negative) or unsigned (positive or zero).

// Swift provides signed and unsigned integers in 8, 16, 32, and 64 bit forms.
// These integers follow a naming convention similar to C, in that an 8-bit
// unsigned integer is of type `UInt8`, and a 32-bit signed integer is of type
// `Int32`. Like all types in Swift, these integer types have capitalized names.

// In most cases, you don't need to pick a specific size of integer to use in
// your code.  Swift's integer type, `Int`, has the same size as the current
// platform's native word size:
// - On a 32-bit platform, `Int` is the same size as `Int32`.
// - On a 64-bit platform, `Int` is the same size as `Int64`.

// Swift's unsigned integer type, `UInt`, has the same size as the current
// platform's native word size:
// - On a 32-bit platform, `UInt` is the same size as `UInt32`.
// - On a 64-bit platform, `UInt` is the same size as `UInt64`.

// STYLE:
// Use `UInt` only when you specifically need an unsigned integer type with the
// same size as the platform's native word size.  If this is not the case, `Int`
// is preferred, even when the values to be stored are known to be non-negative.
// A consistent use of `Int` for integer values aids code interoperability,
// avoids the need to convert between different number types, and matches
// integer type inference
// - https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-NoLink_31

// -----------------------------------------------------------------------------

// The minimum and maximum values of each integer type are accessed
// through their `min` and `max` properties.
let minValue = UInt8.min    // `minValue` is set to 0, and is of type `UInt8`.
let maxValue = UInt8.max    // `maxValue` is set to 255, and is of type `UInt8`.


// -----------------------------------------------------------------------------
//  Floating-Point Numbers
// -----------------------------------------------------------------------------

// Floating-point numbers are numbers with a fractional component.

// Floating-point types can represent a much wider range of values than integer
// types, and can store numbers that are much larger or smaller than can be
// stored in an integer.  Swift provides two signed floating-point number types:
// - `Double` represents a 64-bit floating-point number and has a precision of
//   at least 15 decimal digits.
// - `Float` represents a 32-bit floating-point number and can have a precision
//   as little as 6 decimal digits.

// STYLE:
// In situations where either `Double` or `Float` would be appropriate,
// `Double` is preferred.
// - https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-NoLink_32


// -----------------------------------------------------------------------------
//  Type Safety and Type Inference
// -----------------------------------------------------------------------------

// Swift is a type-safe language.  A type-safe language encourages you to be
// clear about the types of values your code can work with.  If part of your
// code expects a `String`, you can't use an `Int` with it by mistake.

// Type checks happen as part of the compilation process and any type mismatches
// are flagged as compile-time errors.

// Swift uses type inference to determine the type of something if it isn't
// specified explicitly.  It does this by examining the values you use.

// Type inference is particularly useful when you declare a constant or
// variable with an initial value.

let meaningOfLife = 42
// `meaningOfLife` is inferred to be of type `Int`.

let pi = 3.14159
// `pi` is inferred to be of type `Double`.

// If you combine integer and floating-point literals in an expression,
// a type of `Double` will be inferred from the context.
let anotherPi = 3 + 0.14159
// `anotherPi` is also inferred to be of type `Double`.


// -----------------------------------------------------------------------------
//  Numeric Literals
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
// is multiplied by 10^exp (10 raised to the exp power):
let n1 = 1.25e2         // This equals 1.25 x 10^2 which equals 125.0.
let n2 = 1.25e-2        // This equals 1.25 x 10^-2 which equals 0.0125.

// For hexadecimal numbers with an exponent of exp, the base number
// is multiplied by 2^exp:
let n3 = 0xFp2        // This equals 15 x 2^2 which equals 60.0.
let n4 = 0xFp-2       // This equals 15 x 2^-2 which equals 3.75.

// These are all equal:
let decimalDouble = 12.1875
let exponentDouble = 1.21875e1
let hexadecimalDouble = 0xC.3p0

// -----------------------------------------------------------------------------

// Numeric literals can contain extra formatting to make them easier to read.
// Both integers and floats can be padded with extra zeros and can contain
// underscores to help with readability.  Neither type of formatting affects
// the underlying value of the literal.
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

// TODO


// -----------------------------------------------------------------------------
//  Type Aliases
// -----------------------------------------------------------------------------

// // Type aliases allow us to use different names for existing types.
// // They are a tool to make our code more readable.
// typealias Seconds = Double
// typealias MyClassAlias = C1
//
// // Type aliases are particularly useful for simplifying
// // more complex types like the ones for closures.
// typealias MyClosureDefinition = (Int, String, (Double, Double)) -> [String]


// -----------------------------------------------------------------------------
//  Booleans
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Tuples
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Optionals
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Error Handling
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Assertions
// -----------------------------------------------------------------------------

























/*






// -----------------------------------------------------------------------------

// Primitive types.

true                     // A boolean literal.
1                        // An integer literal.
2.2                      // A double floating-point literal.
"way"                    // A string literal.
[2, 3, 4]                // An array of integers.
(1.3, "is", false)       // A tuple of double, string, and boolean.


// A closed range.
-11...12
// -11..<13

// A semi-open range.
-11..<12
// -11..<12


Double.infinity
Float.infinity
Float32.infinity
Float64.infinity
CDouble.infinity




*/







/*












// -----------------------------------------------------------------------------











// Compound types.

// Optional types.





// ----

import Foundation

// The difference between
var p1: NSObject = NSObject()
// and
var p2: NSObject
p2 = NSObject()
// is that Xcode will complain about the former if `p` doesn't get mutated.


// ----




//operators
//Array
//multi-dimensional array
//Dictionary
//tuple
//optionals
//functions
//parameter names
//default arguments
//closures
//generics
//subscripts
//error handling
//testing






















let tag: String? = nil
let tagResult = tag ?? "<none>"

// Alternate:
//
// let tagResult: String
// if let tag = tag {
//   tagResult = tag
// } else {
//   tagResult = "<none>"
// }







42.dynamicType
answer.dynamicType

2.2.dynamicType



_ = print.self


_ = Int()


2.advancedBy(3)




_ = 0
_ = ""





print(Int.self)


let a = 0...3
a.count
a.self
a.dynamicType
print(a)

for _ in 0...3 {
  print("In a loop 4 times.")
}

for _ in a {
  print("This will also loop 4 times.")
}





print("test")
debugPrint("test")



// import UIKit






//
//
//class AA {
//  var bb = self
//}
//
//
//





//autoreleasepool {}




//@testable
//import MyApp







// Further reading:
// http://blog.krzyzanowskim.com/2015/03/09/swift-asserts-the-missing-manual/




// `dump()` crashes when trying to use it an `UnsafeMutablePointer`







// // http://stackoverflow.com/a/24102243/1935675
// import Darwin
// exit(0)

*/
