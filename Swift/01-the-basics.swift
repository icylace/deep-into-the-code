// =============================================================================
// =============================================================================


// -----------------------------------------------------------------------------
//  Constants and Variables
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Comments
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Semicolons
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Integers
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Floating-Point Numbers
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Type Safety and Type Inference
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Numeric Literals
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Numeric Type Conversion
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Type Aliases
// -----------------------------------------------------------------------------


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















// Declaring Constants and Variables
let maximumNumberOfLoginAttempts = 10
var currentLoginAttempt = 0




let π = 3.14159
let 你好 = "你好世界"
let 🐶🐮 = "dogcow"












// Here is a single-line comment.  Comments are ignored by the compiler.

// You can put several single-line comments
// together to simulate a block comment.

/* Here is a block comment.
Block comments are usually used
to write multi-line comments. */

/* Here is a block comment that is used like a single-line comment. */

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

// This is a string which is basically just text.
"Hello world!"

// We can display this string by using the `print` function from the
// Swift Standard Library.  We'll talk more about functions later.
print("Hello world!")

print("hi")       // We can put single-line comments at the end of the line.

print("hi")       /* Same goes for block comments. */

/* Unlike single-line comments, */print("hi")
print/* block comments can */("hi")
print(/* be placed in weird places */"hi")
print("hi"/* but please don't do that! */)

// Semicolons are optional when only a single statement is on the line.
print("hi")
print("hi");

// Semicolons are required when multiple statements are on the same line.
print("hi"); print("hi")

// -----------------------------------------------------------------------------

// Primitive types.

true                     // A boolean literal.
1                        // An integer literal.
2.2                      // A double floating-point literal.
"way"                    // A string literal.
[2, 3, 4]                // An array of integers.
(1.3, "is", false)       // A tuple of double, string, and boolean.

1_000_000       // Underscores can be in a number without affecting its value.
// 1000000
2__3
// 23

// Binary numbers.
0b101010101
0b1_010_10_101

// Octal numbers.
0o3453
0o34__53

// Hexadecimal numbers.
0xFEEDFACE
0xFEED_FACE___

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

Int.min
Int.max
Int8.max
Int16.max
Int32.max
Int64.max
CInt.max


7.1e9


10
10.0
10.00
10_.0


// All of these are equivalent:
let a: Array<Int> = Array<Int>.init(arrayLiteral: 0, 1, 2)
let b: Array<Int> = Array<Int>(arrayLiteral: 0, 1, 2)
let c: Array<Int> = Array(arrayLiteral: 0, 1, 2)
let d: [Int] = Array(arrayLiteral: 0, 1, 2)
let e: [Int] = [Int].init(arrayLiteral: 0, 1, 2)
let f: [Int] = [Int](arrayLiteral: 0, 1, 2)
let g = [Int](arrayLiteral: 0, 1, 2)
let h = [Int]([0, 1, 2])
let i = [0, 1, 2]
// [0, 1, 2]

assert(a == b && b == c && c == d && d == e && e == f && f == g && g == h && h == i)


// Constants are immutable which means they can't change once assigned.
// The type of the constant is written after a colon after the constant name.
// This is known as a type annotation and it is also used elsewhere.
let answer: Int = 42

// We can take advantage of the initialization value to infer the type of the constant.
let myAnswer = 42

// The type annotation is required is there's no initial assignment.
let lateAnswer: Int

// A variable.  Variables are mutable which means they can change.
var hi: String = "hi"

// Again, we can take advantage of type inference.
var hello = "hello"

// Again, we need a type annotation here.
var howdy: String



// let a = 0, b = 1, c = 2

// var x = 3, y = 4, z = 5
























// -----------------------------------------------------------------------------

























// http://stackoverflow.com/a/34983398/1935675
print(UnicodeScalar("A").value)



// ----

// https://www.drivenbycode.com/the-missing-apply-function-in-swift/
func repeatIt(str: String, _ n: Int) -> String {
  return [String](count: n, repeatedValue: str).joinWithSeparator("")
}

print(repeatIt("ha", 3)) // -> "hahaha"

// Now with a tuple.
// Note this technique is deprecated as of Swift 2.2 (I believe).
let params = ("ha", 3)
print(repeatIt(params)) // -> "hahaha"

// ----





// Collection types.

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


















// Type aliases allow us to use different names for existing types.
// They are a tool to make our code more readable.
typealias Seconds = Double
typealias MyClassAlias = C1

// Type aliases are particularly useful for simplifying
// more complex types like the ones for closures.
typealias MyClosureDefinition = (Int, String, (Double, Double)) -> [String]






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
