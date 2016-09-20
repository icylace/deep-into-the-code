// =============================================================================
//  Control Flow
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/ControlFlow.html
// =============================================================================


// -----------------------------------------------------------------------------
//  For-In Loops
// -----------------------------------------------------------------------------

// TODO

// `for-in` loops repeat a predetermined number of times.

for i in 0..<3 {
  print(i)
}

let range = 0..<3
for _ in range {
  print("a")
}

// -----------------------------------------------------------------------------

(1...10).forEach {
  print($0)
}


// -----------------------------------------------------------------------------
//  While Loops
// -----------------------------------------------------------------------------

// `while` loops repeat a nonpredetermined number of times.

import Foundation

while 0.5 < drand48() {
  print("still going...")
}

var i = 0
while i < 10 {
  i += 1
}

// -----------------------------------------------------------------------------

// `repeat-while` loops are like `while` loops except they their condition
// check at the end of the loop.

var j = 0
repeat {
  j += 1
} while j < 10


// -----------------------------------------------------------------------------
//  Conditional Statements
// -----------------------------------------------------------------------------

// `if` statements can direct code execution through a detour.

var x = true, y = true, z = false

if x {
  assert(x == true)
}

if x {
  assert(x == true)
} else {
  assert(x == false)
}

if x {
  assert(x == true)
} else if y {
  assert(x == false && y == true)
} else {
  assert(x == false && y == false)
}

if x {
  assert(x == true)
} else if y {
  assert(x == false && y == true)
} else if z {
  assert(x == false && y == false && z == true)
} else {
  assert(x == false && y == false && z == false)
}

// `if` supports optional unwrapping.

var maybeThing: Optional<Int> = 3
var maybeThing: Int? = 3

if let thing = maybeThing {
  print(thing)
}

if maybeThing != nil {
  print(maybeThing!)
}










if convertedNumber != nil {
  print("convertedNumber contains some integer value.")
}
// Prints "convertedNumber contains some integer value."

if convertedNumber != nil {
  print("convertedNumber has an integer value of \(convertedNumber!).")
}
// Prints "convertedNumber has an integer value of 123."

if let actualNumber = Int(possibleNumber) {
    print("\"\(possibleNumber)\" has an integer value of \(actualNumber)")
} else {
    print("\"\(possibleNumber)\" could not be converted to an integer")
}
// Prints ""123" has an integer value of 123"

if let firstNumber = Int("4"), secondNumber = Int("42") where firstNumber < secondNumber {
  print("\(firstNumber) < \(secondNumber)")
}
// Prints "4 < 42"

// Constants and variables created with optional binding in an if statement are
// available only within the body of the if statement.  In contrast, the
// constants and variables created with a guard statement are available
// in the lines of code that follow the guard statement.

let possibleString: String? = "An optional string."
let forcedString: String = possibleString! // requires an exclamation mark

let assumedString: String! = "An implicitly unwrapped optional string."
let implicitString: String = assumedString // no need for an exclamation mark

// If an implicitly unwrapped optional is `nil` and you try to access its
// wrapped value, you’ll trigger a runtime error.  The result is exactly
// the same as if you place an exclamation mark after a normal optional
// that does not contain a value.

if assumedString != nil {
  print(assumedString)
}
// Prints "An implicitly unwrapped optional string."

if let definiteString = assumedString {
  print(definiteString)
}
// Prints "An implicitly unwrapped optional string."













let tuple = (1.0, "hello")
print(tuple.0)
print(tuple.1)

// if case {
// }

// -----------------------------------------------------------------------------

// `switch`

var s = 0
switch s {
case 0:
  print(0)
default:
  print("non-zero")
}

// -----------------------------------------------------------------------------

let approximateCount = 62
let countedThings = "moons orbiting Saturn"
var naturalCount: String
switch approximateCount {
case 0:
  naturalCount = "no"
case 1..<5:
  naturalCount = "a few"
case 5..<12:
  naturalCount = "several"
case 12..<100:
  naturalCount = "dozens of"
case 100..<1000:
  naturalCount = "hundreds of"
default:
  naturalCount = "many"
}
print("There are \(naturalCount) \(countedThings).")
// Prints "There are dozens of moons orbiting Saturn."


// -----------------------------------------------------------------------------
//  Control Transfer Statements
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Early Exit
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Checking API Availability
// -----------------------------------------------------------------------------
