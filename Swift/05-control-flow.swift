// =============================================================================
//  Control Flow
//  https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/ControlFlow.html
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
