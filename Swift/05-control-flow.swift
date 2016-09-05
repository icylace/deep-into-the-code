// -----------------------------------------------------------------------------
//  For-In Loops
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  While Loops
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Conditional Statements
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Control Transfer Statements
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Early Exit
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Checking API Availability
// -----------------------------------------------------------------------------







// `for` loops repeat a predetermined number of times.

for i in 0..<3 {
  print(i)
}

let range = 0..<3
for _ in range {
  print("a")
}

// `while` loops repeat a nonpredetermined number of times.

import Foundation

while 0.5 < drand48() {
  print("still going...")
}

var i = 0
while i < 10 {
  i += 1
}

// `repeat` loops are like `while` loops except they their condition
// check at the end of the loop.

var j = 0
repeat {
  j += 1
} while j < 10

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
  assert(x == false)
  assert(y == true)
} else {
  assert(x == false)
  assert(y == false)
}

if x {
  assert(x == true)
} else if y {
  assert(x == false)
  assert(y == true)
} else if z {
  assert(x == false)
  assert(y == false)
  assert(z == true)
} else {
  assert(x == false)
  assert(y == false)
  assert(z == false)
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

// `switch`

var s = 0
switch s {
case 0:
  print(0)
default:
  print("non-zero")
}
