// -----------------------------------------------------------------------------
//  Loop statement - A statement that can execute a block of code repeatedly.
//  `for-in` loop - A loop statement that iterates over a sequence.
//  Loop variable - A variable that controls the iterations of a `for-in` loop.
// -----------------------------------------------------------------------------

// The loop variable is, ironically, a constant by default whose value is
// automatically set at the start of each loop iteration.  It doesn't
// have to be declared before it's used.  It's implicitly declared by
// its inclusion in the loop declaration, without the need for a
// `let` keyword.

// Use a `for-in` loop to calculate a sum.  The loop variable is `index`.
var answer = 0
for index in 1...5 {
  answer += index * 5
}
assert(answer == 75)

// Attempting to declare the loop variable with `let` will trigger
// a compile-time error.

// Uncommenting this leads to a compile-time error:
/*
answer = 0
for let index in 1...5 {
  answer += index * 5
}
assert(answer == 75)
*/

// -----------------------------------------------------------------------------

// The loop variable can be declared a variable allowing it to be modified
// within the code block being looped over.

answer = 0
for var index in 1...5 {
  index -= 1
  answer += index * 5
}
assert(answer == 50)

// -----------------------------------------------------------------------------

// If you don't need each value from a sequence, you can ignore them by using
// an underscore in place of a loop variable name.

answer = 1
for _ in 1...10 {
  answer *= 3
}
assert(answer == 59_049)

// -----------------------------------------------------------------------------

// The sequence a `for-in` loop iterates over can be abstracted away.

answer = 1
var range = 1...10
for _ in range {
  answer *= 3
}
assert(answer == 59_049)

// -----------------------------------------------------------------------------

// A `for-in` loop can have an optional `where` clause that controls if
// the loop's block of code gets executed.

answer = 1
for i in 1...10 where i % 2 == 0 {
  answer += i
}
assert(answer == 31)

// A `for-in` loop's `where` clause doesn't need to have anything
// to do with the loop variable.

answer = 1
var loopCondition = 3 < 5
for i in 1...10 where loopCondition {
  answer += i
}
assert(answer == 56)

// -----------------------------------------------------------------------------

// A `for-in` loop can iterate over an array's items.

var allText = ""
let array = ["Foo", "Bar", "Baz"]
for item in array {
  allText += item
}
assert(allText == "FooBarBaz")

// -----------------------------------------------------------------------------

// A `for-in` loop can iterate over a dictionary's key-value pairs through...

// ...using decomposition.

var result = ""
let dictionary = ["spider": 8, "ant": 6, "cat": 4]
for (key, value) in dictionary {
  result += "\(key) \(String(value)) "
}
assert(result == "ant 6 spider 8 cat 4 ")
assert(result.characters.count == 21)

// ...using a regular loop variable.

result = ""
for keyValue in dictionary {
  result += String(describing: keyValue)
}
assert(result == "(\"ant\", 6)(\"spider\", 8)(\"cat\", 4)")
assert(result.characters.count == 33)

// -----------------------------------------------------------------------------








// TODO


result = ""
for indexValueTuple in array.enumerated() {
  result += String(describing: indexValueTuple)
}
assert(result == "(0, \"Foo\")(1, \"Bar\")(2, \"Baz\")")




// -----------------------------------------------------------------------------







let arrayOfOptionalInts: [Int?] = [nil, 2, 3, nil, 5]
// Match only non-nil values.
for case let number? in arrayOfOptionalInts {
  print("Found a \(number)")
}











let optionalInt:[Int]? = [1, 2, 3]

for i in optionalInt! { print(i) }

for i in optionalInt ?? [] { print(i) }

for i in optionalInt as [Int]! { print(i) }



















// The contents of a dictionary are inherently unordered, and iterating over
// them does not guarantee the order in which they will be retrieved.

// -----------------------------------------------------------------------------
//  `while` loop - The loop statement that checks a condition before executing a
//                 block of code, repeating until the condition becomes false.
// -----------------------------------------------------------------------------

var i = 0
loopCondition = true
while loopCondition {
  i += 1
  loopCondition = i < 10
}
assert(i == 10)

// -----------------------------------------------------------------------------

// TODO

// A `while` loop can use optional binding.










// -----------------------------------------------------------------------------
//  `repeat-while` loop - The loop statement that checks a condition after
//                        executing a block of code, repeating until the
//                        condition becomes false.
// -----------------------------------------------------------------------------

i = 0
loopCondition = true
repeat {
  i += 1
  loopCondition = i < 10
} while loopCondition
assert(i == 10)

// -----------------------------------------------------------------------------

// `while` and `repeat-while` loops are best used when the number of iterations
// is not known before the first iteration begins.


// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - Control Flow
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/ControlFlow.html
// =============================================================================
