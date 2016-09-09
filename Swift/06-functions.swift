// =============================================================================
//  Functions
//  https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Functions.html
// =============================================================================


// -----------------------------------------------------------------------------
//  Defining and Calling Functions
// -----------------------------------------------------------------------------

// Define a function called `sayHi` which accepts a string as its sole input
// and that also returns a string.
func sayHi(name: String) -> String {
  let greeting = "Hello, \(name)!"
  return greeting
}

// Call `sayHi` by passing it a string value that has the label `name`.
let a = sayHi(name: "Anna")
assert(a == "Hello, Anna!")


// -----------------------------------------------------------------------------
//  Function Parameters and Return Values
// -----------------------------------------------------------------------------

// A function that takes no parameters.
func sayHi() -> String {
  return "hi"
}

let d = sayHi()
assert(d == "hi")

// -----------------------------------------------------------------------------

// A function that takes a couple parameters.
func sayHi(name: String, alreadyGreeted: Bool) -> String {
  if alreadyGreeted {
    return "Hello again, \(name)!"
  } else {
    return sayHi(name: name)
  }
}

let e = sayHi(name: "Tim", alreadyGreeted: true)
assert(e == "Hello again, Tim!")

// -----------------------------------------------------------------------------

// A function that doesn't return anything.
func sayBye(name: String) {
  print("Goodbye, \(name)!")
}

sayBye(name: "Dave")
// Prints "Goodbye, Dave!"

// -----------------------------------------------------------------------------

// Strictly speaking, the `sayBye` function does still return a value, even
// though no return value is defined.  Functions without a defined return
// return a special value of type `Void`.  This is simply an empty tuple,
// in effect a tuple with zero elements, which can be written as `()`.

func sayByeAgain(name: String) -> Void {
  print("Goodbye again, \(name)!")
}

sayByeAgain(name: "Dave")
// Prints "Goodbye again, Dave!"

func sayFinalBye(name: String) -> () {
  print("Final goodbye, \(name)!")
}

sayFinalBye(name: "Dave")
// Prints "Final goodbye, Dave!"

// -----------------------------------------------------------------------------

// A function's return value can be ignored.
func printAndCount(text: String) -> Int {
  print(text)
  return text.characters.count
}
func printWithoutCounting(text: String) {
  printAndCount(text: text)
}
printAndCount(text: "hello, world")
// Prints "hello, world" and returns a value of 12.
printWithoutCounting(text: "hello, world")
// Prints "hello, world" but does not return a value.

// Return values can be ignored, but a function with a defined return type
// must always return a value.

// -----------------------------------------------------------------------------

// A tuple can be used to emulate multiple simultaneous return values.
func minMax(array: [Int]) -> (min: Int, max: Int) {
  var curMin = array[0]
  var curMax = array[0]
  for value in array[1..<array.count] {
    if value < curMin {
      curMin = value
    } else if value > curMax {
      curMax = value
    }
  }
  return (curMin, curMax)
}

// Because the tuple's member values are named as part of the function's return
// type, they can be accessed with dot syntax.
let bounds = minMax(array: [8, -6, 2, 109, 3, 71])
assert(bounds.min == -6)
assert(bounds.max == 109)

// -----------------------------------------------------------------------------

// An optional tuple type such as `(Int, Int)?` is different from a tuple
// that contains optional types such as `(Int?, Int?)`.  With an optional
// tuple type, the entire tuple is optional, not just each individual
// value within the tuple.

func minMax2(array: [Int]) -> (min: Int, max: Int)? {
  if array.isEmpty { return nil }
  var curMin = array[0]
  var curMax = array[0]
  for value in array[1..<array.count] {
    if value < curMin {
      curMin = value
    } else if value > curMax {
      curMax = value
    }
  }
  return (curMin, curMax)
}

if let bounds = minMax2(array: [8, -6, 2, 109, 3, 71]) {
  assert(bounds.min == -6)
  assert(bounds.max == 109)
}


// -----------------------------------------------------------------------------
//  Function Parameter Names
// -----------------------------------------------------------------------------

// A function that takes a single parameter.
func f1(parameter: Int) {
  // Within the function, `parameter` is used to refer to the parameter value.
  assert(parameter == 1)
}

// By default the parameter name must be used as a label for
// the argument passed to the function.
f1(parameter: 1)

// -----------------------------------------------------------------------------

// Parameters can have custom external parameter names.
func f2(externalParameterName parameter: Int) {
  // Within the function, `parameter` is used
  // instead of `externalParameterName`.
  assert(parameter == 1)
}

// The external parameter name must be used as a label for
// the argument passed to the function.
f2(externalParameterName: 1)

// -----------------------------------------------------------------------------

// If a parameter's external name is an underscore then
// it's not labelled in calls to the function.
func f3(_ parameter: Int) {
  assert(parameter == 1)
}

// The parameter label is prohibited.
f3(1)

// -----------------------------------------------------------------------------

// Parameters can be assigned a default value.
func f4(parameter: Int = 2) {
  // If no valid arguments are passed to the function call then
  // the value of `parameter` will be 2.
  assert(parameter % 2 == 0)
}

f4(parameter: 4)        // `parameter` will be 4.
f4()        // `parameter` will be 2.  Note the label is not needed here.

// STYLE:
// Place parameters with default values at the end of a function's parameter
// list.  This ensures that all calls to the function use the same order for
// their nondefault arguments, and makes it clear that the same function is
// being called in each case.

// -----------------------------------------------------------------------------

// A variadic parameter accepts zero or more values of a specified type.
// A function may have at most one variadic parameter.
func f5(numbers: Double...) -> Double {
  var total: Double = 1
  for number in numbers {
    total *= number
  }
  return total
}

let r5 = f5(numbers: 2.35, 6.7, 55.0)
assert(r5 == 865.975)

// -----------------------------------------------------------------------------

// Function parameters are constants by default.  Parameters that can be
// modified are in-out parameters.

func f6(x: inout Int) {
  x *= 2
}

var r6 = 4
// The ampersand is needed because we're passing in a reference to `r6`.
f6(x: &r6)
assert(r6 == 8)

// In-out parameters can't have default values, and variadic parameters
// can't be in-out.

// In-out parameters are not the same as returning a value from a function.
// In-out parameters are an alternative way for a function to have an effect
// outside of the scope of its function body.

// -----------------------------------------------------------------------------

// TODO

// Multiple parameters are comma-separated.
func f6(first: Int, second: Int) {
  // Do stuff.
}

f6(first: 1, second: 2)

// -----------------------------------------------------------------------------

func f7(_ first: Int, foo second: Int = 4) {
  // Do stuff.
}

f7(1, foo: 2)


// -----------------------------------------------------------------------------
//  Function Types
// -----------------------------------------------------------------------------

// Every function has a specific function type made up of the parameter
// types and the return type of the function.

// func addTwoInts(a: Int, _ b: Int) -> Int {
//   return a + b
// }
// func multiplyTwoInts(a: Int, _ b: Int) -> Int {
//   return a * b
// }


// -----------------------------------------------------------------------------
//  Nested Functions
// -----------------------------------------------------------------------------

func chooseStepper(reversed: Bool) -> (Int) -> Int {
  func increase(x: Int) -> Int { return x + 1 }
  func decrease(x: Int) -> Int { return x - 1 }
  return reversed ? decrease : increase
}

var curValue = -4
let stepToZero = chooseStepper(reversed: curValue > 0)
// `stepToZero` now refers to the nested `increase()` function.

while curValue != 0 {
  print("\(curValue)... ")
  // The label for the nested function is not allowed.
  curValue = stepToZero(curValue)
}

print("zero!")

// -4...
// -3...
// -2...
// -1...
// zero!


// -----------------------------------------------------------------------------

// // Swift 3 bug?
//
// func foo() -> (Int) -> Int {
//   func bar(x: Int) -> Int { return x + 1 }
//   return bar
// }
//
// // The label for the nested function is not allowed to be used.
//
// print(foo()(1))
// // 2
//
// let baz = foo()
// print(baz(1))
// // 2
