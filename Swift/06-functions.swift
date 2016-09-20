// =============================================================================
//  Functions
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Functions.html
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

// Note that both functions have the same name but they don't conflict.
// That's because they have different types.  More on that later.

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
func countText(text: String) -> Int {
  return text.characters.count
}
func countTextWithoutReturn(text: String) {
  // A warning will be flagged anytime a function's return value is ignored.
  countText(text: text)
}

// A warning will be flagged anytime a function's return value is ignored.
countText(text: "hi")

countTextWithoutReturn(text: "hi")

// Return values can be ignored but a function with a defined return type
// that's not `Void` must always return a value.

// If you don't care about a function's return value when you call it, assign
// the function call to an underscore.  This will discard the result without
// triggering a warning.
_ = countText(text: "hi")

// -----------------------------------------------------------------------------

// A tuple can be used to emulate multiple simultaneous return values.

func findMaxAndSize(xs: [Int]) -> (max: Int, size: Int) {
  var curMax = xs[0]
  for x in xs[1..<xs.count] {
    if x > curMax {
      curMax = x
    }
  }
  return (curMax, xs.count)
}

// Because the tuple's member values are named as part of the function's return
// type, they can be accessed with dot syntax.

let info = findMaxAndSize(xs: [8, -6, 2, 109, 3, 71])
assert(info.max == 109)
assert(info.size == 6)

// Tuples are particularly useful as the return values of functions.
// By returning a tuple with multiple distinct values, each of a
// different type, functions can provide more useful information
// about their outcome than if they could only return a single
// value of a single type.

// -----------------------------------------------------------------------------

// An optional tuple type such as `(Int, Int)?` is different from a tuple
// that contains optional types such as `(Int?, Int?)`.  With an optional
// tuple type, the entire tuple is optional, not just each individual
// value within the tuple.

func findMaxAndSize2(xs: [Int]) -> (max: Int, size: Int)? {
  guard !xs.isEmpty else { return nil }
  var curMax = xs[0]
  for x in xs[1..<xs.count] {
    if x > curMax {
      curMax = x
    }
  }
  return (curMax, xs.count)
}

if let info = findMaxAndSize2(xs: [8, -6, 2, 109, 3, 71]) {
  assert(info.max == 109)
  assert(info.size == 6)
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
f4()        // `parameter` will be 2.  Note the label is not used here.

// -----------------------------------------------------------------------------

// A variadic parameter accepts zero or more values of a specified type.
// A function may have at most one variadic parameter.

func f5(xs: Double...) -> Double {
  var total: Double = 1
  for x in xs {
    total *= x
  }
  return total
}

let r5 = f5(xs: 2.35, 6.7, 55.0)
assert(r5 == 865.975)

// -----------------------------------------------------------------------------

// Function parameters are constants by default.  Parameters that can be
// modified are in-out parameters.

func f6(x: inout Int) {
  x *= 2
}

var r6 = 4
// Here an ampersand is needed because we're passing in a reference to `r6`.
f6(x: &r6)
assert(r6 == 8)

// In-out parameters can't have default values, and variadic parameters
// can't be in-out.

// In-out parameters are not the same as returning a value from a function.
// In-out parameters are an alternative way for a function to have an effect
// outside of the scope of its function body.

// -----------------------------------------------------------------------------

// A function with a crazy set of parameters.  This is what not to do!
func f7(_ first: Int, foo second: Int = 4, _ xs: Int..., bar third: Int) -> Int {
  return first + second + third * xs.count
}

let r7 = f7(1, foo: 4, 1, 2, 3, 4, 5, 6, 7, bar: 3)
assert(r7 == 26)


// -----------------------------------------------------------------------------
//  Function Types
// -----------------------------------------------------------------------------

// Every function has a specific function type made up of the parameter
// types and the return type of the function.

// Here, `f10` has the type `() -> Void`.
func f8() {
  // Stuff happens.
}

// -----------------------------------------------------------------------------

// Here, `f9` and `f10` have the same type of `(Int, Int) -> Int`.
func f9(_ a: Int, _ b: Int) -> Int {
  return a + b
}
func f10(_ a: Int, _ b: Int) -> Int {
  return a * b
}

// -----------------------------------------------------------------------------

// Constants and variables can be assigned functions making them
// act like functions.

var f11: (Int, Int) -> Int = f9
let f12 = f9
let r11 = f11(2, 3)
let r12 = f12(2, 3)
assert(r11 == 5)
assert(r12 == 5)


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

// Parameter labels for the nested function are not allowed.
curValue = stepToZero(curValue)
curValue = stepToZero(curValue)
curValue = stepToZero(curValue)
curValue = stepToZero(curValue)

assert(curValue == 0)
