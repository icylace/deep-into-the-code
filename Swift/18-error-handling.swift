// =============================================================================
//  Error Handling
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/ErrorHandling.html
// =============================================================================

// -----------------------------------------------------------------------------
//  Error handling - The process of responding to and recovering from
//                   error conditions.
// -----------------------------------------------------------------------------

// TODO

/*

// Some operations aren't guaranteed to always complete execution or produce a
// useful output.  Optionals are used to represent the absence of a value, but
// when an operation fails, it's often useful to understand what caused the
// failure, so that your code can respond accordingly.

// As an example, consider the task of reading and processing data from a file
// on disk.  There are a number of ways this task can fail, including the file
// not existing at the specified path, the file not having read permissions,
// etc.  Distinguishing among these different situations allows a program
// to resolve some errors and to communicate any errors it can't resolve.


// -----------------------------------------------------------------------------
//  Representing and Throwing Errors
// -----------------------------------------------------------------------------

// Errors are represented by values of types that conform to the `Error`
// protocol.

// Enumerations are particularly well suited to modeling a group of related
// error conditions, with associated values allowing for additional
// information about the nature of an error to be communicated.

enum VendingMachineError: Error {
  case invalidSelection
  case insufficientFunds(coinsNeeded: Int)
  case outOfStock
}

// Throwing an error lets you indicate that something unexpected happened and
// the normal flow of execution can't continue.

throw VendingMachineError.insufficientFunds(coinsNeeded: 5)







// -----------------------------------------------------------------------------
//  Handling Errors
// -----------------------------------------------------------------------------

// When an error is thrown, some surrounding piece of code must be responsible
// for handling the error--for example, by correcting the problem, trying an
// alternative approach, or informing the user of the failure.

// There are four ways to handle errors.  You can propagate the error from a
// function to the code that calls that function, handle the error using a
// `do-catch` statement, handle the error as an optional value, or assert
// that the error will not occur.

// When a function throws an error, it changes the flow of your program, so
// it's important that you can quickly identify places in your code that can
// throw errors.  To identify these places in your code, write the `try`
// keyword, or the `try?` or `try!` variation, before a piece of code
// that calls a function, method, or initializer that can throw
// an error.

// TODO
// - verify that subscripts aren't currently throwable

// Error handling in Swift resembles exception handling in other languages, with
// the use of the try, catch and throw keywords. Unlike exception handling in
// many languages—including Objective-C—error handling in Swift does not
// involve unwinding the call stack, a process that can be
// computationally expensive.  As such, the performance
// characteristics of a throw statement are comparable
// to those of a return statement.

// -----------------------------------------------------------------------------

// To indicate that a function, method, or initializer can throw an error,
// you write the throws keyword in the function's declaration after its
// parameters.  A function marked with throws is called a throwing
// function.  If the function specifies a return type, you write
// the throws keyword before the return arrow (`->`).

func canThrowErrors() throws -> String

func cannotThrowErrors() -> String

// A throwing function propagates errors that are thrown inside of it to the
// scope from which it's called.

// Only throwing functions can propagate errors.  Any errors thrown inside
// a nonthrowing function must be handled inside the function.

struct Item {
  var price: Int
  var count: Int
}

class VendingMachine {
  var inventory = [
    "Candy Bar": Item(price: 12, count: 7),
    "Chips": Item(price: 10, count: 4),
    "Pretzels": Item(price: 7, count: 11)
  ]
  var coinsDeposited = 0

  func vend(itemNamed name: String) throws {
    guard let item = inventory[name] else {
      throw VendingMachineError.invalidSelection
    }

    guard item.count > 0 else {
      throw VendingMachineError.outOfStock
    }

    guard item.price <= coinsDeposited else {
      throw VendingMachineError.insufficientFunds(coinsNeeded: item.price - coinsDeposited)
    }

    coinsDeposited -= item.price

    var newItem = item
    newItem.count -= 1
    inventory[name] = newItem

    print("Dispensing \(name)")
  }
}


Because the vend(itemNamed:) method propagates any errors it throws, any code that
calls this method must either handle the errors—using a do-catch statement, try?,
or try!—or continue to propagate them. For example, the
buyFavoriteSnack(person:vendingMachine:) in the
example below is also a throwing function, and
any errors that the vend(itemNamed:) method
throws will propagate up to the point where
the buyFavoriteSnack(person:vendingMachine:)
function is called.

let favoriteSnacks = [
  "Alice": "Chips",
  "Bob": "Licorice",
  "Eve": "Pretzels",
]
func buyFavoriteSnack(person: String, vendingMachine: VendingMachine) throws {
  let snackName = favoriteSnacks[person] ?? "Candy Bar"
  try vendingMachine.vend(itemNamed: snackName)
}

In this example, the buyFavoriteSnack(person: vendingMachine:) function looks
up a given person’s favorite snack and tries to buy it for them by calling
the vend(itemNamed:) method. Because the vend(itemNamed:) method can throw
an error, it’s called with the try keyword in front of it.

Throwing initializers can propagate errors in the same way as throwing
functions. For example, the initializer for the PurchasedSnack structure
in the listing below calls a throwing function as part of the initialization
process, and it handles any errors that it encounters by propagating them
to its caller.

struct PurchasedSnack {
  let name: String
  init(name: String, vendingMachine: VendingMachine) throws {
    try vendingMachine.vend(itemNamed: name)
    self.name = name
  }
}

// -----------------------------------------------------------------------------

Handling Errors Using Do-Catch

You use a do-catch statement to handle errors by running a block of code. If an
error is thrown by the code in the do clause, it is matched against the catch
clauses to determine which one of them can handle the error.

Here is the general form of a do-catch statement:

do {
  try expression
  statements
} catch pattern 1 {
  statements
} catch pattern 2 where condition {
  statements
}

You write a pattern after catch to indicate what errors that clause can handle.
If a catch clause doesn’t have a pattern, the clause matches any error and binds
the error to a local constant named error. For more information about pattern
matching, see Patterns.

The catch clauses don’t have to handle every possible error that the code in its
do clause can throw. If none of the catch clauses handle the error, the error
propagates to the surrounding scope. However, the error must be handled by some
surrounding scope—either by an enclosing do-catch clause that handles the error
or by being inside a throwing function. For example, the following code handles
all three cases of the VendingMachineError enumeration, but all other errors
have to be handled by its surrounding scope:

var vendingMachine = VendingMachine()
vendingMachine.coinsDeposited = 8
do {
  try buyFavoriteSnack(person: "Alice", vendingMachine: vendingMachine)
} catch VendingMachineError.invalidSelection {
  print("Invalid Selection.")
} catch VendingMachineError.outOfStock {
  print("Out of Stock.")
} catch VendingMachineError.insufficientFunds(let coinsNeeded) {
  print("Insufficient funds. Please insert an additional \(coinsNeeded) coins.")
}
// Prints "Insufficient funds. Please insert an additional 2 coins."

In the above example, the buyFavoriteSnack(person:vendingMachine:) function is
called in a try expression, because it can throw an error. If an error is thrown,
execution immediately transfers to the catch clauses, which decide whether to
allow propagation to continue. If no error is thrown, the remaining statements
in the do statement are executed.

// -----------------------------------------------------------------------------

Converting Errors to Optional Values

You use try? to handle an error by converting it to an optional value. If an error
is thrown while evaluating the try? expression, the value of the expression is nil.
For example, in the following code x and y have the same value and behavior:

func someThrowingFunction() throws -> Int {
  // ...
}

let x = try? someThrowingFunction()

let y: Int?
do {
  y = try someThrowingFunction()
} catch {
  y = nil
}

If someThrowingFunction() throws an error, the value of x and y is nil. Otherwise,
the value of x and y is the value that the function returned. Note that x and y
are an optional of whatever type someThrowingFunction() returns. Here the function
returns an integer, so x and y are optional integers.

Using try? lets you write concise error handling code when you want to handle all
errors in the same way. For example, the following code uses several approaches
to fetch data, or returns nil if all of the approaches fail.

func fetchData() -> Data? {
  if let data = try? fetchDataFromDisk() { return data }
  if let data = try? fetchDataFromServer() { return data }
  return nil
}

// -----------------------------------------------------------------------------

Disabling Error Propagation

Sometimes you know a throwing function or method won’t, in fact, throw an error
at runtime. On those occasions, you can write try! before the expression to
disable error propagation and wrap the call in a runtime assertion that no
error will be thrown. If an error actually is thrown, you’ll get a runtime
error.

For example, the following code uses a loadImage(atPath:) function, which loads
the image resource at a given path or throws an error if the image can’t be
loaded. In this case, because the image is shipped with the application, no
error will be thrown at runtime, so it is appropriate to disable error
propagation.

let photo = try! loadImage(atPath: "./Resources/John Appleseed.jpg")






// -----------------------------------------------------------------------------
//  Specifying Cleanup Actions
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
//  `defer` statement - The statement that executes a set of statements just
//                      before code execution leaves the current code block.
// -----------------------------------------------------------------------------

// A `defer` statement lets you do any necessary cleanup that should be
// performed regardless of how execution leaves the current block of
// code - whether it leaves because an error was thrown or because
// of a statement such as return or break.  For example, you can
// use a defer statement to ensure that file descriptors are
// closed and manually allocated memory is freed.

// A `defer` statement defers execution until the current scope is exited.
// This statement consists of the `defer` keyword and the statements to be
// executed later.  The deferred statements may not contain any code that
// would transfer control out of the statements, such as a `break` or a
// `return` statement, or by throwing an error.  Deferred actions are
// executed in reverse order of how they are specified-that is, the
// code in the first defer statement executes after code in the
// second, and so on.

func processFile(filename: String) throws {
  if exists(filename) {
    let file = open(filename)
    defer {
      close(file)
    }
    while let line = try file.readline() {
      // Work with the file.
    }
    // `close(file)` is called here, at the end of the scope.
  }
}

// The above example uses `defer` to ensure that the `open(_:)` function has a
// corresponding call to `close(_:)`.

// You can use `defer` even when no error handling code is involved.












//assert()
//assertionFailure()
//precondition()
//preconditionFailure()
//fatalError()

//abort()
//exit(1)

enum MyError: ErrorType {
  case Alpha
  case Bravo
  case Tango
}


//do {
////  let theResult = try obj.doDangerousStuff()
//}
//catch MyError.Alpha {
//  // Deal with badness.
//}
//catch MyError.Bravo {
//  // Deal with terribleness.
//}
//catch is ErrorType {
//  // Unexpected error!
//}


defer {}









// -----------------------------------------------------------------------------
//  Error Handling
// -----------------------------------------------------------------------------

// TODO


// You use error handling to respond to error conditions your program may
// encounter during execution.

// In contrast to optionals, which can use the presence or absence of a value to
// communicate success or failure of a function, error handling allows you to
// determine the underlying cause of failure, and, if necessary, propagate the
// error to another part of your program.

// When a function encounters an error condition, it throws an error.  That
// function’s caller can then catch the error and respond appropriately.

func canThrowAnError() throws {
  // this function may or may not throw an error
}

// A function indicates that it can throw an error by including the throws
// keyword in its declaration. When you call a function that can throw an
// error, you prepend the try keyword to the expression.

// Swift automatically propagates errors out of their current scope until
// they are handled by a catch clause.

do {
  try canThrowAnError()
  // no error was thrown
} catch {
  // an error was thrown
}

// A do statement creates a new containing scope, which allows errors to be
// propagated to one or more catch clauses.

// Here’s an example of how error handling can be used to respond to different
// error conditions:

func makeASandwich() throws {
  // ...
}

do {
  try makeASandwich()
  eatASandwich()
} catch SandwichError.outOfCleanDishes {
  washDishes()
} catch SandwichError.missingIngredients(let ingredients) {
  buyGroceries(ingredients)
}

// In this example, the makeASandwich() function will throw an error if no clean
// dishes are available or if any ingredients are missing. Because makeASandwich()
// can throw an error, the function call is wrapped in a try expression. By wrapping
// the function call in a do statement, any errors that are thrown will be propagated
// to the provided catch clauses.

// If no error is thrown, the eatASandwich() function is called. If an error is
// thrown and it matches the SandwichError.outOfCleanDishes case, then the
// washDishes() function will be called. If an error is thrown and it matches
// the SandwichError.missingIngredients case, then the buyGroceries(_:)
// function is called with the associated [String] value captured by
// the catch pattern.

*/
