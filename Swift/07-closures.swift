// =============================================================================
//  Closures
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Closures.html
// =============================================================================

// -----------------------------------------------------------------------------
//  Closure - A self-contained block of functionality that can capture and store
//            references to any constants and variables from the context in
//            which they are defined.  This is known as closing over those
//            constants and variables.
// -----------------------------------------------------------------------------

// Global and nested functions are actually special cases of closures.
// Closures take one of three forms:
//
// - Global function - A named closure that does not capture any values.
// - Nested function - A named closure that can capture values from its
//                     enclosing function.
// - Closure expression - An unnamed closure that can capture values from
//                        its surrounding context.


// -----------------------------------------------------------------------------
//  Closure Expressions
// -----------------------------------------------------------------------------

// TODO

// Closure expressions are a way to write inline closures in a brief, focused
// syntax. Closure expressions provide several syntax optimizations for writing
// closures in a shortened form without loss of clarity or intent.


let names = ["Chris", "Alex", "Ewa", "Barry", "Daniella"]

func backwards(s1: String, _ s2: String) -> Bool {
  return s1 > s2
}

var reversed = names.sort(backwards)
// reversed is equal to ["Ewa", "Daniella", "Chris", "Barry", "Alex"]






// The Sorted Method

// The Swift Standard Library provides a method called [`sorted(by:)`](https://developer.apple.com/reference/swift/sequence/1641066-sorted),
// which


//



// sorts an array of values of a known type, based on the output of a
// sorting closure that you provide.  Once it completes the sorting process,
// the `sorted(by:)` method returns a new array of the same type and size
// as the old one, with its elements in the correct sorted order. The
// original array is not modified by the sorted(by:) method.



let names = ["Chris", "Alex", "Ewa", "Barry", "Daniella"]
The sorted(by:) method accepts a closure that takes two arguments of the same type as the array’s contents, and returns a Bool value to say whether the first value should appear before or after the second value once the values are sorted. The sorting closure needs to return true if the first value should appear before the second value, and false otherwise.

This example is sorting an array of String values, and so the sorting closure needs to be a function of type (String, String) -> Bool.

One way to provide the sorting closure is to write a normal function of the correct type, and to pass it in as an argument to the sorted(by:) method:

func backward(_ s1: String, _ s2: String) -> Bool {
    return s1 > s2
}
var reversedNames = names.sorted(by: backward)
// reversedNames is equal to ["Ewa", "Daniella", "Chris", "Barry", "Alex"]
If the first string (s1) is greater than the second string (s2), the backward(_:_:) function will return true, indicating that s1 should appear before s2 in the sorted array. For characters in strings, “greater than” means “appears later in the alphabet than”. This means that the letter "B" is “greater than” the letter "A", and the string "Tom" is greater than the string "Tim". This gives a reverse alphabetical sort, with "Barry" being placed before "Alex", and so on.

However, this is a rather long-winded way to write what is essentially a single-expression function (a > b). In this example, it would be preferable to write the sorting closure inline, using closure expression syntax.

Closure Expression Syntax

Closure expression syntax has the following general form:

{ (parameters) -> return type in
    statements
}
The parameters in closure expression syntax can be in-out parameters, but they can’t have a default value. Variadic parameters can be used if you name the variadic parameter. Tuples can also be used as parameter types and return types.

The example below shows a closure expression version of the backward(_:_:) function from earlier:

reversedNames = names.sorted(by: { (s1: String, s2: String) -> Bool in
    return s1 > s2
})
Note that the declaration of parameters and return type for this inline closure is identical to the declaration from the backward(_:_:) function. In both cases, it is written as (s1: String, s2: String) -> Bool. However, for the inline closure expression, the parameters and return type are written inside the curly braces, not outside of them.

The start of the closure’s body is introduced by the in keyword. This keyword indicates that the definition of the closure’s parameters and return type has finished, and the body of the closure is about to begin.

Because the body of the closure is so short, it can even be written on a single line:

reversedNames = names.sorted(by: { (s1: String, s2: String) -> Bool in return s1 > s2 } )
This illustrates that the overall call to the sorted(by:) method has remained the same. A pair of parentheses still wrap the entire argument for the method. However, that argument is now an inline closure.

Inferring Type From Context

Because the sorting closure is passed as an argument to a method, Swift can infer the types of its parameters and the type of the value it returns. The sorted(by:) method is being called on an array of strings, so its argument must be a function of type (String, String) -> Bool. This means that the (String, String) and Bool types do not need to be written as part of the closure expression’s definition. Because all of the types can be inferred, the return arrow (->) and the parentheses around the names of the parameters can also be omitted:

reversedNames = names.sorted(by: { s1, s2 in return s1 > s2 } )
It is always possible to infer the parameter types and return type when passing a closure to a function or method as an inline closure expression. As a result, you never need to write an inline closure in its fullest form when the closure is used as a function or method argument.

Nonetheless, you can still make the types explicit if you wish, and doing so is encouraged if it avoids ambiguity for readers of your code. In the case of the sorted(by:) method, the purpose of the closure is clear from the fact that sorting is taking place, and it is safe for a reader to assume that the closure is likely to be working with String values, because it is assisting with the sorting of an array of strings.

Implicit Returns from Single-Expression Closures

Single-expression closures can implicitly return the result of their single expression by omitting the return keyword from their declaration, as in this version of the previous example:

reversedNames = names.sorted(by: { s1, s2 in s1 > s2 } )
Here, the function type of the sorted(by:) method’s argument makes it clear that a Bool value must be returned by the closure. Because the closure’s body contains a single expression (s1 > s2) that returns a Bool value, there is no ambiguity, and the return keyword can be omitted.

Shorthand Argument Names

Swift automatically provides shorthand argument names to inline closures, which can be used to refer to the values of the closure’s arguments by the names $0, $1, $2, and so on.

If you use these shorthand argument names within your closure expression, you can omit the closure’s argument list from its definition, and the number and type of the shorthand argument names will be inferred from the expected function type. The in keyword can also be omitted, because the closure expression is made up entirely of its body:

reversedNames = names.sorted(by: { $0 > $1 } )
Here, $0 and $1 refer to the closure’s first and second String arguments.

Operator Methods

There’s actually an even shorter way to write the closure expression above. Swift’s String type defines its string-specific implementation of the greater-than operator (>) as a method that has two parameters of type String, and returns a value of type Bool. This exactly matches the method type needed by the sorted(by:) method. Therefore, you can simply pass in the greater-than operator, and Swift will infer that you want to use its string-specific implementation:

reversedNames = names.sorted(by: >)
For more about operator method, see Operator Methods.












// A closure...

// ...variable.
var closure: (ParameterTypes) -> (ReturnType)

// ...optional variable.
var closure: ((ParameterTypes) -> (ReturnType))?

// ...type alias.
typealias Closure = (ParameterTypes) -> (ReturnType)

// ...constant.
let closure: ClosureType = { ... }

// ...argument to a function call.
_ = foo({ (ParameterTypes) -> (ReturnType) in statements })

// -----------------------------------------------------------------------------

// A closure function parameter with full syntax.
_ = array.sorted({ (item1: Int, item2: Int) -> Bool in return item1 < item2 })

// -----------------------------------------------------------------------------

// A closure function parameter...

// ...with implied types,...
_ = array.sorted({ (item1, item2) -> Bool in return item1 < item2 })

// ...and with implied return type,...
_ = array.sorted({ (item1, item2) in return item1 < item2 })

// ...as a trailing closure,...
_ = array.sorted { (item1, item2) in return item1 < item2 }

// ...using shorthand argument names,...
_ = array.sorted { return $0 < $1 }

// ...using an implied return value.
_ = array.sorted { $0 < $1 }

// -----------------------------------------------------------------------------

// A closure function parameter as a reference to an existing function.
_ = array.sorted(<)

// -----------------------------------------------------------------------------

// A closure function parameter...

// ...with explicit capture semantics...
_ = array.sorted({ [unowned self] (item1: Int, item2: Int) -> Bool in return item1 < item2 })

// ...and inferred parameters / return type.
_ = array.sorted({ [unowned self] in return $0 < $1 })










// -----------------------------------------------------------------------------
//  Trailing Closures
// -----------------------------------------------------------------------------

// If you need to pass a closure expression to a function as the function's
// final argument and the closure expression is long, it can be useful to
// write it as a trailing closure instead. A trailing closure
// is written after the function call’s parentheses, even though it is still
// an argument to the function. When you use the trailing closure syntax, you don’t write the argument label for the closure as part of the function call.

func someFunctionThatTakesAClosure(closure: () -> Void) {
    // function body goes here
}

// Here's how you call this function without using a trailing closure:

someFunctionThatTakesAClosure(closure: {
    // closure's body goes here
})

// Here's how you call this function with a trailing closure instead:

someFunctionThatTakesAClosure() {
    // trailing closure's body goes here
}
The string-sorting closure from the Closure Expression Syntax section above can be written outside of the sorted(by:) method’s parentheses as a trailing closure:

reversedNames = names.sorted() { $0 > $1 }
If a closure expression is provided as the function or method’s only argument and you provide that expression as a trailing closure, you do not need to write a pair of parentheses () after the function or method’s name when you call the function:

reversedNames = names.sorted { $0 > $1 }
Trailing closures are most useful when the closure is sufficiently long that it is not possible to write it inline on a single line. As an example, Swift’s Array type has a map(_:) method which takes a closure expression as its single argument. The closure is called once for each item in the array, and returns an alternative mapped value (possibly of some other type) for that item. The nature of the mapping and the type of the returned value is left up to the closure to specify.

After applying the provided closure to each array element, the map(_:) method returns a new array containing all of the new mapped values, in the same order as their corresponding values in the original array.

Here’s how you can use the map(_:) method with a trailing closure to convert an array of Int values into an array of String values. The array [16, 58, 510] is used to create the new array ["OneSix", "FiveEight", "FiveOneZero"]:

let digitNames = [
    0: "Zero", 1: "One", 2: "Two",   3: "Three", 4: "Four",
    5: "Five", 6: "Six", 7: "Seven", 8: "Eight", 9: "Nine"
]
let numbers = [16, 58, 510]
The code above creates a dictionary of mappings between the integer digits and English-language versions of their names. It also defines an array of integers, ready to be converted into strings.

You can now use the numbers array to create an array of String values, by passing a closure expression to the array’s map(_:) method as a trailing closure:

let strings = numbers.map {
    (number) -> String in
    var number = number
    var output = ""
    repeat {
        output = digitNames[number % 10]! + output
        number /= 10
    } while number > 0
    return output
}
// strings is inferred to be of type [String]
// its value is ["OneSix", "FiveEight", "FiveOneZero"]
The map(_:) method calls the closure expression once for each item in the array. You do not need to specify the type of the closure’s input parameter, number, because the type can be inferred from the values in the array to be mapped.

In this example, the variable number is initialized with the value of the closure’s number parameter, so that the value can be modified within the closure body. (The parameters to functions and closures are always constants.) The closure expression also specifies a return type of String, to indicate the type that will be stored in the mapped output array.

The closure expression builds a string called output each time it is called. It calculates the last digit of number by using the remainder operator (number % 10), and uses this digit to look up an appropriate string in the digitNames dictionary. The closure can be used to create a string representation of any integer greater than zero.

NOTE

The call to the digitNames dictionary’s subscript is followed by an exclamation mark (!), because dictionary subscripts return an optional value to indicate that the dictionary lookup can fail if the key does not exist. In the example above, it is guaranteed that number % 10 will always be a valid subscript key for the digitNames dictionary, and so an exclamation mark is used to force-unwrap the String value stored in the subscript’s optional return value.

The string retrieved from the digitNames dictionary is added to the front of output, effectively building a string version of the number in reverse. (The expression number % 10 gives a value of 6 for 16, 8 for 58, and 0 for 510.)

The number variable is then divided by 10. Because it is an integer, it is rounded down during the division, so 16 becomes 1, 58 becomes 5, and 510 becomes 51.

The process is repeated until number is equal to 0, at which point the output string is returned by the closure, and is added to the output array by the map(_:) method.

The use of trailing closure syntax in the example above neatly encapsulates the closure’s functionality immediately after the function that closure supports, without needing to wrap the entire closure within the map(_:) method’s outer parentheses.












// -----------------------------------------------------------------------------
//  Capturing Values
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Closures Are Reference Types
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Nonescaping Closures
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Autoclosures
// -----------------------------------------------------------------------------
