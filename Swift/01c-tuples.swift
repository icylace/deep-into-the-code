// -----------------------------------------------------------------------------
//  Tuple type - A type that groups together multiple types.
//  Tuple value (tuple) - A value that groups together multiple values.
// -----------------------------------------------------------------------------

// Each individual value within a tuple can be of any type.  The tuple's type is
// a combination of the types of its contained values.

var httpStatus = (404, "Not Found")
assert(type(of: httpStatus) == (Int, String).self)

// -----------------------------------------------------------------------------

// The element values in a tuple can be accessed using zero-based index numbers.

assert(httpStatus.0 == 404)
assert(httpStatus.1 == "Not Found")

// -----------------------------------------------------------------------------
//  Decomposition - The separation of a tuple's contents into separate constants
//                  or variables which can then be accessed as usual.
// -----------------------------------------------------------------------------

let (code, message) = httpStatus
assert(code == 404)
assert(message == "Not Found")

var (code2, message2) = httpStatus
code2 = 200
assert(code2 == 200)
message2 = "Okay"
assert(message2 == "Okay")

// -----------------------------------------------------------------------------

// If you only need some of the tuple's values ignore the unwanted parts with
// an underscore (`_`) during decomposition.

let (statusCode, _) = httpStatus
assert(statusCode == 404)

// -----------------------------------------------------------------------------

// A tuple's individual elements can be labeled when the tuple is defined
// allowing their values to be accessed with those labels.

// If the tuple's type is not explicitly defined then the inferred tuple type
// takes on the labels of the initial tuple value assignment.

var httpOkay = (code: 200, description: "OK")
assert(httpOkay.code == 200)
assert(httpOkay.description == "OK")

// Uncommenting this will produce a runtime error:
/*
httpOkay = (c: 200, d: "OK")
*/

// Tuple indexing for labeled elements still works.

assert(httpOkay.0 == 200)
assert(httpOkay.1 == "OK")

// You don't have to label all of a tuple's elements.

let httpRedirect = (301, description: "Moved Permanently")
assert(httpRedirect.0 == 301)
assert(httpRedirect.1 == "Moved Permanently")
assert(httpRedirect.description == "Moved Permanently")

// -----------------------------------------------------------------------------

// A tuple defined with an explicit type has its element labels take precedence
// over the element labels for tuple values used with it.

let t1: (Int, Int, String) = (foo: 2, bar: 3, baz: "6")
// Uncommenting this will produce a compile-time error:
/*
assert(t1.foo == 2)
assert(t1.bar == 3)
assert(t1.baz == "6")
*/

let t2: (foo: Int, bar: Int, baz: String) = (foo: 2, bar: 3, baz: "6")
assert(t2.foo == 2)
assert(t2.bar == 3)
assert(t2.baz == "6")

let t3: (foo: Int, bar: Int, baz: String) = (2, 3, "6")
assert(t3.foo == 2)
assert(t3.bar == 3)
assert(t3.baz == "6")

let t4: (foo: Int, Int, String) = (foo: 2, bar: 3, baz: "6")
assert(t4.foo == 2)
// Uncommenting this will produce a compile-time error:
/*
assert(t4.bar == 3)
assert(t4.baz == "6")
*/

// -----------------------------------------------------------------------------

// Tuple element labels matter when accessing individual tuple elements but
// otherwise are meaningless.

assert(t1 == (2, 3, "6"))
assert(t1 == (aaaaaaaaaaaaa: 2, 3, "6"))
assert(t1 == (2, bbbbbbbbbbbbb: 3, "6"))
assert(t1 == (2, 3, ccccccccccccc: "6"))
assert(t1 == (aaa: 2, bbb: 3, ccc: "6"))

assert(t2 == (2, 3, "6"))
assert(t2 == (aaaaaaaaaaaaa: 2, 3, "6"))
assert(t2 == (2, bbbbbbbbbbbbb: 3, "6"))
assert(t2 == (2, 3, ccccccccccccc: "6"))
assert(t2 == (aaa: 2, bbb: 3, ccc: "6"))

assert(t1 == t2)

// -----------------------------------------------------------------------------

// Since tuple element labels have limited usefulness, the order in which a
// tuple's elements are used is significant.

assert(t2 == (foo: 2, bar: 3, baz: "6"))
// Uncommenting this will produce a compile-time error:
/*
assert(t2 == (baz: "6", foo: 2, bar: 3))
*/

// -----------------------------------------------------------------------------

// A tuple can have zero elements.

var t5: () = ()

// `().self` cannot be used with the equality comparison operator (`==`).

// Uncommenting this will produce a compile-time error:
/*
assert(type(of: t5) == ().self)
*/

// `Void` is a type alias for the zero-element tuple (`()`).

var t6: Void = ()

assert(type(of: t5) == Void.self)
assert(type(of: t6) == Void.self)

// Since `Void` is a type alias it can't be assigned like `()` can.

// Uncommenting this will produce a compile-time error:
/*
t5 = Void
t6 = Void
*/

// Uncommenting this will produce a compile-time error:
/*
var t7 = ()
assert(type(of: t7) == Void.self)
// Uncommenting this will produce a compile-time error:
/*
t7 = Void
*/
*/

// -----------------------------------------------------------------------------

// A single-element tuple doesn't truly exist.

let t8: Int = 2
let t9: (Int) = (2)
let t10: (((Int))) = (((2)))
assert(type(of: t8) == type(of: t9))
assert(type(of: t8) == type(of: t10))
assert(type(of: t8) == Int.self)
assert(t8 == t9)
assert(t8 == t10)
assert(t8 == 2)

// -----------------------------------------------------------------------------

// Decomposing a "single-element tuple" is pointless.

let (t11) = (3)
assert(t11 == 3)

// -----------------------------------------------------------------------------

// A "single-element tuple" doesn't affect type safety.

let t12: ((((((Int)))))) = ((2))
let t13: ((Int)) = ((((((2))))))
assert(type(of: t12) == Int.self)
assert(type(of: t13) == Int.self)
assert(t12 == 2)
assert(t13 == 2)

// Uncommenting this will produce a compile-time error:
/*
let t14: ((((((Int)))))) = ((""))
*/

// -----------------------------------------------------------------------------

// It's not possible to declare a constant or variable as a single-element
// tuple with an element label.

// Uncommenting this will produce a compile-time error:
/*
let t15: (number: Int) = 2
var t16: (number: Int) = (number: 2)
*/


// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - The Basics
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html
// =============================================================================
