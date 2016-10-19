// =============================================================================
//  The Basics
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html
// =============================================================================

// -----------------------------------------------------------------------------
//  Tuple type - A type that groups together multiple types.
//  Tuple value (tuple) - A value that groups together multiple values.
// -----------------------------------------------------------------------------

// The values within a tuple can be of any type and do not have to be of the
// same type as each other.  The tuple's type is a combination of the types
// of the values of the tuple.

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

// If the tuple constant's or variable's type is not explicitly defined then the
// inferred tuple type uses the same element labels as the ones for the initial
// tuple value assignment.

var httpOkay = (code: 200, description: "OK")
assert(httpOkay.code == 200)
assert(httpOkay.description == "OK")

// If the following is uncommented it will produce a runtime error:
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

// If a tuple constant's or variable's type is explicitly defined then the way
// its element labels are defined takes precedence over the way element labels
// are defined for tuple values used with that constant or variable.

let t1: (Int, Int, String) = (foo: 2, bar: 3, baz: "6")
// If the following is uncommented it will produce a compile-time error:
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
// If the following is uncommented it will produce a compile-time error:
/*
assert(t4.bar == 3)
assert(t4.baz == "6")
*/

// -----------------------------------------------------------------------------

// Tuple element labels only matter when accessing individual tuple elements.
// Otherwise, they're meaningless.

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
// If the following is uncommented it will produce a compile-time error:
/*
assert(t2 == (baz: "6", foo: 2, bar: 3))
*/

// -----------------------------------------------------------------------------

// A tuple can have zero elements.

var empty1: () = ()

// `().self` cannot be used with the equality comparison operator (`==`).

// If the following is uncommented it will produce a compile-time error:
/*
assert(type(of: empty1) == ().self)
*/

// `Void` is a type alias for the zero-element tuple (`()`).

var empty2: Void = ()

assert(type(of: empty1) == Void.self)
assert(type(of: empty2) == Void.self)

// Since `Void` is a type alias it can't be assigned like `()` can.

// If the following is uncommented it will produce a compile-time error:
/*
empty1 = Void
empty2 = Void
*/

// If the following is uncommented it will produce a compile-time warning:
/*
var empty3 = ()
assert(type(of: empty3) == Void.self)
// If the following is uncommented it will produce a compile-time error:
/*
empty3 = Void
*/
*/

// -----------------------------------------------------------------------------

// A single-element tuple doesn't truly exist.

let t5: Int = 2
let t6: (Int) = (2)
let t7: (((Int))) = (((2)))
assert(type(of: t5) == type(of: t6))
assert(type(of: t5) == type(of: t7))
assert(type(of: t5) == Int.self)
assert(t5 == t6)
assert(t5 == t7)
assert(t5 == 2)

// -----------------------------------------------------------------------------

// Decomposing a "single-element tuple" is pointless.

let (t8) = (3)
assert(t8 == 3)

// -----------------------------------------------------------------------------

// A "single-element tuple" doesn't affect type safety.

let t9: ((((((Int)))))) = ((2))
let t10: ((Int)) = ((((((2))))))
assert(type(of: t9) == Int.self)
assert(type(of: t10) == Int.self)
assert(t9 == 2)
assert(t10 == 2)

// If the following is uncommented it will produce a compile-time error:
/*
let t11: ((((((Int)))))) = ((""))
*/

// -----------------------------------------------------------------------------

// It's not possible to declare a constant or variable as a single-element
// tuple with an element label.

// If the following is uncommented it will produce a compile-time error:
/*
let t12: (number: Int) = 2
var t13: (number: Int) = (number: 2)
*/
