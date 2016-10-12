// =============================================================================
//  The Basics
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html
// =============================================================================

// -----------------------------------------------------------------------------
//  Optional type - A type having a specific underlying type and whose values
//                  may be of that underlying type or nil otherwise.
//  Optional value (optional) - A value having either an underlying value or
//                              the lack of any value at all.
// -----------------------------------------------------------------------------

// Any type can be made into an optional type.

let o1 = Int?(8)
// `o1` is inferred to be of the type `Int?`, or "optional `Int`".
assert(type(of: o1) == Int?.self)

// An optional having a value equals a non-optional that has a similar value.

assert(o1 == Int?(8))
assert(o1 == Int(8))
assert(o1 == 8)

let o2: Int? = 8
assert(type(of: o2) == Int?.self)
assert(o2 == Int?(8))
assert(o2 == Int(8))
assert(o2 == 8)

// -----------------------------------------------------------------------------
//  Forced unwrapping - An attempt at accessing an optional's underlying value.
//  Forced unwrap operator (`!`) - The operator that force-unwraps an optional.
// -----------------------------------------------------------------------------

let o3: String? = "An optional string."
// We know that `o3` has a string, so it's safe to force-unwrap it.
let o4 = o3!
assert(type(of: o4) == String.self)
assert(o4 == "An optional string.")

// Force-unwrapping a nonexistent optional value triggers a runtime error.

let o5: String? = nil
// If the following is uncommented it will produce a runtime error:
/*
let o6 = o5!
*/





// TODO




/*






// An optional variable can be in a valueless state by assigning it the
// special value nil.

var oo: Int? = 8
assert(oo! == 8)
oo = nil
assert(oo! == nil)





assert(responseCode! == 404)
// `responseCode` contains an actual `Int` value of 404.
responseCode = nil
// `responseCode` now contains no value.

// An optional variable defined without an explicit default value will
// be set to nil.

var surveyAnswer: String?
assert(surveyAnswer == nil)

// nil cannot be used with nonoptional constants and variables.












// -----------------------------------------------------------------------------

// An optional variable declared without an explicit default value will be nil.

var o3: String?
assert(o3 == nil)

// An optional constant must be explicitly initialized before being used.

let o4: String?
// If the following is uncommented it will produce a runtime error:
/*
assert(o4 == nil)
*/

let o5: String?
o5 = nil
assert(o5 == nil)

// -----------------------------------------------------------------------------

// Another way to write an optional is by using generator syntax.

var o6: Optional<String>
assert(String?.self == Optional<String>.self)
assert(type(of: o6) == String?.self)
assert(type(of: o6) == Optional<String>.self)
assert(o6 == nil)






// TODO


// You use optionals in situations where a value might be absent.

let possibleNumber = "123"
let convertedNumber = Int(possibleNumber)
assert(type(of: convertedNumber) == Int?.self)
assert(convertedNumber! == 123)
// `convertedNumber` is inferred to be of the type `Int?` because `Int`
// can accept a `String` that may or may not represent a number.


// -----------------------------------------------------------------------------




// -----------------------------------------------------------------------------

// Nested

var maybeThing1: Int? = 3
var maybeThing2: Int?? = Int?(3)
var maybeThing3: Int??? = Int???(3)

var maybeThing33: Int??? = Optional<Int>??(3)

var maybeThing333: Int??? = Optional<Int?>(3)

var maybeThing4: Optional<Int> = 3
var maybeThing5: Optional<Optional<Int>> = Int?(3)

// TODO
// - is `Int??` notation possible?




var x: String?












// -----------------------------------------------------------------------------
//  Implicitly unwrapped optional - An optional treated as if it has a value.
// -----------------------------------------------------------------------------

// Implicitly unwrapped optionals are useful when an optional's value is
// in confirmed to exist after the optional is first defined and can be
// assumed stay existing.  The primary use of implicitly unwrapped
// optionals in Swift is during class initialization.

// An implicitly unwrapped optional is a normal optional behind the scenes, but
// can also be used like a nonoptional value, without the need for it to be
// unwrapped each time it is accessed.

let assumedString: String! = "An implicitly unwrapped optional string."
let implicitString: String = assumedString

// If an implicitly unwrapped optional is nil and you try to access its wrapped
// value, you'll trigger a runtime error.  The result is exactly the same as if
// you force-unwrap a normal optional that doesn't contain a value.

*/
