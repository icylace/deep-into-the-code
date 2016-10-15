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

// -----------------------------------------------------------------------------
//  nil - The special value that represents a valueless state.
// -----------------------------------------------------------------------------

var o5 = Int?(8)
assert(o5! == 8)
o5 = nil
assert(o5 == nil)

// An optional in a valueless state cannot be force-unwrapped.

// If the following is uncommented it will produce a runtime error:
/*
let o6 = o5!
*/

// nil cannot be used with non-optional constants and variables.

// If the following is uncommented it will produce a runtime error:
/*
let o7: Int = nil
*/

// -----------------------------------------------------------------------------

// An optional variable declared without an explicit default value will be nil.

var o8: String?
assert(o8 == nil)

// An optional constant must be explicitly initialized before being used.

let o9: String?
// If the following is uncommented it will produce a runtime error:
/*
assert(o9 == nil)
*/

let o10: String?
o10 = nil
assert(o10 == nil)

// -----------------------------------------------------------------------------

// Another way to write an optional is by using generator syntax.

var o11: Optional<String>
assert(String?.self == Optional<String>.self)
assert(type(of: o11) == String?.self)
assert(type(of: o11) == Optional<String>.self)
assert(o11 == nil)

// -----------------------------------------------------------------------------




// TODO


/*

// You use optionals in situations where a value might be absent.

let possibleNumber = "123"
let convertedNumber = Int(possibleNumber)
assert(type(of: convertedNumber) == Int?.self)
assert(convertedNumber! == 123)
// `convertedNumber` is inferred to be of the type `Int?` because `Int`
// can accept a `String` that may or may not represent a number.




// -----------------------------------------------------------------------------

// Nested optionals

var maybeThing1: Int? = 3
var maybeThing2: Int?? = Int?(3)
var maybeThing3: Int??? = Int???(3)

var maybeThing33: Int??? = Optional<Int>??(3)

var maybeThing333: Int??? = Optional<Int?>(3)

var maybeThing4: Optional<Int> = 3
var maybeThing5: Optional<Optional<Int>> = Int?(3)


*/





// -----------------------------------------------------------------------------
//  Implicitly unwrapped optional - An optional treated as if it has a value.
// -----------------------------------------------------------------------------

// Implicitly unwrapped optionals are useful when an optional's value is
// in confirmed to exist after the optional is first defined and can be
// assumed stay existing.  The primary use of implicitly unwrapped
// optionals in Swift is during class initialization.

// An implicitly unwrapped optional can be used like a non-optional value.

let assumedString: String! = "An implicitly unwrapped optional string."
let implicitString: String = assumedString

// If an implicitly unwrapped optional is nil and you try to access its wrapped
// value, you'll trigger a runtime error.  The result is exactly the same as if
// you force-unwrap a normal optional that doesn't contain a value.
