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

// TODO

// You use optionals in situations where a value might be absent.

let possibleNumber = "123"
let convertedNumber = Int(possibleNumber)
assert(type(of: convertedNumber) == Int?.self)
assert(convertedNumber! == 123)
// `convertedNumber` is inferred to be of type `Int?`, or "optional `Int`",
// because `Int` can accept a `String` that may or may not represent
// a number.

// -----------------------------------------------------------------------------

var maybe = Int?(3)
assert(type(of: maybe) == Int?.self)

var might: Int? = 3
assert(type(of: might) == Int?.self)

var could: String?
assert(type(of: could) == String?.self)
assert(could == nil)

// Another way to write an optional is by using generator syntax.

var would: Optional<String>
assert(String?.self == Optional<String>.self)
assert(type(of: would) == String?.self)
assert(type(of: would) == Optional<String>.self)
assert(would == nil)

// -----------------------------------------------------------------------------

// An optional variable can be in a valueless state by assigning it the
// special value nil.

var responseCode: Int? = 404
assert(type(of: responseCode) == Int?.self)
assert(responseCode! == 404)
// `responseCode` contains an actual `Int` value of 404.
responseCode = nil
// `responseCode` now contains no value.

// An optional variable defined without an explicit default value will
// be set to nil.

var surveyAnswer: String?
assert(surveyAnswer == nil)

// nil cannot be used with nonoptional constants and variables.








// var maybeThing: Optional<Int> = 3
// var maybeThing: Optional<Optional<Int>> = Int?(3)
// Int??




var x: String?




// -----------------------------------------------------------------------------
//  Forced unwrapping - An attempt at accessing an optional's underlying value.
//  Forced unwrap operator (`!`) - The operator that force-unwraps an optional.
// -----------------------------------------------------------------------------

let maybeText: String? = "An optional string."

// We know that `maybeText` has a string, so it's safe to force-unwrap it.
let forcedText = maybeText!
assert(type(of: forcedText) == String.self)
assert(forcedText == "An optional string.")

// Trying to use `!` to access a nonexistent optional value triggers a runtime
// error.

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
