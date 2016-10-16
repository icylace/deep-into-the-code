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

// Optionals are used in situations where a value might be absent.

var maybeNumber = Int("123")
assert(type(of: maybeNumber) == Int?.self)
// `maybeNumber` is inferred to be of the type `Int?`, or "optional `Int`",
// because `Int` can accept a `String` that may or may not represent a number.

// Any type can be made into an optional type.

let o1 = Bool?(false)
assert(type(of: o1) == Bool?.self)

// An optional having a value equals a non-optional that has a similar value.

assert(o1 == Bool?(false))
assert(o1 == Bool(false))
assert(o1 == false)

let o2: Int? = 8
assert(type(of: o2) == Int?.self)
assert(o2 == Int?(8))
assert(o2 == Int(8))
assert(o2 == 8)

// -----------------------------------------------------------------------------

// Another way to write an optional is by using the longhand form.

var o3: Optional<Int>
assert(Int?.self == Optional<Int>.self)
assert(type(of: o3) == Int?.self)
assert(type(of: o3) == Optional<Int>.self)
assert(o3 == nil)

// -----------------------------------------------------------------------------

// Nested optionals may be declared and assigned with any combination of
// shorthand and longhand forms.

let o4: Int?? = 8
let o5: Int?? = Int?(8)
let o6: Int?? = Optional<Int>(8)
let o7: Int?? = Int??(8)
let o8: Int?? = Optional<Int>?(8)
let o9: Int?? = Optional<Int?>(8)
let o10: Int?? = Optional<Optional<Int>>(8)
let o11: Optional<Optional<Int>> = Int??(8)
let o12: Optional<Optional<Int>> = Optional<Optional<Int>>(8)

let o13: Int??? = 8
let o14: Int??? = Optional<Int?>(8)
let o15: Optional<Int?>? = Optional<Int>??(8)
let o16: Optional<Optional<Int?>> = Optional<Optional<Int>?>(8)
let o17: Optional<Optional<Int>>? = Optional<Optional<Optional<Int>>>(8)
let o18: Optional<Optional<Optional<Int>>> = Optional<Optional<Optional<Int>>>(8)

let o19: Int???????????????????????????????????????????????????????????????? = 8

// -----------------------------------------------------------------------------
//  Forced unwrapping - An attempt at accessing an optional's underlying value.
//  Forced unwrap operator (`!`) - The operator that force-unwraps an optional.
// -----------------------------------------------------------------------------

let o20 = Int?(123)
assert(type(of: o20) == Int?.self)
assert(o20! == 123)
assert(o20 == 123)

// We know that `o20` has an integer, so it's safe to force-unwrap it.
let o21 = o20!
assert(type(of: o21) == Int.self)
// If the following is uncommented it will produce a compile-time error:
/*
assert(o21! == 123)
*/
assert(o21 == 123)

// If we assign it without force-unwrapping it we're simply passing
// the optional around.
let o22 = o20
assert(type(of: o22) == Int?.self)
assert(o22! == 123)
assert(o22 == 123)

// -----------------------------------------------------------------------------

// Nested optionals may be force-unwrapped as many levels as necessary.

assert(o4!! == 8)
assert(o4! == 8)

assert(o13!!! == 8)
assert(o13!! == 8)

assert(o19!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! == 8)
assert(o19!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! == 8)

// -----------------------------------------------------------------------------
//  nil - The special value that represents a valueless state.
// -----------------------------------------------------------------------------

var o23 = Int?(8)
assert(o23! == 8)
o23 = nil
assert(o23 == nil)

// An optional in a valueless state cannot be force-unwrapped.

// If the following is uncommented it will produce a runtime error:
/*
let o24 = o23!
*/

// nil cannot be used with non-optional constants and variables.

// If the following is uncommented it will produce a compile-time error:
/*
let o25: Int = nil
*/

// -----------------------------------------------------------------------------

// An optional variable declared without an explicit default value will be nil.

var o26: Int?
assert(o26 == nil)

// Printing an optional variable will work unlike printing an implicitly
// unwrapped optional variable, which is explained later.

print(o26)

// An optional constant must be explicitly initialized before being used.

let o27: Int?
// If the following is uncommented it will produce a compile-time error:
/*
assert(o27 == nil)
*/

let o28: Int?
o28 = nil
assert(o28 == nil)

// -----------------------------------------------------------------------------
//  Implicitly unwrapped optional - An optional treated as if it has a value.
// -----------------------------------------------------------------------------

// Implicitly unwrapped optionals are useful when an optional's value is
// is confirmed to exist after the optional is first defined and can be
// assumed to stay existing.  Their primary use is during class initialization.

// An implicitly unwrapped optional can be used like a non-optional value.

let assumedInt1: Int! = 411

let number1 = assumedInt1
assert(type(of: number1) == Int?.self)
assert(number1 == 411)
assert(number1! == 411)

let number2: Int = assumedInt1
assert(type(of: number2) == Int.self)
assert(number2 == 411)

// -----------------------------------------------------------------------------

// An implicitly unwrapped optional variable set to nil will crash if printed.

var assumedInt2: Int!
assert(assumedInt2 == nil)
// If the following is uncommented it will produce a runtime error:
/*
assert(assumedInt2! == nil)
*/
// If the following is uncommented it will produce a runtime error:
/*
print(assumedInt2)
*/
assumedInt2 = 345
assert(assumedInt2 == 345)

// -----------------------------------------------------------------------------

// An implicitly unwrapped optional constant set to nil will crash if printed.
// It's also pointless since it can't be assigned a different value.

let assumedInt3: Int! = nil
assert(assumedInt3 == nil)
// If the following is uncommented it will produce a runtime error:
/*
assert(assumedInt3! == nil)
*/
// If the following is uncommented it will produce a runtime error:
/*
print(assumedInt3)
*/
