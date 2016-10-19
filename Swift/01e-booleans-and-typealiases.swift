// -----------------------------------------------------------------------------
//  Boolean - A value that can only ever be true or false.
// -----------------------------------------------------------------------------

let orangesAreOrange = true
let todayIsYesterday = false
let skyIsPurple = Bool(false)
let skyIsBlue: Bool = true

// -----------------------------------------------------------------------------
//  Type alias - An alternative name for an existing type.
// -----------------------------------------------------------------------------

// Type aliases are useful when you want to refer to an existing type by a name
// that is contextually more appropriate, such as when working with data of a
// specific size from an external source.

typealias AudioSample = UInt16

// Once you define a type alias you can use the alias anywhere you might
// use the original name.

var sample = AudioSample.min
assert(type(of: sample) == AudioSample.self)
assert(type(of: sample) == UInt16.self)
assert(AudioSample.self == UInt16.self)
assert(sample == 0)


// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - The Basics
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html
// =============================================================================
