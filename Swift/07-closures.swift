// =============================================================================
//  Closures
//  https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Closures.html
// =============================================================================

// Closures are self-contained blocks of functionality that can be passed around
// and used in your code.  Closures can capture and store references to any
// constants and variables from the context in which they are defined.
// This is known as closing over those constants and variables.


// -----------------------------------------------------------------------------
//  Closure Expressions
// -----------------------------------------------------------------------------

// TODO

let names = ["Chris", "Alex", "Ewa", "Barry", "Daniella"]

func backwards(s1: String, _ s2: String) -> Bool {
  return s1 > s2
}
var reversed = names.sort(backwards)
// reversed is equal to ["Ewa", "Daniella", "Chris", "Barry", "Alex"]


// -----------------------------------------------------------------------------
//  Trailing Closures
// -----------------------------------------------------------------------------


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
