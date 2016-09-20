// =============================================================================
//  Strings and Characters
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html
// =============================================================================


// -----------------------------------------------------------------------------
//  String Literals
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Initializing an Empty String
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  String Mutability
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Strings Are Value Types
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Working with Characters
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Concatenating Strings and Characters
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  String Interpolation
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Unicode
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Counting Characters
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Accessing and Modifying a String
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Comparing Strings
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Unicode Representations of Strings
// -----------------------------------------------------------------------------










// This is a string which is basically just text.
"Hello world!"



// http://stackoverflow.com/a/34983398/1935675
print(UnicodeScalar("A").value)



// ----

// https://www.drivenbycode.com/the-missing-apply-function-in-swift/
func repeatIt(_ str: String, _ n: Int) -> String {
  return [String](repeating: str, count: n).joined(separator: "")
}

print(repeatIt("ha", 3)) // -> "hahaha"
