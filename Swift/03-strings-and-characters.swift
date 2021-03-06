// -----------------------------------------------------------------------------
//  Character - A unit of data representing either text or a control code.
//  Control code - A non-visual character representing a special action to take.
//  String - A series of characters.
//  Unicode - An international standard for encoding, representing, and
//            processing text in different writing systems.
// -----------------------------------------------------------------------------

// The `String` and `Character` types provide a fully Unicode-compliant way to
// work with text in your code.

// Unicode enables you to represent almost any character from any language in
// a standardized form, and to read and write those characters to and from an
// external source such as a text file or web page.




// TODO

// The contents of a string can be accessed in various ways, including as a
// collection of `Character` values.


// Every string is composed of encoding-independent Unicode characters
// and provides support for accessing those characters in various
// Unicode representations.

// Swift's `String` type is bridged with Foundation's `NSString` class.
// Foundation also extends `String` to expose methods defined by
// `NSString`.  This means if you import Foundation you can
// access those `NSString` methods on `String`
// without casting.

// -----------------------------------------------------------------------------
//  String literal - A literal value representing a predefined string.
// -----------------------------------------------------------------------------

let s1 = "hello"
assert(type(of: s1) == String.self)




// TODO





// -----------------------------------------------------------------------------

// An empty string created with a string literal.
let empty1 = ""

// An empty string created with a `String` initializer.
let empty2 = String()

assert(empty1 == empty2)

// Find out whether a string is empty by checking its `isEmpty` property.

assert(empty1.isEmpty == true)






// -----------------------------------------------------------------------------

var variableString = "Horse"
variableString += " and carriage"
// variableString is now "Horse and carriage"










// -----------------------------------------------------------------------------
//  Strings Are Value Types
// -----------------------------------------------------------------------------

// If you create a new `String` value, it's copied when it's passed to a
// function or method, or when it's assigned to a constant or variable.
// In each case, a new copy of the existing `String` value is created,
// and the new copy is passed or assigned, not the original version.

// Value types are described in Structures and Enumerations Are Value Types.

// Swift's copy-by-default `String` behavior ensures that when a function or method
// passes you a `String` value, it is clear that you own that exact `String` value,
// regardless of where it came from.  You can be confident that the string you
// are passed will not be modified unless you modify it yourself.








// -----------------------------------------------------------------------------
//  Working with Characters
// -----------------------------------------------------------------------------

// You can access a string's individual `Character` values by iterating
// over its `characters` property.

for character in "Dog!🐶".characters {
  print(character)
}
// Output:
// D
// o
// g
// !
// 🐶

// The for-in loop is described in For-In Loops.

// You can create a stand-alone `Character` constant or variable from a single
// -character string literal by providing a `Character` type annotation:

let exclamationMark: Character = "!"

// `String` values can be constructed by passing an array of `Character` values
// as an argument to the `String` initializer.

let catCharacters: [Character] = ["C", "a", "t", "!", "🐱"]
let catString = String(catCharacters)
assert(catString == "Cat!🐱")






// -----------------------------------------------------------------------------
//  Concatenation - The act of adding strings together.
// -----------------------------------------------------------------------------

// Strings can be concatenated with the addition operator (`+`).

let string1 = "hello"
let string2 = " there"
var welcome = string1 + string2
assert(welcome == "hello there")

// Strings can be concatenated with the addition assignment operator (`+=`).

var instruction = "look over"
instruction += string2
assert(instruction == "look over there")

// You can append a `Character` value to a `String` variable with the `String`
// type's `append()` method.

let exclamationMark2: Character = "!"
welcome.append(exclamationMark2)
assert(welcome == "hello there!")

// You can't append a `String` or `Character` to an existing `Character`
// variable, because a `Character` value must contain a single
// character only.





// -----------------------------------------------------------------------------
//  String interpolation - A way of constructing a new `String` value from a mix
//                         of expressions included inside a string literal.
// -----------------------------------------------------------------------------

// Each item you insert into the string literal is wrapped in a pair
// of parentheses, prefixed by a backslash:

let multiplier = 3
let message = "\(multiplier) times 2.5 is \(Double(multiplier) * 2.5)"
// message is "3 times 2.5 is 7.5"

// The value of multiplier is also part of a larger expression later in the string.
// This expression calculates the value of Double(multiplier) * 2.5 and inserts the
// result (7.5) into the string. In this case, the expression is written as
// \(Double(multiplier) * 2.5) when it is included inside the string
// literal.
//
// The expressions you write inside parentheses within an interpolated string
// cannot contain an unescaped backslash (\), a carriage return, or a line
// feed.  However, they can contain other string literals.





// -----------------------------------------------------------------------------

// Unicode Scalars
//
// Behind the scenes, Swift’s native String type is built from Unicode scalar
// values.  A Unicode scalar is a unique 21-bit number for a character or
// modifier, such as U+0061 for LATIN SMALL LETTER A ("a"), or U+1F425
// for FRONT-FACING BABY CHICK ("🐥").
//
// NOTE
//
// A Unicode scalar is any Unicode code point in the range U+0000 to U+D7FF
// inclusive or U+E000 to U+10FFFF inclusive.  Unicode scalars do not
// include the Unicode surrogate pair code points which are the code
// points in the range U+D800 to U+DFFF inclusive.
//
// Note that not all 21-bit Unicode scalars are assigned to a character.
// Some scalars are reserved for future assignment.  Scalars that have
// been assigned to a character typically also have a name, such as
// LATIN SMALL LETTER A and FRONT-FACING BABY CHICK in the examples
// above.

// -----------------------------------------------------------------------------

// Special Characters in String Literals
//
// String literals can include the following special characters:
//
// The escaped special characters `\0` (null character), `\\` (backslash), `\t`
// (horizontal tab), `\n` (line feed), `\r` (carriage return), `\"` (double quote)
// and `\'` (single quote).
//
// An arbitrary Unicode scalar, written as \u{n}, where n is a 1–8 digit
// hexadecimal number with a value equal to a valid Unicode code point.
//
// The code below shows four examples of these special characters . The wiseWords
// constant contains two escaped double quote characters.  The dollarSign,
// blackHeart, and sparklingHeart constants demonstrate the Unicode
// scalar format:

let wiseWords = "\"Imagination is more important than knowledge\" - Einstein"
// "Imagination is more important than knowledge" - Einstein
let dollarSign = "\u{24}"        // $,  Unicode scalar U+0024
let blackHeart = "\u{2665}"      // ♥,  Unicode scalar U+2665
let sparklingHeart = "\u{1F496}" // 💖, Unicode scalar U+1F496




// Extended Grapheme Clusters
//
// Every instance of Swift’s Character type represents a single extended grapheme
// cluster.  An extended grapheme cluster is a sequence of one or more Unicode
// scalars that (when combined) produce a single human-readable character.
//
// Here's an example. The letter é can be represented as the single Unicode scalar
// é (LATIN SMALL LETTER E WITH ACUTE, or U+00E9).  However, the same letter can
// also be represented as a pair of scalars—a standard letter e (LATIN SMALL
// LETTER E, or U+0065), followed by the COMBINING ACUTE ACCENT scalar
// (U+0301).  The COMBINING ACUTE ACCENT scalar is graphically applied
// to the scalar that precedes it, turning an e into an é when it is
// rendered by a Unicode-aware text-rendering system.

// In both cases, the letter é is represented as a single Swift Character value
// that represents an extended grapheme cluster.  In the first case, the
// cluster contains a single scalar; in the second case, it is a cluster
// of two scalars:

let eAcute: Character = "\u{E9}"                         // é
let combinedEAcute: Character = "\u{65}\u{301}"          // e followed by ́
// eAcute is é, combinedEAcute is é

// Extended grapheme clusters are a flexible way to represent many complex script
// characters as a single Character value.  For example, Hangul syllables from
// the Korean alphabet can be represented as either a precomposed or
// decomposed sequence.  Both of these representations qualify as a
// single Character value in Swift:

let precomposed: Character = "\u{D55C}"                  // 한
let decomposed: Character = "\u{1112}\u{1161}\u{11AB}"   // ᄒ, ᅡ, ᆫ
// precomposed is 한, decomposed is 한

// Extended grapheme clusters enable scalars for enclosing marks (such as COMBINING
// ENCLOSING CIRCLE, or U+20DD) to enclose other Unicode scalars as part of a
// single Character value:

let enclosedEAcute: Character = "\u{E9}\u{20DD}"
// enclosedEAcute is é⃝

// Unicode scalars for regional indicator symbols can be combined in pairs to make
// a single Character value, such as this combination of REGIONAL INDICATOR SYMBOL
// LETTER U (U+1F1FA) and REGIONAL INDICATOR SYMBOL LETTER S (U+1F1F8):

let regionalIndicatorForUS: Character = "\u{1F1FA}\u{1F1F8}"
// regionalIndicatorForUS is 🇺🇸







// -----------------------------------------------------------------------------
//  Counting Characters
// -----------------------------------------------------------------------------

// To retrieve a count of the `Character` values in a string, use the `count`
// property of the string's `characters` property:

let unusualMenagerie = "Koala 🐨, Snail 🐌, Penguin 🐧, Dromedary 🐪"
assert(unusualMenagerie.characters.count == 40)

// Note that Swift's use of extended grapheme clusters for `Character` values means
// that string concatenation and modification may not always affect a string's
// character count.
//
// For example, if you initialize a new string with the four-character word cafe,
// and then append a COMBINING ACUTE ACCENT (U+0301) to the end of the string,
// the resulting string will still have a character count of 4, with a fourth
// character of é, not e:

var word = "cafe"
print("the number of characters in \(word) is \(word.characters.count)")
// Prints "the number of characters in cafe is 4"

word += "\u{301}"    // COMBINING ACUTE ACCENT, U+0301

print("the number of characters in \(word) is \(word.characters.count)")
// Prints "the number of characters in café is 4"




// NOTE
//
// Extended grapheme clusters can be composed of one or more Unicode scalars.
// This means that different characters—and different representations of the
// same character—can require different amounts of memory to store.  Because
// of this, characters in Swift do not each take up the same amount of
// memory within a string's representation.  As a result, the number
// of characters in a string cannot be calculated without iterating
// through the string to determine its extended grapheme cluster
// boundaries. If you are working with particularly long string
// values, be aware that the characters property must iterate
// over the Unicode scalars in the entire string in order to
// determine the characters for that string.
//
// The count of the characters returned by the characters property is not always
// the same as the length property of an NSString that contains the same
// characters. The length of an NSString is based on the number of
// 16-bit code units within the string’s UTF-16 representation and
// not the number of Unicode extended grapheme clusters within
// the string.

// -----------------------------------------------------------------------------
//  Accessing and Modifying a String
// -----------------------------------------------------------------------------

// You access and modify a string through its methods and properties, or by using subscript syntax.

// -----------------------------------------------------------------------------

// String Indices
//
// Each String value has an associated index type, String.Index, which corresponds to the position of each Character in the string.
//
// As mentioned above, different characters can require different amounts of memory to store, so in order to determine which Character is at a particular position, you must iterate over each Unicode scalar from the start or end of that String. For this reason, Swift strings cannot be indexed by integer values.
//
// Use the startIndex property to access the position of the first Character of a String. The endIndex property is the position after the last character in a String. As a result, the endIndex property isn’t a valid argument to a string’s subscript. If a String is empty, startIndex and endIndex are equal.
//
// You access the indices before and after a given index using the index(before:) and index(after:) methods of String. To access an index farther away from the given index, you can use the index(_:offsetBy:) method instead of calling one of these methods multiple times.
//
// You can use subscript syntax to access the Character at a particular String index.

let greeting = "Guten Tag!"
assert(greeting[greeting.startIndex] == "G")
assert(greeting[greeting.index(before: greeting.endIndex)] == "!")
assert(greeting[greeting.index(after: greeting.startIndex)] == "u")

let index = greeting.index(greeting.startIndex, offsetBy: 7)
assert(greeting[index] == "a")



// TODO

// Attempting to access an index outside of a string’s range or a Character at an index outside of a string’s range will trigger a runtime error.
//
// greeting[greeting.endIndex] // error
// greeting.index(after: endIndex) // error

// Use the indices property of the characters property to access all of the indices of individual characters in a string.

for index in greeting.characters.indices {
  print("\(greeting[index]) ", terminator: "")
}
// Prints "G u t e n   T a g ! "

// NOTE
//
// You can use the startIndex and endIndex properties and the index(before:),
// index(after:), and index(_:offsetBy:) methods on any type that conforms to
// the Collection protocol. This includes String, as shown here, as well as
// collection types such as Array, Dictionary, and Set.




// -----------------------------------------------------------------------------

// Inserting and Removing
//
// To insert a single character into a string at a specified index, use the
// insert(_:at:) method, and to insert the contents of another string at a
// specified index, use the insert(contentsOf:at:) method.

var welcome2 = "hello"
welcome2.insert("!", at: welcome2.endIndex)
// welcome2 now equals "hello!"

welcome2.insert(contentsOf:" there".characters, at: welcome2.index(before: welcome2.endIndex))
// welcome2 now equals "hello there!"

// To remove a single character from a string at a specified index, use the
// remove(at:) method, and to remove a substring at a specified range, use
// the removeSubrange(_:) method:

welcome2.remove(at: welcome2.index(before: welcome2.endIndex))
// welcome2 now equals "hello there"

let range = welcome2.index(welcome2.endIndex, offsetBy: -6)..<welcome2.endIndex
welcome2.removeSubrange(range)
// welcome2 now equals "hello"

// NOTE
//
// You can use the the insert(_:at:), insert(contentsOf:at:), remove(at:),
// and removeSubrange(_:) methods on any type that conforms to the
// RangeReplaceableCollection protocol. This includes String, as
// shown here, as well as collection types such as Array,
// Dictionary, and Set.





// -----------------------------------------------------------------------------
//  Comparing Strings
// -----------------------------------------------------------------------------

// Swift provides three ways to compare textual values: string and character
// equality, prefix equality, and suffix equality.

// -----------------------------------------------------------------------------

// String and Character Equality
//
// String and character equality is checked with the “equal to” operator (==) and
// the “not equal to” operator (!=), as described in Comparison Operators:

let quotation = "We're a lot alike, you and I."
let sameQuotation = "We're a lot alike, you and I."
if quotation == sameQuotation {
  print("These two strings are considered equal")
}
// Prints "These two strings are considered equal"

// Two String values (or two Character values) are considered equal if their
// extended grapheme clusters are canonically equivalent.  Extended grapheme
// clusters are canonically equivalent if they have the same linguistic
// meaning and appearance, even if they are composed from different
// Unicode scalars behind the scenes.
//
// For example, LATIN SMALL LETTER E WITH ACUTE (U+00E9) is canonically equivalent
// to LATIN SMALL LETTER E (U+0065) followed by COMBINING ACUTE ACCENT (U+0301).
// Both of these extended grapheme clusters are valid ways to represent the
// character é, and so they are considered to be canonically equivalent:

// "Voulez-vous un café?" using LATIN SMALL LETTER E WITH ACUTE
let eAcuteQuestion = "Voulez-vous un caf\u{E9}?"

// "Voulez-vous un café?" using LATIN SMALL LETTER E and COMBINING ACUTE ACCENT
let combinedEAcuteQuestion = "Voulez-vous un caf\u{65}\u{301}?"

if eAcuteQuestion == combinedEAcuteQuestion {
  print("These two strings are considered equal")
}
// Prints "These two strings are considered equal"

// Conversely, LATIN CAPITAL LETTER A (U+0041, or "A"), as used in English, is not
// equivalent to CYRILLIC CAPITAL LETTER A (U+0410, or "А"), as used in Russian.
// The characters are visually similar, but do not have the same linguistic
// meaning:





let latinCapitalLetterA: Character = "\u{41}"

let cyrillicCapitalLetterA: Character = "\u{0410}"

if latinCapitalLetterA != cyrillicCapitalLetterA {
  print("These two characters are not equivalent.")
}
// Prints "These two characters are not equivalent."




// NOTE
//
// String and character comparisons in Swift are not locale-sensitive.

// -----------------------------------------------------------------------------

// Prefix and Suffix Equality
//
// To check whether a string has a particular string prefix or suffix, call the
// string’s hasPrefix(_:) and hasSuffix(_:) methods, both of which take a
// single argument of type String and return a Boolean value.
//
// The examples below consider an array of strings representing the scene
// locations from the first two acts of Shakespeare’s Romeo and Juliet:

let romeoAndJuliet = [
  "Act 1 Scene 1: Verona, A public place",
  "Act 1 Scene 2: Capulet's mansion",
  "Act 1 Scene 3: A room in Capulet's mansion",
  "Act 1 Scene 4: A street outside Capulet's mansion",
  "Act 1 Scene 5: The Great Hall in Capulet's mansion",
  "Act 2 Scene 1: Outside Capulet's mansion",
  "Act 2 Scene 2: Capulet's orchard",
  "Act 2 Scene 3: Outside Friar Lawrence's cell",
  "Act 2 Scene 4: A street in Verona",
  "Act 2 Scene 5: Capulet's mansion",
  "Act 2 Scene 6: Friar Lawrence's cell"
]

// let romeoAndJuliet = [
//   "Act 1 Scene 1: Verona, A public place",
//   "Act 1 Scene 2: Capulet's mansion",
//   "Act 1 Scene 3: A room in Capulet's mansion",
// ]

// You can use the hasPrefix(_:) method with the romeoAndJuliet array to count
// the number of scenes in Act 1 of the play:

var act1SceneCount = 0
for scene in romeoAndJuliet {
  if scene.hasPrefix("Act 1 ") {
    act1SceneCount += 1
  }
}
print("There are \(act1SceneCount) scenes in Act 1")
// Prints "There are 5 scenes in Act 1"

// Similarly, use the hasSuffix(_:) method to count the number of scenes that
// take place in or around Capulet’s mansion and Friar Lawrence's cell:





var mansionCount = 0
var cellCount = 0
for scene in romeoAndJuliet {
  if scene.hasSuffix("Capulet's mansion") {
    mansionCount += 1
  } else if scene.hasSuffix("Friar Lawrence's cell") {
    cellCount += 1
  }
}
print("\(mansionCount) mansion scenes; \(cellCount) cell scenes")
// Prints "6 mansion scenes; 2 cell scenes"

// NOTE
//
// The hasPrefix(_:) and hasSuffix(_:) methods perform a character-by-character
// canonical equivalence comparison between the extended grapheme clusters in
// each string, as described in String and Character Equality.



// -----------------------------------------------------------------------------
//  Unicode Representations of Strings
// -----------------------------------------------------------------------------

// When a Unicode string is written to a text file or some other storage, the
// Unicode scalars in that string are encoded in one of several Unicode
// -defined encoding forms.  Each form encodes the string in small
// chunks known as code units. These include the UTF-8 encoding
// form (which encodes a string as 8-bit code units), the
// UTF-16 encoding form (which encodes a string as 16-bit
// code units), and the UTF-32 encoding form (which
// encodes a string as 32-bit code units).
//
// Swift provides several different ways to access Unicode representations of
// strings.  You can iterate over the string with a for-in statement, to
// access its individual Character values as Unicode extended grapheme
// clusters.  This process is described in Working with Characters.
//
// Alternatively, access a String value in one of three other
// Unicode-compliant representations:
//
// A collection of UTF-8 code units (accessed with the string’s utf8 property)
// A collection of UTF-16 code units (accessed with the string’s utf16 property)
// A collection of 21-bit Unicode scalar values, equivalent to the string’s UTF-32 encoding form (accessed with the string’s unicodeScalars property)
//
// Each example below shows a different representation of the following string, which is made up of the characters D, o, g, ‼ (DOUBLE EXCLAMATION MARK, or Unicode scalar U+203C), and the 🐶 character (DOG FACE, or Unicode scalar U+1F436):

let dogString = "Dog‼🐶"




// -----------------------------------------------------------------------------

// UTF-8 Representation
//
// You can access a UTF-8 representation of a String by iterating over its
// utf8 property. This property is of type String.UTF8View, which is a collection of unsigned 8-bit (UInt8) values, one for each byte in the string’s UTF-8 representation:

// image: ../Art/UTF8_2x.png

for codeUnit in dogString.utf8 {
  print("\(codeUnit) ", terminator: "")
}
print("")
// 68 111 103 226 128 188 240 159 144 182

// In the example above, the first three decimal codeUnit values (68, 111, 103)
// represent the characters D, o, and g, whose UTF-8 representation is the same
// as their ASCII representation. The next three decimal codeUnit values (226,
// 128, 188) are a three-byte UTF-8 representation of the DOUBLE EXCLAMATION
// MARK character. The last four codeUnit values (240, 159, 144, 182) are a
// four-byte UTF-8 representation of the DOG FACE character.


// -----------------------------------------------------------------------------

// UTF-16 Representation
//
// You can access a UTF-16 representation of a String by iterating over its utf16
// property.  This property is of type String.UTF16View, which is a collection
// of unsigned 16-bit (UInt16) values, one for each 16-bit code unit in the
// string's UTF-16 representation:

// image: ../Art/UTF16_2x.png

for codeUnit in dogString.utf16 {
  print("\(codeUnit) ", terminator: "")
}
print("")
// Prints "68 111 103 8252 55357 56374 "

// Again, the first three codeUnit values (68, 111, 103) represent the characters
// D, o, and g, whose UTF-16 code units have the same values as in the string's
// UTF-8 representation (because these Unicode scalars represent ASCII characters).
//
// The fourth codeUnit value (8252) is a decimal equivalent of the hexadecimal
// value 203C, which represents the Unicode scalar U+203C for the DOUBLE
// EXCLAMATION MARK character. This character can be represented as a
// single code unit in UTF-16.
//
// The fifth and sixth codeUnit values (55357 and 56374) are a UTF-16 surrogate
// pair representation of the DOG FACE character.  These values are a high
// -surrogate value of U+D83D (decimal value 55357) and a low-surrogate
// value of U+DC36 (decimal value 56374).





// -----------------------------------------------------------------------------

// Unicode Scalar Representation
//
// You can access a Unicode scalar representation of a String value by iterating
// over its unicodeScalars property. This property is of type UnicodeScalarView,
// which is a collection of values of type UnicodeScalar.
//
// Each UnicodeScalar has a value property that returns the scalar’s 21-bit value,
// represented within a UInt32 value:
//
// image: ../Art/UnicodeScalar_2x.png

for scalar in dogString.unicodeScalars {
  print("\(scalar.value) ", terminator: "")
}
print("")
// Prints "68 111 103 8252 128054 "

// The value properties for the first three UnicodeScalar values (68, 111, 103)
// once again represent the characters D, o, and g.
//
// The fourth codeUnit value (8252) is again a decimal equivalent of the
// hexadecimal value 203C, which represents the Unicode scalar U+203C
// for the DOUBLE EXCLAMATION MARK character.
//
// The value property of the fifth and final UnicodeScalar, 128054, is a decimal
// equivalent of the hexadecimal value 1F436, which represents the Unicode
// scalar U+1F436 for the DOG FACE character.
//
// As an alternative to querying their value properties, each UnicodeScalar value
// can also be used to construct a new String value, such as with string
// interpolation:

for scalar in dogString.unicodeScalars {
  print("\(scalar) ")
}
// D
// o
// g
// ‼
// 🐶







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








// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - Strings and Characters
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html
// =============================================================================
