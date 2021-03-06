// -----------------------------------------------------------------------------
//  Dictionary - A collection type having unordered key-value associations.
// -----------------------------------------------------------------------------

// TODO


// A dictionary stores associations between keys of the same type and values of the
// same type in a collection with no defined ordering.  Each value is associated with
// a unique key that acts as an identifier for that value within the dictionary.
//
// NOTE
//
// Swift’s Dictionary type is bridged to Foundation’s NSDictionary class.

// -----------------------------------------------------------------------------

// Dictionary Type Shorthand Syntax
//
// The type of a Swift dictionary is written in full as Dictionary<Key, Value>,
// where Key is the type of value that can be used as a dictionary key, and
// Value is the type of value that the dictionary stores for those keys.
//
// NOTE
//
// A dictionary Key type must conform to the Hashable protocol, like a set’s value type.
//
// You can also write the type of a dictionary in shorthand form as [Key: Value].
// Although the two forms are functionally identical, the shorthand form is
// preferred and is used throughout this guide when referring to the type
// of a dictionary.

// -----------------------------------------------------------------------------

// An empty dictionary can be created with initializer syntax.

// This dictionary's keys are of type `Int` and its values are of type `String`.
var namesOfIntegers = [Int: String]()
assert(type(of: namesOfIntegers) == [Int: String].self)

// If the context already provides type information, you can create an empty
// dictionary with an empty dictionary literal, which is written as [:]
// (a colon inside a pair of square brackets):

namesOfIntegers[16] = "sixteen"
// namesOfIntegers now contains 1 key-value pair
namesOfIntegers = [:]
// namesOfIntegers is once again an empty dictionary of type [Int: String]




// -----------------------------------------------------------------------------

// Creating a Dictionary with a Dictionary Literal
//
// You can also initialize a dictionary with a dictionary literal, which has a
// similar syntax to the array literal seen earlier. A dictionary literal is a
// shorthand way to write one or more key-value pairs as a Dictionary collection.
//
// A key-value pair is a combination of a key and a value.
//
// In a dictionary literal,
// the key and value in each key-value pair are separated by a colon. The key-value
// pairs are written as a list, separated by commas, surrounded by a pair of square
// brackets:

// _ = [key 1: value 1, key 2: value 2, key 3: value 3]

// The example below creates a dictionary to store the names of international airports.
// In this dictionary, the keys are three-letter International Air Transport Association
// codes, and the values are airport names:

var airports: [String: String] = ["YYZ": "Toronto Pearson", "DUB": "Dublin"]

// The airports dictionary is declared as having a type of [String: String],
// which means “a Dictionary whose keys are of type String, and whose values
// are also of type String”.
//
// NOTE
//
// The airports dictionary is declared as a variable (with the var introducer), and
// not a constant (with the let introducer), because more airports are added to the
// dictionary in the examples below.

// The airports dictionary is initialized with a dictionary literal containing two
// key-value pairs. The first pair has a key of "YYZ" and a value of "Toronto Pearson".
// The second pair has a key of "DUB" and a value of "Dublin".

// This dictionary literal contains two String: String pairs. This key-value type
// matches the type of the airports variable declaration (a dictionary with only
//   String keys, and only String values), and so the assignment of the dictionary
//   literal is permitted as a way to initialize the airports dictionary with two
//   initial items.

// As with arrays, you don’t have to write the type of the dictionary if you’re
// initializing it with a dictionary literal whose keys and values have consistent
// types. The initialization of airports could have been written in a shorter form
// instead:

var airports2 = ["YYZ": "Toronto Pearson", "DUB": "Dublin"]

// Because all keys in the literal are of the same type as each other, and likewise
// all values are of the same type as each other, Swift can infer that [String: String]
// is the correct type to use for the airports dictionary.

// -----------------------------------------------------------------------------

// Accessing and Modifying a Dictionary
//
// You access and modify a dictionary through its methods and properties, or by
// using subscript syntax.
//
// As with an array, you find out the number of items in a Dictionary by
// checking its read-only count property:

print("The airports dictionary contains \(airports.count) items.")
// Prints "The airports dictionary contains 2 items."

// Use the Boolean isEmpty property as a shortcut for checking whether the count
// property is equal to 0:

if airports.isEmpty {
  print("The airports dictionary is empty.")
} else {
  print("The airports dictionary is not empty.")
}
// Prints "The airports dictionary is not empty."

// You can add a new item to a dictionary with subscript syntax. Use a new key of the appropriate type as the subscript index, and assign a new value of the appropriate type:

airports["LHR"] = "London"
// the airports dictionary now contains 3 items

// You can also use subscript syntax to change the value associated with a particular key:

airports["LHR"] = "London Heathrow"
// the value for "LHR" has been changed to "London Heathrow"

// As an alternative to subscripting, use a dictionary’s updateValue(_:forKey:) method to set or update the value for a particular key. Like the subscript examples above, the updateValue(_:forKey:) method sets a value for a key if none exists, or updates the value if that key already exists. Unlike a subscript, however, the updateValue(_:forKey:) method returns the old value after performing an update. This enables you to check whether or not an update took place.
//
// The updateValue(_:forKey:) method returns an optional value of the dictionary’s value type. For a dictionary that stores String values, for example, the method returns a value of type String?, or “optional String”. This optional value contains the old value for that key if one existed before the update, or nil if no value existed:

if let oldValue = airports.updateValue("Dublin Airport", forKey: "DUB") {
  print("The old value for DUB was \(oldValue).")
}
// Prints "The old value for DUB was Dublin."

// You can also use subscript syntax to retrieve a value from the dictionary for a particular key. Because it is possible to request a key for which no value exists, a dictionary’s subscript returns an optional value of the dictionary’s value type. If the dictionary contains a value for the requested key, the subscript returns an optional value containing the existing value for that key. Otherwise, the subscript returns nil:

if let airportName = airports["DUB"] {
  print("The name of the airport is \(airportName).")
} else {
  print("That airport is not in the airports dictionary.")
}
// Prints "The name of the airport is Dublin Airport."

// You can use subscript syntax to remove a key-value pair from a dictionary by assigning a value of nil for that key:

airports["APL"] = "Apple International"
// "Apple International" is not the real airport for APL, so delete it
airports["APL"] = nil
// APL has now been removed from the dictionary

// Alternatively, remove a key-value pair from a dictionary with the removeValue(forKey:) method. This method removes the key-value pair if it exists and returns the removed value, or returns nil if no value existed:

if let removedValue = airports.removeValue(forKey: "DUB") {
  print("The removed airport's name is \(removedValue).")
} else {
  print("The airports dictionary does not contain a value for DUB.")
}
// Prints "The removed airport's name is Dublin Airport."

// -----------------------------------------------------------------------------

// Iterating Over a Dictionary

// You can iterate over the key-value pairs in a dictionary with a for-in loop.
// Each item in the dictionary is returned as a (key, value) tuple, and you can
// decompose the tuple's members into temporary constants or variables as part
// of the iteration:

let output = [String]()
for (airportCode, airportName) in airports {
  output.append("\(airportCode): \(airportName)")
}
assert(output == [
  "YYZ: Toronto Pearson",
  "LHR: London Heathrow",
])

// For more about the for-in loop, see For-In Loops.

// You can also retrieve an iterable collection of a dictionary’s keys or values by accessing its keys and values properties:

for airportCode in airports.keys {
  print("Airport code: \(airportCode)")
}
// Airport code: YYZ
// Airport code: LHR

for airportName in airports.values {
  print("Airport name: \(airportName)")
}
// Airport name: Toronto Pearson
// Airport name: London Heathrow


// If you need to use a dictionary’s keys or values with an API that takes an Array instance, initialize a new array with the keys or values property:

let airportCodes = [String](airports.keys)
// airportCodes is ["YYZ", "LHR"]

let airportNames = [String](airports.values)
// airportNames is ["Toronto Pearson", "London Heathrow"]

// Swift’s Dictionary type does not have a defined ordering. To iterate over the keys or values of a dictionary in a specific order, use the sorted() method on its keys or values property.



// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - Collection Types
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/CollectionTypes.html
// =============================================================================
