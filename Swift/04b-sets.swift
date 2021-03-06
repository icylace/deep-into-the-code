// -----------------------------------------------------------------------------
//  Set - A collection type having unordered distinct values of similar type.
// -----------------------------------------------------------------------------

// A set stores distinct values of the same type in a collection with no defined
// ordering.  You can use a set instead of an array when the order of items is
// not important, or when you need to ensure that an item only appears once.

// Swift’s Set type is bridged to Foundation's NSSet class.

// -----------------------------------------------------------------------------
//  Hash Values for Set Types
// -----------------------------------------------------------------------------

// A type must be hashable in order to be stored in a set—that is, the type must
// provide a way to compute a hash value for itself.  A hash value is an Int
// value that is the same for all objects that compare equally, such that
// if a == b, it follows that a.hashValue == b.hashValue.

// All of Swift’s basic types (such as String, Int, Double, and Bool) are
// hashable by default, and can be used as set value types or dictionary
// key types.

// Enumeration case values without associated values are also hashable
// by default.


// NOTE
//
// You can use your own custom types as set value types or dictionary key types by
// making them conform to the Hashable protocol from Swift's standard library.
//
// See:
// https://developer.apple.com/reference/swift/hashable

// Types that conform to the Hashable protocol must provide a gettable Int property
// called hashValue.  The value returned by a type's hashValue property is not
// required to be the same across different executions of the same program,
// or in different programs.
//
// Because the Hashable protocol conforms to Equatable, conforming types must also
// provide an implementation of the equals operator (==).  The Equatable protocol
// requires any conforming implementation of == to be an equivalence relation.
// That is, an implementation of == must satisfy the following three conditions,
// for all values a, b, and c:
//
// a == a (Reflexivity)
//
// a == b implies b == a (Symmetry)
//
// a == b && b == c implies a == c (Transitivity)

// For more information about conforming to protocols, see Protocols.

// -----------------------------------------------------------------------------

// Set Type Syntax
//
// The type of a Swift set is written as Set<Element>, where Element is the type
// that the set is allowed to store.  Unlike arrays and dictionaries, sets do
// not have an equivalent shorthand form.



// -----------------------------------------------------------------------------

// Creating and Initializing an Empty Set
//
// You can create an empty set of a certain type using initializer syntax:

var letters = Set<Character>()
assert(type(of: letters) == Set<Character>.self)
assert(letters.count == 0)

// Alternatively, if the context already provides type information, such as a function
// argument or an already typed variable or constant, you can create an empty set with
// an empty array literal:

letters.insert("a")
// letters now contains 1 value of type Character
letters = []
// letters is now an empty set, but is still of type Set<Character>

// -----------------------------------------------------------------------------

// Creating a Set with an Array Literal
//
// You can also initialize a set with an array literal, as a shorthand way to write
// one or more values as a set collection.

// This creates a set called `favoriteGenres` to store String values and initializes it
var favoriteGenres: Set<String> = ["Rock", "Classical", "Hip hop"]
// favoriteGenres has been initialized with three initial items

// A set type cannot be inferred from an array literal alone, so the type `Set`
// must be explicitly declared.  However, because of type inference you don't
// have to write the type of the set if you're initializing it with an array
// literal containing values of the same type.

var favoriteGenres2: Set = ["Rock", "Classical", "Hip hop"]





// -----------------------------------------------------------------------------

// Accessing and Modifying a Set
//
// You access and modify a set through its methods and properties.
//
// To find out the number of items in a set, check its read-only count property:

assert(favoriteGenres.count == 3)

// The Boolean `isEmpty` property is a shortcut for checking whether the `count`
// property is equal to 0.

assert(favoriteGenres.isEmpty == false)

// You can add a new item into a set by calling the set’s insert(_:) method:

favoriteGenres.insert("Jazz")
// favoriteGenres now contains 4 items

// You can remove an item from a set by calling the set’s remove(_:) method, which
// removes the item if it’s a member of the set, and returns the removed value,
// or returns nil if the set did not contain it. Alternatively, all items in a
// set can be removed with its removeAll() method.

if let removedGenre = favoriteGenres.remove("Rock") {
  assert(removedGenre == "Rock")
} else {
  print("I never much cared for that.")
}

// To check whether a set contains a particular item, use the contains(_:) method.

if favoriteGenres.contains("Funk") {
  print("I get up on the good foot.")
} else {
  print("It's too funky in here.")
}
// Prints "It's too funky in here."


// -----------------------------------------------------------------------------

// Iterating Over a Set

// You can iterate over the values in a set with a for-in loop.

for genre in favoriteGenres {
  print("\(genre)")
}
// Jazz
// Hip hop
// Classical


// -----------------------------------------------------------------------------

// For more about the for-in loop, see For-In Loops.

// Swift’s Set type does not have a defined ordering. To iterate over the values
// of a set in a specific order, use the sorted() method, which returns the set’s
// elements as an array sorted using the < operator.

for genre in favoriteGenres.sorted() {
  print("\(genre)")
}
// Classical
// Hip hop
// Jazz



// -----------------------------------------------------------------------------
//  Performing Set Operations
// -----------------------------------------------------------------------------

// Performing Set Operations
//
// You can efficiently perform fundamental set operations, such as combining
// two sets together, determining which values two sets have in common, or
// determining whether two sets contain all, some, or none of the same
// values.

// -----------------------------------------------------------------------------

// Fundamental Set Operations
//
// The illustration below depicts two sets—a and b—with the results of various set operations represented by the shaded regions.
//
// image: ../Art/setVennDiagram_2x.png
//
// Use the intersection(_:) method to create a new set with only the values common to both sets.
// Use the symmetricDifference(_:) method to create a new set with values in either set, but not both.
// Use the union(_:) method to create a new set with all of the values in both sets.
// Use the subtracting(_:) method to create a new set with values not in the specified set.

let oddDigits: Set = [1, 3, 5, 7, 9]
let evenDigits: Set = [0, 2, 4, 6, 8]
let singleDigitPrimeNumbers: Set = [2, 3, 5, 7]

assert(oddDigits.union(evenDigits).sorted() == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
assert(oddDigits.intersection(evenDigits).sorted() == [])
assert(oddDigits.subtracting(singleDigitPrimeNumbers).sorted() == [1, 9])
assert(oddDigits.symmetricDifference(singleDigitPrimeNumbers).sorted() == [1, 2, 9])



// -----------------------------------------------------------------------------

// Set Membership and Equality
//
// The illustration below depicts three sets—a, b and c—with overlapping regions
// representing elements shared among sets. Set a is a superset of set b, because
// a contains all elements in b. Conversely, set b is a subset of set a, because
// all elements in b are also contained by a. Set b and set c are disjoint with
// one another, because they share no elements in common.



let houseAnimals: Set = ["🐶", "🐱"]
let apartmentAnimals: Set = ["🐱", "🐶"]
let farmAnimals: Set = ["🐮", "🐔", "🐑", "🐶", "🐱"]
let cityAnimals: Set = ["🐦", "🐭"]

// The "is equal" operator (`==`) checks if two sets have all the same values.
assert(houseAnimals == apartmentAnimals)

// The `isSubset(of:)` method checks if the values of a set are contained in a
// given set.
assert(houseAnimals.isSubset(of: farmAnimals) == true)

// Use the isStrictSubset(of:) or isStrictSuperset(of:) methods to determine
// whether a set is a subset or superset, but not equal to, a specified set.

// The `isSuperset(of:)` method checks if a set contains all the values in a
// given set.
assert(farmAnimals.isSuperset(of: houseAnimals) == true)

// The `isDisjoint(with:)` method checks whether two sets have common values.
assert(farmAnimals.isDisjoint(with: cityAnimals) == true)



// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - Collection Types
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/CollectionTypes.html
// =============================================================================
