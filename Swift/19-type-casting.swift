// TODO

// -----------------------------------------------------------------------------
//  Type casting - The act of checking the type of an instance, or to treat that
//                 instance as a different superclass or subclass from somewhere
//                 else in its own class hierarchy.
// -----------------------------------------------------------------------------

// Type casting in Swift is implemented with the `is` and `as` operators.  These
// two operators provide a simple and expressive way to check the type of a value
// or cast a value to a different type.

// You can also use type casting to check whether a type conforms to a protocol.


// -----------------------------------------------------------------------------
//  Defining a Class Hierarchy for Type Casting
// -----------------------------------------------------------------------------

// You can use type casting with a hierarchy of classes and subclasses to check the
// type of a particular class instance and to cast that instance to another class
// within the same hierarchy. The three code snippets below define a hierarchy of
// classes and an array containing instances of those classes, for use in an
// example of type casting.
//
// The first snippet defines a new base class called MediaItem. This class provides
// basic functionality for any kind of item that appears in a digital media library.
// Specifically, it declares a name property of type String, and an init name
// initializer. (It is assumed that all media items, including all movies and
// songs, will have a name.)

class MediaItem {
  var name: String
  init(name: String) {
    self.name = name
  }
}

// The next snippet defines two subclasses of MediaItem.  The first subclass, Movie,
// encapsulates additional information about a movie or film. It adds a director
// property on top of the base MediaItem class, with a corresponding initializer.
// The second subclass, Song, adds an artist property and initializer on top of
// the base class:

class Movie: MediaItem {
  var director: String
  init(name: String, director: String) {
    self.director = director
    super.init(name: name)
  }
}

class Song: MediaItem {
  var artist: String
  init(name: String, artist: String) {
    self.artist = artist
    super.init(name: name)
  }
}

// The final snippet creates a constant array called library, which contains two
// Movie instances and three Song instances. The type of the library array is
// inferred by initializing it with the contents of an array literal. Swift’s
// type checker is able to deduce that Movie and Song have a common superclass
// of MediaItem, and so it infers a type of [MediaItem] for the library array:

let library = [
  Movie(name: "Casablanca", director: "Michael Curtiz"),
  Song(name: "Blue Suede Shoes", artist: "Elvis Presley"),
  Movie(name: "Citizen Kane", director: "Orson Welles"),
  Song(name: "The One And Only", artist: "Chesney Hawkes"),
  Song(name: "Never Gonna Give You Up", artist: "Rick Astley")
]
// the type of "library" is inferred to be [MediaItem]

// The items stored in library are still Movie and Song instances behind the scenes.
// However, if you iterate over the contents of this array, the items you receive
// back are typed as MediaItem, and not as Movie or Song. In order to work with
// them as their native type, you need to check their type, or downcast them to
// a different type, as described below.


// -----------------------------------------------------------------------------
//  Type check operator (`is`) - The operator that checks if an instance is of
//                               a certain subclass type
// -----------------------------------------------------------------------------

var movieCount = 0
var songCount = 0

for item in library {
  if item is Movie {
    movieCount += 1
  } else if item is Song {
    songCount += 1
  }
}

assert(movieCount == 2)
assert(songCount == 3)


// -----------------------------------------------------------------------------
//  Upcasting - The act of treating an instance as an instance of a superclass.
//  Downcasting - The act of treating an instance as an instance of a subclass.
//  Type cast operator (`as`) - An operator attempting to downcast an instance.
//  Conditional type cast operator (`as?`) - The type cast operator that returns
//                                           an optional of the type being
//                                           downcasted to.
//  Forced type cast operator (`as!`) - The type cast operator that returns
//                                      the result of force-unwrapping the
//                                      attempted downcast.
// -----------------------------------------------------------------------------

// A constant/variable of a certain class type may actually refer to an
// instance of a subclass behind the scenes.  Where you believe this is
// the case, you can try to downcast to the subclass type with a type
// cast operator.

// Because downcasting can fail, the type cast operator comes in two different
// forms.


// Use the conditional form of the type cast operator (`as?`) when you are not
// sure if the downcast will succeed.  This form of the operator will always
// return an optional value, and the value will be nil if the downcast was
// not possible. This enables you to check for a successful downcast.


// Use the forced form of the type cast operator (as!) only when you are
// sure that the downcast will always succeed. This form of the operator
// will trigger a runtime error if you try to downcast to an incorrect
// class type.

// In this example, each item in the array might be a Movie, or it might be a
// Song.  You don't know in advance which actual class to use for each item,
// and so it is appropriate to use the conditional form of the type cast
// operator (as?) to check the downcast each time through the loop.

var output1 = [String]()
for item in library {
  if let movie = item as? Movie {
    output1.append("Movie: \(movie.name), dir. \(movie.director)")
  } else if let song = item as? Song {
    output1.append("Song: \(song.name), by \(song.artist)")
  }
}
assert(output1 == [
  "Movie: Casablanca, dir. Michael Curtiz",
  "Song: Blue Suede Shoes, by Elvis Presley",
  "Movie: Citizen Kane, dir. Orson Welles",
  "Song: The One And Only, by Chesney Hawkes",
  "Song: Never Gonna Give You Up, by Rick Astley",
])

// The example starts by trying to downcast the current item as a Movie.
// Because item is a MediaItem instance, it’s possible that it might be
// a Movie; equally, it's also possible that it might be a Song, or
// even just a base MediaItem. Because of this uncertainty, the
// `as?` form of the type cast operator returns an optional
// value when attempting to downcast to a subclass type.
// The result of item as? Movie is of type Movie?, or
// "optional Movie".

// Downcasting to Movie fails when applied to the Song instances in the library
// array.  To cope with this, the example above uses optional binding to check
// whether the optional Movie actually contains a value (that is, to find out
// whether the downcast succeeded.)  This optional binding is written “if let
// movie = item as? Movie”, which can be read as:

// “Try to access item as a Movie. If this is successful, set a new temporary
// constant called movie to the value stored in the returned optional Movie.”


// Casting does not actually modify the instance or change its values.  The
// underlying instance remains the same; it is simply treated and accessed
// as an instance of the type to which it has been cast.



// -----------------------------------------------------------------------------
//  Type Casting for Any and AnyObject
// -----------------------------------------------------------------------------

// Swift provides two special types for working with nonspecific types:
// - Any can represent an instance of any type at all, including function types.
// - AnyObject can represent an instance of any class type.

// TODO
// - put in styleguide with reference:
// Use `Any` and `AnyObject` only when you explicitly need the behavior and
// capabilities they provide.  It is always better to be specific about the
// types you expect to work with in your code.

// Here's an example of using `Any` to work with a mix of different types,
// including function types and non-class types.
var things = [Any]()
things.append(0)
things.append(0.0)
things.append(42)
things.append(3.14159)
things.append("hello")
things.append((3.0, 5.0))
things.append(Movie(name: "Ghostbusters", director: "Ivan Reitman"))
things.append({ (name: String) -> String in "Hello, \(name)" })

// The `is` and `as` operators can be used in a `switch` statement's cases to
// discover the specific type of a constant/variable that is known only to
// be of type `Any` or `AnyObject`.
var output = [String]()
for thing in things {
  switch thing {
  case 0 as Int:
    output.append("zero as an Int")
  case 0 as Double:
    output.append("zero as a Double")
  case let someInt as Int:
    output.append("an integer value of \(someInt)")
  case let someDouble as Double where someDouble > 0:
    output.append("a positive double value of \(someDouble)")
  case is Double:
    output.append("some other double value that I don't want to print")
  case let someString as String:
    output.append("a string value of \"\(someString)\"")
  case let (x, y) as (Double, Double):
    output.append("an (x, y) point at \(x), \(y)")
  case let movie as Movie:
    output.append("a movie called \(movie.name), dir. \(movie.director)")
  case let stringConverter as (String) -> String:
    output.append(stringConverter("Michael"))
  default:
    output.append("something else")
  }
}
assert(output == [
  "zero as an Int",
  "zero as a Double",
  "an integer value of 42",
  "a positive double value of 3.14159",
  "a string value of \"hello\"",
  "an (x, y) point at 3.0, 5.0",
  "a movie called Ghostbusters, dir. Ivan Reitman",
  "Hello, Michael",
])


// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - Type Casting
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TypeCasting.html
// =============================================================================
