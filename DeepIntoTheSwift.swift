// WORK IN PROGRESS






// Here is a single-line comment.  Comments are ignored by the compiler.

// You can put several single-line comments
// together to simulate a block comment.

/* Here is a block comment.
Block comments are usually used
to write multi-line comments. */

/* Here is a block comment that is used like a single-line comment. */

/**
 * Block comments that span multiple lines
 * are usually formatted a bit better.
 */

/**
 * Also, block comments...
 *
 * /* ...may be nested. */
 *
 */

// This is a string which is basically just text.
"Hello!"

// We can display this string by using the `print` function
// from the Swift Standard Library.
print("Hello world!")

print("I")            // We can put single-line comments at the end of the line.

print("think")        /* Same goes for block comments. */

/* Unlike single-line comments, */print("that")
print/* block comments can also */("learning")
print(/* be placed in weird places */"a")
print("language"/* but please don't do that! */)

// Semicolons are optional when only a single statement is on the line.
print("in")
print("a");

// Semicolons are required when multiple statements are on the same line.
print("very"); print("direct")









// -----------------------------------------------------------------------------


// Primitive types.

true                     // A boolean literal.
1                        // An integer literal.
2.2                      // A double floating-point literal.
"way"                    // A string literal.
[2, 3, 4]                // An array of integers.
(1.3, "is", false)       // A tuple of double, string, and boolean.

print(true, 1, 2.2, "way", [2, 3, 4], (1.3, "is", false))

print("1_000_000      =>", 1_000_000)
print("2__3           =>", 2__3)
print("0b101010101    =>", 0b101010101)
print("0b1_010_10_101 =>", 0b1_010_10_101)
print("0o3453         =>", 0o3453)
print("0o34__53       =>", 0o34__53)
print("0xFEEDFACE     =>", 0xFEEDFACE)
print("0xFEED_FACE___ =>", 0xFEED_FACE___)

print("-11...12 =>", -11...12)        // An closed range.
print("-11..<12 =>", -11..<12)        // A semi-open range.

print("Double.infinity  =>", Double.infinity)
print("Float.infinity   =>", Float.infinity)
print("Float32.infinity =>", Float32.infinity)
print("Float64.infinity =>", Float64.infinity)

print("Int.min   =>", Int.min)
print("Int.max   =>", Int.max)
print("Int8.max  =>", Int8.max)
print("Int16.max =>", Int16.max)
print("Int32.max =>", Int32.max)
print("Int64.max =>", Int64.max)

print("CDouble.infinity =>", CDouble.infinity)
print("CInt.max =>", CInt.max)




10
10.0
10.00
10_.0








// http://stackoverflow.com/a/34983398/1935675
print(UnicodeScalar("A").value)



// ----

// https://www.drivenbycode.com/the-missing-apply-function-in-swift/
func repea(str: String, _ n: Int) -> String {
  return [String](count: n, repeatedValue: str).joinWithSeparator("")
}

print(repea("ha", 3)) // -> "hahaha"

// Now with a tuple
let params = ("ha", 3)
print(repea(params)) // -> "hahaha"

// ----




func aaa(a: String..., b: String) {
}


// Collection types.

// Compound types.

// Optional types.



// A constant.  Constants are immutable meaning they can't change once assigned.
// The type of the constant is written after a colon after the constant name.
// This is known as a type annotation and it is also used elsewhere.
let answer: Int = 42

// We can take advantage of the initialization value to infer the type of the constant.
let myAnswer = 42

// The type annotation is required is there's no initial assignment.
let lateAnswer: Int

// A variable.  Variables are mutable which means they can change.
var hi: String = "hi"

// Again, we can take advantage of type inference.
var hello = "hello"

// Again, we need a type annotation here.
var howdy: String



// let a = 0, b = 1, c = 2

// var x = 3, y = 4, z = 5




print("")

var condition = true

if condition {
  print("condition was true")
}

if condition {
  print("condition was true")
} else {
  print("condition was false")
}

if condition {
  print("condition was true")
} else if condition {
  print("condition was true")
} else {
  print("condition was false")
}

if condition {
  print("condition was true")
} else if condition {
  print("condition was true")
} else if condition {
  print("condition was true")
} else {
  print("condition was false")
}



var maybeThing: Int? = 3

if let thing = maybeThing {
  print(thing)
}

if maybeThing != nil {
  print(maybeThing!)
}

let tuple = (1.0, "hello")
print(tuple.0)
print(tuple.1)

// if case {
// }

var s = 0
switch s {
case 0:
  print(0)
default:
  print("non-zero")
}


print("")


for i in 0..<3 {
  print(i)
}

let range = 0..<3
for _ in range {
  print("a")
}


var i = 0
while i < 10 {
  i += 1
}


var j = 0
repeat {
  j += 1
} while j < 10





//operators
//Array
//multi-dimensional array
//Dictionary
//tuple
//optionals
//functions
//parameter names
//default arguments
//closures
//generics
//subscripts
//error handling
//testing




enum A {}

enum A1 {

  case Clubs, Diamonds, Hearts, Spades

}





struct B {}




class C {}

// class initializers
//class C0 {
//}




class C1 {

  var a: Int
  var b = 0

  // Computed properties must have a type annotation.
  var c: Int {
    return self.b
  }

  // Computed properties that read and write have an extended syntax.
  var d: Int {
    get {
      return self.d
    }
    set {
      // A setter cannot set its own computed property.
      self.b += newValue
    }
  }

  init() {
    a = 0
    self.a = 1
  }

}

class C2 {

  static var a1 = 0
  static var a2: Int = 1

  init() {
    C2.a1 = 2
    self.dynamicType.a2 = 3
  }

  static func staticMethod() {
    print("C2.a1: \(a1)")
    print("C2.a2: \(a2)")
  }

}

class C3 {

  // Lazy stored properties must have an initializer.
  lazy var e = 0

}











// Type aliases allow us to use different names for existing types.
// They are a tool to make our code more readable.
typealias Seconds = Double
typealias MyClassAlias = C1

// Type aliases are particularly useful for simplifying
// more complex types like the ones for closures.
typealias MyClosureDefinition = (Int, String, (Double, Double)) -> [String]






let tag: String? = nil
let tagResult = tag ?? "<none>"

// Alternate:
//
// let tagResult: String
// if let tag = tag {
//   tagResult = tag
// } else {
//   tagResult = "<none>"
// }







42.dynamicType
answer.dynamicType







2.2.dynamicType



_ = print.self


_ = Int()


2.advancedBy(3)




_ = 0
_ = ""





print(Int.self)


let a = 0...3
a.count
a.self
a.dynamicType
print(a)

for _ in 0...3 {
  print("In a loop 4 times.")
}

for _ in a {
  print("This will also loop 4 times.")
}







//assert()
//assertionFailure()
//precondition()
//preconditionFailure()
//fatalError()

//abort()
//exit(1)

enum MyError: ErrorType {
  case Alpha
  case Bravo
  case Tango
}


//do {
////  let theResult = try obj.doDangerousStuff()
//}
//catch MyError.Alpha {
//  // Deal with badness.
//}
//catch MyError.Bravo {
//  // Deal with terribleness.
//}
//catch is ErrorType {
//  // Unexpected error!
//}


defer {}




print("test")
debugPrint("test")



// import UIKit






//
//
//class AA {
//  var bb = self
//}
//
//
//





//autoreleasepool {}




//@testable
//import MyApp







// Further reading:
// http://blog.krzyzanowskim.com/2015/03/09/swift-asserts-the-missing-manual/










// // http://stackoverflow.com/a/24102243/1935675
// import Darwin
// exit(0)
