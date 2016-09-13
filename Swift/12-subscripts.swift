// =============================================================================
//  Subscripts
//  https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Subscripts.html
// =============================================================================

// Classes, structures, and enumerations can define subscripts, which are
// shortcuts for accessing properties.  Subscripts can be used to set and
// retrieve values by index without needing separate methods for setting
// and retrieval.


// -----------------------------------------------------------------------------
//  Subscript Syntax
// -----------------------------------------------------------------------------

// Subscripts are defined similar to computed properties.
class Foo {
  var x = 0
  subscript(index: Int) -> Int {
    get {
      return x
    }
    set(newValue) {
      x = newValue
    }
  }
}

let foo = Foo()
assert(foo[8] == 0)
foo.x = 2
assert(foo[8] == 2)
foo[8] = 5
assert(foo[8] == 5)

// -----------------------------------------------------------------------------

// Read-only subscripts are defined similar to read-only computed properties.
struct Bar {
  var x = 1
  subscript(index: Int) -> Int {
    return x
  }
}

let bar = Bar()
assert(bar[8] == 1)

// -----------------------------------------------------------------------------

enum Baz {
  case x
  subscript(index: Int) -> Baz {
    return x
  }
}

// You can define multiple subscripts for a single type, and the appropriate
// subscript overload to use is selected based on the type of index value
// you pass to the subscript.

// class DailyMeal {
//     enum MealTime {
//         case Breakfast
//         case Lunch
//         case Dinner
//     }
//
//     var meals: [MealTime : String] = [:]
//
//     subscript(requestedMeal: MealTime) -> String {
//         get {
//             if let thisMeal = meals[requestedMeal] {
//                 return thisMeal
//             } else {
//                 return "Ramen"
//             }
//         }
//
//         set(newMealName) {
//             meals[requestedMeal] = newMealName
//         }
//     }
// }

// enum Foobar {
//   case values([Int])
//   case singleThing(Double)
//
//   subscript(index:Int) -> Int? {
//     get {
//       switch self {
//       case .values (let numbers):
//         return numbers[index]
//       default:
//         return nil
//       }
//     }
//     set {
//       switch self {
//       case .values (let numbers):
//         numbers[index] = newValue!
//         self = .values(numbers)
//       default:
//         break
//       }
//     }
//   }
// }

// enum Baz {
//   case x
//   subscript(index: Baz) -> Int {
//     return 0
//   }
// }
//
// let baz = Baz[.x]
// // assert()

// A class or structure can provide as many subscript implementations as it
// needs, and the appropriate subscript to be used will be inferred based
// on the types of the value or values that are contained within the
// subscript brackets at the point that the subscript is used.
// This is known as subscript overloading.


// -----------------------------------------------------------------------------
//  Subscript Usage
// -----------------------------------------------------------------------------

// The exact meaning of "subscript" depends on the context in which it is used.
// Subscripts are typically used as a shortcut for accessing the member
// elements in a collection, list, or sequence.  You are free to implement
// subscripts in the most appropriate way for your particular class or
// structure's functionality.


// -----------------------------------------------------------------------------
//  Subscript Options
// -----------------------------------------------------------------------------

// Subscripts can take any number of input parameters, and these input
// parameters can be of any type.  Subscripts can also return any type.
// Subscripts can use variable parameters and variadic parameters, but
// cannot use in-out parameters or provide default parameter values.

// Subscripts are not limited to a single dimension, and you can
// define subscripts with multiple input parameters to suit your
// custom type's needs.

struct Matrix {
  let rows: Int, columns: Int
  var grid: [Double]
  init(rows: Int, columns: Int) {
    self.rows = rows
    self.columns = columns
    grid = Array(repeating: 0.0, count: rows * columns)
  }
  func indexIsValidFor(row: Int, column: Int) -> Bool {
    return row >= 0 && row < rows && column >= 0 && column < columns
  }
  subscript(row: Int, column: Int) -> Double {
    get {
      assert(indexIsValidFor(row: row, column: column), "Index out of range")
      return grid[(row * columns) + column]
    }
    set {
      assert(indexIsValidFor(row: row, column: column), "Index out of range")
      grid[(row * columns) + column] = newValue
    }
  }
}

var matrix = Matrix(rows: 2, columns: 2)

matrix[0, 1] = 1.5
matrix[1, 0] = 3.2
