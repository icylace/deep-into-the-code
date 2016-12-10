// -----------------------------------------------------------------------------
//  Subscript - A shortcut for instance access with array-like bracket notation.
// -----------------------------------------------------------------------------

// A subscript is typically used as a shortcut for accessing member elements
// of collection-type data like arrays.  Though, it can be used in whatever
// way suits its context.

// A subscript...
// ...can take any number of input parameters of any type combination.
// ...can return any type.
// (?)...can use variable parameters.
// ...can use variadic parameters.
// ...cannot use in-out parameters.
// ...cannot provide default parameter values.

// TODO


class Foo {
  var xs = [0, 0, 0]
  subscript(index: Int) -> Int {
    get {
      return xs[index]
    }
    set(newValue) {
      xs[index] = newValue
    }
  }
}

let foo = Foo()
assert(foo[1] == 0)
foo.xs[1] = 2
assert(foo[1] == 2)
foo[1] = 5
assert(foo[1] == 5)

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

struct Baz {
  var x = 1
  subscript(index: Int) -> Int {
    get {
      return x
    }
  }
}

let baz = Baz()
assert(baz[8] == 1)

// -----------------------------------------------------------------------------




/*



// TODO

enum Baz {
  case x
  subscript(index: Int) -> Baz {
    return .x
  }
}

*/



// Multiple subscripts can be defined for a single type, and the appropriate
// subscript overload to use is selected based on the type of index value
// passed to the subscript.

class DailyMeal {
  enum MealTime {
    case Breakfast, Lunch, Dinner
  }

  var meals: [MealTime: String] = [:]

  subscript(requestedMeal: MealTime) -> String {
    get {
      if let thisMeal = meals[requestedMeal] {
        return thisMeal
      } else {
        return "Ramen"
      }
    }
    set(newMealName) {
      meals[requestedMeal] = newMealName
    }
  }
}


enum Foobar {
  case values([Int])
  case singleThing(Double)

  subscript(index:Int) -> Int? {
    get {
      switch self {
      case .values(var numbers):
        return numbers[index]
      default:
        return nil
      }
    }
    set {
      switch self {
      case .values(var numbers):
        numbers[index] = newValue!
        self = .values(numbers)
      default:
        break
      }
    }
  }
}





enum Bazz: Int {
  case x
  subscript(index: Bazz) -> Int {
    return index.rawValue
  }
}

let bazz = Bazz.x
let quux = bazz[.x]





// -----------------------------------------------------------------------------
//  Subscript overloading - The choosing of a subscript implementation provided
//                          by a type based on the types of values passed
//                          to the subscript brackets.
// -----------------------------------------------------------------------------




// -----------------------------------------------------------------------------


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
    // TODO
    // - does the following work?
    // return 0..<rows ~= row && 0..<columns ~= column
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



// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - Subscripts
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Subscripts.html
// =============================================================================
