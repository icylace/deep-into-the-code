// =============================================================================
//  Methods
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Methods.html
// =============================================================================

// TODO

// -----------------------------------------------------------------------------
//  Method - A function belonging to a particular type.
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Instance method - A method that works with an instance of the
//                    type it belongs to.
// -----------------------------------------------------------------------------

// An instance method...
// ...has implicit access to all other instance methods and properties of
//    the type it belongs to.
// ...can be called only on a specific instance of the type it belongs to.
// ...cannot be called in isolation without an existing instance.

class Counter {
  var count = 0
  func increment(by amount: Int = 1) {
    count += amount
  }
  func reset() {
    count = 0
  }
  // Instances come with a special `self` property which references
  // the instance itself.
  func restart() {
    self.count = 0
  }
}

let c = Counter()
assert(c.count == 0)
c.increment()
assert(c.count == 1)
c.increment(by: 5)
assert(c.count == 6)
c.reset()
assert(c.count == 0)
c.increment()
c.increment()
assert(c.count == 2)
c.restart()
assert(c.count == 0)

// -----------------------------------------------------------------------------

// Usually you don't need to use `self`, in which case Swift assumes you are
// referring to a property or method of the current instance whenever you
// use a known property or method name within a method.

// The main exception to this rule occurs when a parameter name for an instance
// method has the same name as a property of that instance.  In this situation,
// the parameter name takes precedence, and it becomes necessary to refer to
// the property in a more qualified way.  You use the `self` property to
// distinguish between the parameter name and the property name.

struct Point {
  var x = 0.0, y = 0.0
  func isToTheRightOf(x: Double) -> Bool {
    return self.x > x
  }
}

let somePoint = Point(x: 4.0, y: 5.0)

if somePoint.isToTheRightOf(x: 1.0) {
  print("This point is to the right of the line where x == 1.0")
}
// Prints "This point is to the right of the line where x == 1.0"

// Without the self prefix, Swift would assume that both uses of x referred to
// the method parameter called x.

// -----------------------------------------------------------------------------

// Structures and enumerations are value types.  By default, the properties of
// a value type cannot be modified from within its instance methods.

// However, if you need to modify the properties of your structure or
// enumeration within a particular method, you can opt in to mutating
// behavior for that method.  The method can then mutate (that is,
// change) its properties from within the method, and any changes
// that it makes are written back to the original structure when
// the method ends.  The method can also assign a completely new
// instance to its implicit self property, and this new
// instance will replace the existing one when the
// method ends.

// You can opt in to this behavior by placing the `mutating` keyword before the
// `func` keyword for that method:

struct Point {
  var x = 0.0, y = 0.0
  mutating func moveBy(x deltaX: Double, y deltaY: Double) {
    x += deltaX
    y += deltaY
  }
}

var somePoint = Point(x: 1.0, y: 1.0)
somePoint.moveBy(x: 2.0, y: 3.0)
assert(somePoint.x == 3.0)
assert(somePoint.y == 4.0)

// Note that you cannot call a mutating method on a constant of structure type,
// because its properties cannot be changed, even if they are variable
// properties.

let fixedPoint = Point(x: 3.0, y: 3.0)
// This will report an error:
fixedPoint.moveBy(x: 2.0, y: 3.0)
// fixedPoint.moveBy(x: 2.0, y: 3.0)

// -----------------------------------------------------------------------------

// Mutating methods can assign an entirely new instance to the implicit `self`
// property.  The Point example shown above could have been written in the
// following way instead:

struct Point {
  var x = 0.0, y = 0.0
  mutating func moveBy(x deltaX: Double, y deltaY: Double) {
    self = Point(x: x + deltaX, y: y + deltaY)
  }
}

// This version of the mutating moveBy(x:y:) method creates a brand new
// structure whose x and y values are set to the target location.
// The end result of calling this alternative version of the
// method will be exactly the same as for calling the
// earlier version.

// Mutating methods for enumerations can set the implicit `self` parameter
// to be a different case from the same enumeration:

enum TriStateSwitch {
  case off, low, high
  mutating func next() {
    switch self {
    case .off:
      self = .low
    case .low:
      self = .high
    case .high:
      self = .off
    }
  }
}

var ovenLight = TriStateSwitch.low
ovenLight.next()
assert(ovenLight == .high)
ovenLight.next()
assert(ovenLight == .off)


// -----------------------------------------------------------------------------
//  Type method - A method that works with the type it belongs to.
// -----------------------------------------------------------------------------

// You indicate type methods by writing the `static` keyword
// before the method's `func` keyword.  Classes may also use the `class`
// keyword to allow subclasses to override the superclass's
// implementation of that method.

// Each type method is explicitly scoped to the type it supports.

class SomeClass {
  class func someTypeMethod() {
    // Type method implementation goes here.
  }
}

SomeClass.someTypeMethod()

// Within a type method, the `self` refers to the type itself.  This means you
// can use `self` to disambiguate between type properties and type method
// parameters, just as you do for instance properties and instance
// method parameters.

// Any unqualified method and property names that you use within the body of a
// type method will refer to other type-level methods and properties.  A type
// method can call another type method with the other method's name, without
// needing to prefix it with the type name.  Similarly, type methods on
// structures and enumerations can access type properties by using the
// type property's name without a type name prefix.

struct LevelTracker {
  static var highestUnlockedLevel = 1
  var currentLevel = 1

  static func unlock(_ level: Int) {
    if level > highestUnlockedLevel { highestUnlockedLevel = level }
  }

  static func isUnlocked(_ level: Int) -> Bool {
    return level <= highestUnlockedLevel
  }

  @discardableResult
  mutating func advance(to level: Int) -> Bool {
    if LevelTracker.isUnlocked(level) {
      currentLevel = level
      return true
    } else {
      return false
    }
  }
}

// In addition to its type property and type methods, LevelTracker tracks
// an individual player's progress through the game.  It uses an instance
// property called currentLevel to track the level that a player is
// currently playing.

// To help manage the currentLevel property, LevelTracker defines an instance
// method called advance(to:).  Before updating currentLevel, this method
// checks whether the requested new level is already unlocked.  The
// advance(to:) method returns a Boolean value to indicate whether
// or not it was actually able to set currentLevel.  Because it's
// not necessarily a mistake for code that calls the advance(to:)
// method to ignore the return value, this function is marked
// with the @discardableResult attribute.

class Player {
  var tracker = LevelTracker()
  let playerName: String
  func complete(level: Int) {
    LevelTracker.unlock(level + 1)
    tracker.advance(to: level + 1)
  }
  init(name: String) {
    playerName = name
  }
}

var player = Player(name: "Argyrios")
player.complete(level: 1)
print("highest unlocked level is now \(LevelTracker.highestUnlockedLevel)")
// Prints "highest unlocked level is now 2"

player = Player(name: "Beto")
if player.tracker.advance(to: 6) {
  print("player is now on level 6")
} else {
  print("level 6 has not yet been unlocked")
}
// Prints "level 6 has not yet been unlocked"
