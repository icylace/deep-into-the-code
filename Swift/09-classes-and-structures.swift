// =============================================================================
//  Classes and Structures
//  https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/ClassesAndStructures.html
// =============================================================================


// -----------------------------------------------------------------------------
//  Comparing Classes and Structures
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Structures and Enumerations Are Value Types
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Classes Are Reference Types
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Choosing Between Classes and Structures
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Assignment and Copy Behavior for Strings, Arrays, and Dictionaries
// -----------------------------------------------------------------------------







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








struct B {}
