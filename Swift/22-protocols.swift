// =============================================================================
//  Protocols
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Protocols.html
// =============================================================================

// Protocol - A blueprint of methods, properties, and other requirements that
//            suit a particular task or piece of functionality.  The protocol
//            can then be adopted by a class, structure, or enumeration to
//            provide an actual implementation of those requirements.
//            Any type that satisfies the requirements of a protocol
//            is said to conform to that protocol.


// -----------------------------------------------------------------------------
//  Protocol Syntax
// -----------------------------------------------------------------------------

// A protocol named `SomeProtocol`.
protocol SomeProtocol {
  // Protocol definition goes here.
}

// A custom type, in this case a struct, adopting a protocol.
struct SomeStructure: SomeProtocol {
  // Structure definition goes here.
}

// A custom type adopting multiple protocols.
struct SomeStructure: SomeProtocol, SecondProtocol, ThirdProtocol {
  // Structure definition goes here.
}

// If a class also has a superclass, the superclass gets listed before
// any protocols.

class SomeClass: SomeSuperclass, SomeProtocol, SecondProtocol {
  // Class definition goes here.
}


// -----------------------------------------------------------------------------
//  Property Requirements
// -----------------------------------------------------------------------------

// TODO

// A protocol can require any conforming type to provide an instance property
// or type property with a particular name and type.  The protocol doesn't
// specify whether the property should be stored or computed.  The
// protocol also specifies whether each property must be gettable
// or gettable and settable.

// If a protocol requires a property to be gettable and settable, that property
// requirement cannot be fulfilled by a constant stored property or a read-only
// computed property.

// If the protocol only requires a property to be gettable, the requirement can
// be satisfied by any kind of property and it is valid for the property to be
// also settable if this is useful for your own code.

// Property requirements are always declared as variable properties.

protocol SomeProtocol {
  // A gettable and settable property.
  var mustBeSettable: Int { get set }
  // A gettable property.
  var doesNotNeedToBeSettable: Int { get }
}

// In a protocol, type property requirements are always prefixed with `static`.
// This rule pertains even though type property requirements can be prefixed
// with the class or static keyword when implemented by a class.

protocol AnotherProtocol {
  // A gettable and settable type property.
  static var someTypeProperty: Int { get set }
}

// Here's an example of a protocol with a single instance property
// requirement.  The protocol doesn’t specify anything else about
// the nature of the conforming type.
protocol FullyNamed {
  var fullName: String { get }
}

struct Person: FullyNamed {
  var fullName: String
}

let john = Person(fullName: "John Appleseed")
// john.fullName is "John Appleseed"

// Here's a more complex class, which also adopts and conforms to the
// FullyNamed protocol:
class Starship: FullyNamed {
  var prefix: String?
  var name: String
  init(name: String, prefix: String? = nil) {
    self.name = name
    self.prefix = prefix
  }
  var fullName: String {
    return (prefix != nil ? prefix! + " " : "") + name
  }
}

var ncc1701 = Starship(name: "Enterprise", prefix: "USS")
// ncc1701.fullName is "USS Enterprise"


// -----------------------------------------------------------------------------
//  Method Requirements
// -----------------------------------------------------------------------------

// Protocols can require specific instance methods and/or type methods to be
// implemented by conforming types.  These methods are written as part of
// the protocol's definition in the same way as for normal instance and
// type methods but without curly braces or a method body.  Variadic
// parameters are allowed, subject to the same rules as for normal
// methods.  Default values, however, cannot be specified for
// method parameters within a protocol's definition.

// As with type property requirements, you always prefix type method
// requirements with the static keyword when they are defined in a
// protocol.  This is true even though type method requirements
// are prefixed with the class or static keyword when
// implemented by a class:

protocol SomeProtocol {
  static func someMethod()
}

// A protocol with a single instance method requirement.
protocol RandomNumberGenerator {
  func random() -> Double
}

// `RandomNumberGenerator` does not make any assumptions about how each random
// number will be generated.  It simply requires the generator to provide a
// standard way to generate a new random number.

// Here's an implementation of a class that adopts and conforms to the
// RandomNumberGenerator protocol.  This class implements a
// pseudorandom number generator algorithm known as a
// linear congruential generator:

class LinearCongruentialGenerator: RandomNumberGenerator {
  var lastRandom = 42.0
  let m = 139968.0
  let a = 3877.0
  let c = 29573.0
  func random() -> Double {
    lastRandom = ((lastRandom * a + c).truncatingRemainder(dividingBy: m))
    return lastRandom / m
  }
}

let generator = LinearCongruentialGenerator()
print("Here's a random number: \(generator.random())")
// Prints "Here's a random number: 0.37464991998171"
print("And another one: \(generator.random())")
// Prints "And another one: 0.729023776863283"


// -----------------------------------------------------------------------------
//  Mutating Method Requirements
// -----------------------------------------------------------------------------

// It is sometimes necessary for a method to modify (or mutate) the instance it
// belongs to.  For instance methods on value types (that is, structures and
// enumerations) you place the `mutating` keyword before a method's `func`
// keyword to indicate that the method is allowed to modify the instance
// it belongs to and any properties of that instance. This process is
// described in Modifying Value Types from Within Instance Methods.

// If you define a protocol instance method requirement that is intended to
// mutate instances of any type that adopts the protocol, mark the method
// with the mutating keyword as part of the protocol’s definition.
// This enables structures and enumerations to adopt the protocol
// and satisfy that method requirement.

// NOTE
// If you mark a protocol instance method requirement as mutating, you do not
// need to write the mutating keyword when writing an implementation of that
// method for a class.  The mutating keyword is only used by structures
// and enumerations.

// The example below defines a protocol called Togglable, which defines a single instance method requirement called toggle. As its name suggests, the toggle() method is intended to toggle or invert the state of any conforming type, typically by modifying a property of that type.

// The toggle() method is marked with the mutating keyword as part of the Togglable protocol definition, to indicate that the method is expected to mutate the state of a conforming instance when it is called:

protocol Togglable {
    mutating func toggle()
}

// If you implement the Togglable protocol for a structure or enumeration, that structure or enumeration can conform to the protocol by providing an implementation of the toggle() method that is also marked as mutating.

// The example below defines an enumeration called OnOffSwitch. This enumeration toggles between two states, indicated by the enumeration cases on and off. The enumeration’s toggle implementation is marked as mutating, to match the Togglable protocol’s requirements:

enum OnOffSwitch: Togglable {
  case off, on
  mutating func toggle() {
    switch self {
    case .off:
      self = .on
    case .on:
      self = .off
    }
  }
}
var lightSwitch = OnOffSwitch.off
lightSwitch.toggle()
// lightSwitch is now equal to .on


// -----------------------------------------------------------------------------
//  Initializer Requirements
// -----------------------------------------------------------------------------

protocol Conformable {
  // Require conforming types to implement a specific initializer.
  init(x: Int)
}








// -----------------------------------------------------------------------------
//  Protocols as Types
// -----------------------------------------------------------------------------

// Protocols do not actually implement any functionality themselves.
// Nonetheless, any protocol you create will become a fully-fledged
// type for use in your code.

// Because it is a type, you can use a protocol in many places where
// other types are allowed.

// Here's an example of a protocol used as a type:

class Dice {
    let sides: Int
    let generator: RandomNumberGenerator
    init(sides: Int, generator: RandomNumberGenerator) {
        self.sides = sides
        self.generator = generator
    }
    func roll() -> Int {
        return Int(generator.random() * Double(sides)) + 1
    }
}

// This example defines a new class called Dice, which represents an n-sided dice for use in a board game. Dice instances have an integer property called sides, which represents how many sides they have, and a property called generator, which provides a random number generator from which to create dice roll values.

// The generator property is of type RandomNumberGenerator. Therefore, you can set it to an instance of any type that adopts the RandomNumberGenerator protocol. Nothing else is required of the instance you assign to this property, except that the instance must adopt the RandomNumberGenerator protocol.

// Dice also has an initializer, to set up its initial state. This initializer has a parameter called generator, which is also of type RandomNumberGenerator. You can pass a value of any conforming type in to this parameter when initializing a new Dice instance.

// Dice provides one instance method, roll, which returns an integer value between 1 and the number of sides on the dice. This method calls the generator’s random() method to create a new random number between 0.0 and 1.0, and uses this random number to create a dice roll value within the correct range. Because generator is known to adopt RandomNumberGenerator, it is guaranteed to have a random() method to call.

// Here’s how the Dice class can be used to create a six-sided dice with a LinearCongruentialGenerator instance as its random number generator:

var d6 = Dice(sides: 6, generator: LinearCongruentialGenerator())
for _ in 1...5 {
    print("Random dice roll is \(d6.roll())")
}
// Random dice roll is 3
// Random dice roll is 5
// Random dice roll is 4
// Random dice roll is 5
// Random dice roll is 4


// -----------------------------------------------------------------------------
//  Delegation
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Adding Protocol Conformance with an Extension
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Collections of Protocol Types
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Protocol Inheritance
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Class-Only Protocols
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Protocol Composition
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Checking for Protocol Conformance
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Optional Protocol Requirements
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Protocol Extensions
// -----------------------------------------------------------------------------
