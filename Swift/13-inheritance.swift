// -----------------------------------------------------------------------------
//  Inheritance - The behavior through which a class can take on the methods,
//                properties, and other characteristics of another class.
//  Subclass - A class that inherits from another class.
//  Superclass - A class that is inherited from by another class.
//  Base class - A class that doesn't inherit from another class.
// -----------------------------------------------------------------------------

// TODO

/*

// A Swift class...
// ...unlike other types, can inherit from another class.
// ...can use methods, properties, and subscripts belonging to its superclasses
//    and can provide its own overriding versions of them to refine or modify
//    their behavior.
// ...can add property observers to inherited properties, regardless of whether
//    they were originally defined as stored or computed properties.
// ...doesn't inherit from a universal base class but it can
//    itself be a base class which can be built upon.

class Vehicle {
  var currentSpeed = 0.0
  var description: String {
    return "Traveling at \(currentSpeed) miles per hour."
  }
  func makeNoise() -> String {
    // An arbitrary vehicle doesn't necessarily make a noise.
    return ""
  }
}

let someVehicle = Vehicle()
assert(someVehicle.description == "Traveling at 0.0 miles per hour.")

// The Vehicle class defines common characteristics for an arbitrary vehicle,
// but is not much use in itself. To make it more useful, you need to refine
// it to describe more specific kinds of vehicles.

// -----------------------------------------------------------------------------
//  Subclassing - The act of basing a new class on an existing class.
// -----------------------------------------------------------------------------

// A subclass...
// ...inherits characteristics from an existing class which can then be refined.
// ...can have new characteristics added to it.

// `Bicycle` inherits from `Vehicle`.
class Bicycle: Vehicle {
  var hasBasket = false
}

let bicycle = Bicycle()
bicycle.hasBasket = true
bicycle.currentSpeed = 15.0
assert(bicycle.description == "traveling at 15.0 miles per hour")

// Subclasses can themselves be subclassed.

class Tandem: Bicycle {
  var currentNumberOfPassengers = 0
}

let tandem = Tandem()
tandem.hasBasket = true
tandem.currentNumberOfPassengers = 2
tandem.currentSpeed = 22.0
assert(tandem.description == "Traveling at 22.0 miles per hour.")

// -----------------------------------------------------------------------------
//  Overriding - The act of a subclass providing its own custom implementation
//               of an instance/type method, instance/type property, or
//               subscript that would otherwise be inherited.
// -----------------------------------------------------------------------------

To override a characteristic that would otherwise be inherited, you prefix your
overriding definition with the override keyword.  Doing so clarifies that you
intend to provide an override and have not provided a matching definition by
mistake.  Overriding by accident can cause unexpected behavior, and any overrides
without the override keyword are diagnosed as an error when your code is compiled.

The override keyword also prompts the Swift compiler to check that your overriding
class's superclass (or one of its parents) has a declaration that matches the one
you provided for the override.  This check ensures that your overriding definition
is correct.

// -----------------------------------------------------------------------------

Accessing Superclass Methods, Properties, and Subscripts

In a method, property, or subscript override it can be useful to use
the existing superclass implementation as part of the override.
For example, you can refine the behavior of that existing
implementation, or store a modified value in an existing
inherited variable.

Where this is appropriate, you access the superclass version of a method, property,
or subscript by using the super prefix:

An overridden method named someMethod() can call the superclass version of
someMethod() by calling super.someMethod() within the overriding method implementation.

An overridden property called someProperty can access the superclass version of
someProperty as super.someProperty within the overriding getter or setter implementation.

An overridden subscript for someIndex can access the superclass version of the
same subscript as super[someIndex] from within the overriding subscript implementation.

// -----------------------------------------------------------------------------

// An inherited instance or type method can be overridden.

class Train: Vehicle {
  override func makeNoise() -> String {
    return "Choo Choo"
  }
}

let train = Train()
assert(train.makeNoise() == "Choo Choo")

// -----------------------------------------------------------------------------

Overriding Properties

You can override an inherited instance or type property to provide your own
custom getter and setter for that property, or to add property observers.

// -----------------------------------------------------------------------------

Overriding Property Getters and Setters

You can provide a custom getter (and setter, if appropriate) to override
any inherited property, regardless of whether the inherited property is
originally implemented as a stored or computed property.  The stored or
computed nature of an inherited property is not known by a subclass
The subclass only knows that the inherited property has a certain
name and type.  You must always state both the name and the type
of the property you are overriding, so the compiler can check
that your override matches a superclass property with the
same name and type.

You can present an inherited read-only property as a read-write property by
providing both a getter and a setter in your subclass property override.

You cannot, however, present an inherited read-write property as a
read-only property.

NOTE

If you provide a setter as part of a property override, you must also provide a
getter for that override.  If you don't want to modify the inherited property's
value within the overriding getter, you can simply pass through the inherited
value by returning super.someProperty from the getter, where someProperty is
the name of the property you are overriding.

class Car: Vehicle {
  var gear = 1
  override var description: String {
    return super.description + ".. in gear \(gear)."
  }
}

let car = Car()
car.currentSpeed = 25.0
car.gear = 3
assert(car.description == "Traveling at 25.0 miles per hour... in gear 3.")

// -----------------------------------------------------------------------------

Overriding Property Observers

You can use property overriding to add property observers to an inherited property.
This enables you to be notified when the value of an inherited property changes,
regardless of how that property was originally implemented. For more information
on property observers, see Property Observers.

NOTE

You cannot add property observers to inherited constant stored properties or
inherited read-only computed properties.  The value of these properties cannot
be set, and so it is not appropriate to provide a `willSet` or `didSet`
implementation as part of an override.

Note also that you cannot provide both an overriding setter and an overriding
property observer for the same property.  If you want to observe changes to a
property’s value, and you are already providing a custom setter for that property,
you can simply observe any value changes from within the custom setter.

The following example defines a new class called AutomaticCar, which is a subclass
of Car.  The AutomaticCar class represents a car with an automatic gearbox, which
automatically selects an appropriate gear to use based on the current speed:

class AutomaticCar: Car {
  override var currentSpeed: Double {
    didSet {
      gear = Int(currentSpeed / 10.0) + 1
    }
  }
}

let automatic = AutomaticCar()
automatic.currentSpeed = 35.0
assert(automatic.description == "Traveling at 35.0 miles per hour... in gear 4.")

// -----------------------------------------------------------------------------
//  Preventing Overrides
// -----------------------------------------------------------------------------

You can prevent a method, property, or subscript from being overridden by
marking it as final.

Do this by writing the final modifier before the method, property, or
subscript's introducer keyword (such as final var, final func, final
class func, and final subscript).

Any attempt to override a final method, property, or subscript in a subclass is
reported as a compile-time error.  Methods, properties, or subscripts that you
add to a class in an extension can also be marked as final within the
extension's definition.

You can mark an entire class as final by writing the final modifier before the
class keyword in its class definition (final class).  Any attempt to subclass a
final class is reported as a compile-time error.

*/


// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - Inheritance
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Inheritance.html
// =============================================================================
