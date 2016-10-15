// =============================================================================
//  Deinitialization
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Deinitialization.html
// =============================================================================

// When instances are no longer needed they get automatically deallocated to
// free up resources like memory.  This typically renders manual cleanup
// unnecessary.  However, there might be times when additional manual
// cleanup is needed, like when working with your own resources such
// as files.  A deinitializer can be used for this.

// -----------------------------------------------------------------------------
//  Deinitializer - A special code block that is automatically called
//                  right before a class instance is deallocated.
// -----------------------------------------------------------------------------

// A deinitializer...
// ...can access all the properties of the instance it is called on.
// ...cannot be called directly.
// ...does not take any parameters.
// ...is only available on class types.
// ...can be defined at most once per class definition.

class Foo {
  deinit {
    print("Deinitializing instance of `Foo`...")
  }
}

var foo: Foo? = Foo()
foo = nil
// Output:
// Deinitializing instance of `Foo`...

// When `foo` is set to nil its reference to the `Foo` instance is broken.
// That instance is not still being referred to by something else, so its
// deinitializer is called and then it's deallocated.

// -----------------------------------------------------------------------------

// A superclass deinitializer...
// ...is inherited by its subclasses,
// ...is called automatically at the end of a subclass deinitializer.
// ...is always called even if a subclass doesn't have its own deinitializer.

class Bar: Foo {
  // `Bar` inherits the deinitializer of `Foo`.
}

class Baz: Bar {
  deinit {
    print("Deinitializing instance of `Baz`...")
  }
}

var baz: Baz? = Baz()
baz = nil
// Output:
// Deinitializing instance of `Baz`...
// Deinitializing instance of `Foo`...

// -----------------------------------------------------------------------------

class Bank {
  static var coins = 10_000
  static func distribute(coins: Int) -> Int {
    let coinsToVend = min(coins, self.coins)
    self.coins -= coinsToVend
    return coinsToVend
  }
  static func receive(coins: Int) {
    self.coins += coins
  }
}

class Player {
  var coins = 0
  func borrow(coins: Int) {
    self.coins += Bank.distribute(coins: coins)
  }
  deinit {
    Bank.receive(coins: coins)
  }
}

// Here, `player` is optional because players can leave at any point.
var player: Player? = Player()
assert(player!.coins == 0)
assert(Bank.coins == 10_000)

player!.borrow(coins: 2100)
assert(player!.coins == 2100)
assert(Bank.coins == 7900)

player!.borrow(coins: 200)
assert(player!.coins == 2300)
assert(Bank.coins == 7700)

// The player has left and the bank gets back the coins the player borrowed.
player = nil
assert(Bank.coins == 10_000)
