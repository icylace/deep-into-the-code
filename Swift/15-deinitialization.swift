// =============================================================================
//  Deinitialization
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Deinitialization.html
// =============================================================================

// -----------------------------------------------------------------------------
//  Deinitializer - A special code block that is automatically called
//                  right before a class instance is deallocated.
// -----------------------------------------------------------------------------

class Foo {
  deinit {
    // Perform the deinitialization.
  }
}

// TODO

// A deinitializer...
// ...is called automatically, just before instance deallocation takes place.
// ...can access all the properties of the instance it is called on.
// ...cannot be called directly.
// ...does not take any parameters.
// ...is only available on class types.
// ...can be defined at most once per class definition.

// A superclass deinitializer...
// ...is inherited by its subclasses,
// ...is called automatically at the end of a subclass deinitializer.
// ...is always called even if a subclass doesn't have its own deinitializer.

// Swift automatically deallocates your instances when they are no longer
// needed, to free up resources, rendering manual cleanup typically
// unnecessary.  However, when you are working with your own
// resources, like files, you might need to perform some
// additional manual cleanup, which is a good time to
// use a deinitializer.

// -----------------------------------------------------------------------------

class Bank {
  static var coins = 10_000
  static func distribute(coins coinsRequested: Int) -> Int {
    let coinsToVend = min(coinsRequested, coins)
    coins -= coinsToVend
    return coinsToVend
  }
  static func receive(coins coinsReturned: Int) {
    coins += coinsReturned
  }
}

class Player {
  var coins = 0
  func borrow(coinsToBorrow: Int) {
    coins += Bank.distribute(coins: coinsToBorrow)
  }
  deinit {
    Bank.receive(coins: coins)
  }
}

// A new player starts with no coins.
var player: Player? = Player()
assert(player!.coins == 0)
assert(Bank.coins == 10_000)

// `player` is optional because players can leave the game at any point.

// The player borrows coins from the bank.
player!.borrow(coins: 2100)
assert(player!.coins == 2100)
assert(Bank.coins == 7900)

// The player has left and the bank gets back all the
// coins the player had.
player = nil
assert(Bank.coins == 10_000)

// When `player` is set to `nil` its reference to the `Player` instance is
// broken.  No other properties or variables are still referring to that
// same `Player` instance, so it is deallocated in order to free up its
// memory.  Just before this happens its deinitializer is called
// automatically and its coins are returned to the bank.
