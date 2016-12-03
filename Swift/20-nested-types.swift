// It can be convenient to define utility types purely for use within the
// context of a more complex type.  To achieve this, types can be nested
// inside other types.  Types can be nested to any level required.

// In Blackjack, the Ace cards have a value of either one or eleven.
// This is represented by `Values` which is nested within `Rank`.
struct BlackjackCard {

  enum Suit: Character {
    case spades = "♠", hearts = "♡", diamonds = "♢", clubs = "♣"
  }

  enum Rank: Int {
    case two = 2, three, four, five, six, seven, eight, nine, ten
    case jack, queen, king, ace
    struct Values {
      let first: Int, second: Int?
    }
    var values: Values {
      switch self {
      case .ace:
        return Values(first: 1, second: 11)
      case .jack, .queen, .king:
        return Values(first: 10, second: nil)
      default:
        return Values(first: self.rawValue, second: nil)
      }
    }
  }

  let rank: Rank, suit: Suit

  var description: String {
    var output = "suit is \(suit.rawValue),"
    output += " value is \(rank.values.first)"
    if let second = rank.values.second {
      output += " or \(second)"
    }
    return output
  }

}

// Though `Rank` and `Suit` are nested within `BlackjackCard` their types are
// inferred from context, so initialization of this instance is able to refer
// to the enumeration cases by their case names (.ace and .spades) alone.
let theAceOfSpades = BlackjackCard(rank: .ace, suit: .spades)
assert(theAceOfSpades.description == "suit is ♠, value is 1 or 11")

// -----------------------------------------------------------------------------

// To use a nested type outside of its definition context, prefix its name with
// the name of the type it is nested within.

// The names of `Suit`, `Rank`, and `Values` can be kept deliberately short
// because their names are qualified by the context of their definitions.
let heartsSymbol = BlackjackCard.Suit.hearts.rawValue
assert(heartsSymbol == "♡")

// TODO
// - add the following to the style guide with an appropriate reference.
// Keep the names of nested types short since they can take advantage of being
// qualified by the context of their definitions when they're being used.


// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - Nested Types
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/NestedTypes.html
// =============================================================================
