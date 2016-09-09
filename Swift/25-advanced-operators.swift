// =============================================================================
//  Advanced Operators
//  https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/AdvancedOperators.html
// =============================================================================


// -----------------------------------------------------------------------------
//  Bitwise Operators
// -----------------------------------------------------------------------------

let a: UInt8 = 0b01101101
let b: UInt8 = 0b11001110

assert(~a == 0b10010010)          // Bitwise NOT.
assert(a & b == 0b01001100)       // Bitwise AND.
assert(a | b == 0b11101111)       // Bitwise OR.
assert(a ^ b == 0b10100011)       // Bitwise XOR.

// -----------------------------------------------------------------------------

// Shifting unsigned integers (logical shift).
let c: UInt8 = 0b00000100
assert(c << 1 == 0b00001000)        // Bitwise left shift operator.
assert(c << 2 == 0b00010000)
assert(c << 5 == 0b10000000)
assert(c << 6 == 0b00000000)

assert(c >> 1 == 0b00000010)        // Bitwise right shift operator.
assert(c >> 2 == 0b00000001)
assert(c >> 3 == 0b00000000)

// -----------------------------------------------------------------------------

// Shifting signed integers (arithmetic shift).
let d: Int8 = 0b01100110
let e: Int8 = -0b01100110
assert(d << 3 == 0b00110000)        // Bitwise left shift operator.
assert(e << 3 == -0b00110000)

assert(d >> 3 == 0b00001100)        // Bitwise right shift operator.
assert(e >> 1 == -0b00110011)
assert(e >> 2 == -0b00011010)
assert(e >> 3 == -0b00001101)
assert(e >> 4 == -0b00000111)
assert(e >> 5 == -0b00000100)

// -----------------------------------------------------------------------------

// Getting the components of a color.
let hotPink: UInt32 = 0xFF69B4
let redComponent = (hotPink & 0xFF0000) >> 16
let greenComponent = (hotPink & 0x00FF00) >> 8
let blueComponent = hotPink & 0x0000FF
assert(redComponent == 0xFF)
assert(greenComponent == 0x69)
assert(blueComponent == 0xB4)


// -----------------------------------------------------------------------------
//  Overflow Operators
// -----------------------------------------------------------------------------

// Normally, overflows are handled poorly.

// 32,767 is the maximum value an Int16 can hold.
var o0 = Int16.max
// The following is commented out because it causes the program to crash.
// o0 += 1

// Overflow operators allow overflows to happen.

// -----------------------------------------------------------------------------

// Overflow integer addition.
var o1 = UInt8.max
o1 = o1 &+ 1
assert(o1 == UInt8.min)
assert(UInt8.min == 0)
assert(UInt8.max == 255)

// -----------------------------------------------------------------------------

// Overflow operators don't let you know when an overflow happens
// but methods like `addWithOverflow` do.
// http://stackoverflow.com/a/35974079/1935675
var o2: Int8 = 100
o2 = o2 &+ o2
assert(o2 == -56)
o2 = 100
var o3 = Int8.addWithOverflow(o2, o2)
assert(o3.0 == -56)
assert(o3.overflow == true)

// -----------------------------------------------------------------------------

// Overflow integer subtraction.
var o4 = UInt8.min
o4 = o4 &- 1
assert(o4 == UInt8.max)

// -----------------------------------------------------------------------------

var o5 = Int8.min
o5 = o5 &- 1
assert(o5 == Int8.max)
assert(Int8.min == -128)
assert(Int8.max == 127)

// -----------------------------------------------------------------------------

// Overflow integer multiplication.
var o6: UInt8 = 4
o6 = o6 &* 196
assert(o6 == 16)


// -----------------------------------------------------------------------------
//  Precedence and Associativity
// -----------------------------------------------------------------------------

let x = 2 + 3 % 4 * 5
let y = 2 + ((3 % 4) * 5)
assert(x == y)
assert(y == 17)


// -----------------------------------------------------------------------------
//  Operator Functions
// -----------------------------------------------------------------------------

struct Vector2D {
  var x = 0.0, y = 0.0
}

// Overload the binary infix addition operator (`+`) for Vector2D values.
func + (left: Vector2D, right: Vector2D) -> Vector2D {
  return Vector2D(x: left.x + right.x, y: left.y + right.y)
}

let v1 = Vector2D(x: 3.0, y: 1.0)
let v2 = Vector2D(x: 2.0, y: 5.0)
let v3 = v1 + v2
assert(v3.x == 5.0)
assert(v3.y == 6.0)

// -----------------------------------------------------------------------------

// Overload the prefix negation operator (`-`) for Vector2D values.
prefix func - (vector: Vector2D) -> Vector2D {
  return Vector2D(x: -vector.x, y: -vector.y)
}

let v4 = -v3
assert(v4.x == -5.0)
assert(v4.y == -6.0)

let v5 = -v4
assert(v5.x == 5.0)
assert(v5.y == 6.0)

// -----------------------------------------------------------------------------

// Overload the addition assignment operator (`+=`) for Vector2D values.
// The `left` parameter is `inout` because our operator directly edits it.
func += (left: inout Vector2D, right: Vector2D) {
  left = left + right
}

var v6 = v1
v6 += v2
assert(v6.x == 5.0)
assert(v6.y == 6.0)

// It's not possible to overload the default assignment operator (`=`).
// Compound assignment operators can be overloaded.  The ternary conditional
// operator (`a ? b : c`) cannot be overloaded.

// -----------------------------------------------------------------------------

// Overload the equivalence operators (`==` and `!=`) for Vector2D values.
func == (left: Vector2D, right: Vector2D) -> Bool {
  return (left.x == right.x) && (left.y == right.y)
}
func != (left: Vector2D, right: Vector2D) -> Bool {
  return !(left == right)
}

assert(v3 == v5)
assert(v3 != v4)


// -----------------------------------------------------------------------------
//  Custom Operators
// -----------------------------------------------------------------------------

// The tokens `=`, `->`, `//`, `/*`, `*/`, `.`, the prefix operators `<`, `&`,
// and `?`, the infix operator `?`, and the postfix operators `>`, `!`, and `?`
// are reserved which means they can’t be overloaded or be used as custom
// operators.

// Define a new postfix unary operator called `+++`.
// No associativity is defined so default to `none`.
// No precedence is defined so default to `100`.
postfix operator +++ {}

postfix func +++ (v: inout Vector2D) -> Vector2D {
  v += v
  return v
}

var v7 = Vector2D(x: 2.0, y: 3.0)
let v8 = v7+++
assert(v8.x == 4.0)
assert(v8.y == 6.0)

// -----------------------------------------------------------------------------

// If both a prefix operator and postfix operator are applied to the same
// operand then the postfix operator is applied first.
var v9 = v8
v9 = -v9+++
assert(v9.x == -8.0)
assert(v9.y == -12.0)

// -----------------------------------------------------------------------------

// Define a new infix binary operator called `+-`.
infix operator +- { associativity left precedence 140 }

func +- (left: Vector2D, right: Vector2D) -> Vector2D {
  return Vector2D(x: left.x + right.x, y: left.y - right.y)
}

let v10 = v7 +- v8
assert(v10.x == 8.0)
assert(v10.y == 0.0)
