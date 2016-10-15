// =============================================================================
//  Advanced Operators
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/AdvancedOperators.html
// =============================================================================

// TODO

// Arithmetic operators in Swift do not overflow by default.  Overflow behavior
// is trapped and reported as an error.  To opt in to overflow behavior, use
// Swift's second set of arithmetic operators that overflow by default, such
// as the overflow addition operator (&+).  All of these overflow operators
// begin with an ampersand (&).

/*

When you define your own structures, classes, and enumerations, it can be useful
to provide your own implementations of the standard Swift operators for these
custom types. Swift makes it easy to provide tailored implementations of these
operators and to determine exactly what their behavior should be for each type
you create.

You’re not limited to the predefined operators. Swift gives you the freedom to
define your own custom infix, prefix, postfix, and assignment operators, with
custom precedence and associativity values. These operators can be used and
adopted in your code like any of the predefined operators, and you can even
extend existing types to support the custom operators you define.

*/



// -----------------------------------------------------------------------------
//  Bitwise operator - An operator that manipulates data on a bit-by-bit basis.
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
//  Bitwise NOT operator (`~`) - The bitwise operator that inverts all bits.
// -----------------------------------------------------------------------------

let a: UInt8 = 0b01101101       // Equals 109 in decimal.
let b: UInt8 = 0b11001110       // Equals 206 in decimal.

assert(~a == 0b10010010)        // Equals 146 in decimal.

// -----------------------------------------------------------------------------
//  Bitwise AND operator (`&`) - The bitwise operator that returns a number
//                               whose bits are set to 1 only if the bits
//                               are equal to 1 in both operands.
// -----------------------------------------------------------------------------

assert(a & b == 0b01001100)       // Equals 76 in decimal.

// -----------------------------------------------------------------------------
//  Bitwise OR operator (`|`) - The bitwise operator that returns a number
//                              whose bits are set to 1 only if the bits
//                              are equal to 1 in either operand.
// -----------------------------------------------------------------------------

assert(a | b == 0b11101111)       // Equals 239 in decimal.

// -----------------------------------------------------------------------------
//  Bitwise XOR operator (`^`) - The bitwise operator that returns a number
//  (XOR is pronounced           whose bits are set to 1 only if the bits
//  as "exclusive OR")           in both operands are different from
//                               each other.
// -----------------------------------------------------------------------------

assert(a ^ b == 0b10100011)       // Equals 163 in decimal.

// -----------------------------------------------------------------------------
//  Bit-shifting - The behavior of moving all the bits in a number to either
//                 the left or right by a given number of places.
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
//  Bitwise left shift operator (`<<`) - The bitwise operator that bit-shifts
//                                       a number to the left.
// -----------------------------------------------------------------------------

// A bitwise left shift multiplies an integer by a factor of two.  For example,
// shifting an integer's bits to the left by one position doubles its value.

// -----------------------------------------------------------------------------
//  Bitwise right shift operator (`>>`) - The bitwise operator that bit-shifts
//                                        a number to the right.
// -----------------------------------------------------------------------------

// A bitwise right shift divides an integer by a factor of two.  For example,
// shifting an integer's bits to the right by one position halves its value.

// -----------------------------------------------------------------------------
//  Logical shift - The bit-shifting behavior for unsigned integers.
// -----------------------------------------------------------------------------

// The rules for logical shifting:
//
// - Existing bits are moved to the left or right by the requested number
//   of places.
// - Any bits that are moved beyond the bounds of the integer's storage
//   are discarded.
// - Zeros are inserted in the spaces left behind after the original bits
//   are moved to the left or right.

var c: UInt8 = 0b11111111

// Left shifting unsigned integers (logical shift).
assert(c << 1 == 0b11111110)
assert(c << 2 == 0b11111100)
assert(c << 5 == 0b11100000)
assert(c << 6 == 0b11000000)

// Right shifting unsigned integers (logical shift).
assert(c >> 1 == 0b01111111)
assert(c >> 2 == 0b00111111)
assert(c >> 5 == 0b00000111)
assert(c >> 6 == 0b00000011)

c = 0b00000100

assert(c << 1 == 0b00001000)
assert(c << 2 == 0b00010000)
assert(c << 5 == 0b10000000)
assert(c << 6 == 0b00000000)

assert(c >> 1 == 0b00000010)
assert(c >> 2 == 0b00000001)
assert(c >> 5 == 0b00000000)
assert(c >> 6 == 0b00000000)

// -----------------------------------------------------------------------------
//  Arithmetic shift - The bit-shifting behavior for signed integers.
// -----------------------------------------------------------------------------

// The shifting behavior is more complex for signed integers than for unsigned
// integers because of the way signed integers are represented in binary.

// -----------------------------------------------------------------------------
//  Sign bit - The most significant bit of a signed integer that indicates
//             whether the integer is negative (1) or not (0).
//  Value bits - The bits aside from the sign bit in a signed integer which
//               stores the absolute value of the signed integer.
// -----------------------------------------------------------------------------

Negative numbers, however, are stored differently. They are stored by
subtracting their absolute value from 2 to the power of n, where n is the
number of value bits. An eight-bit number has seven value bits, so this
means 2 to the power of 7, or 128.

Here’s how the bits inside an Int8 look for the number -4:

image: ../Art/bitshiftSignedMinusFour_2x.png

This time, the sign bit is 1 (meaning “negative”), and the seven value bits have
a binary value of 124 (which is 128 - 4):

image: ../Art/bitshiftSignedMinusFourValue_2x.png

This encoding for negative numbers is known as a two’s complement representation.
It may seem an unusual way to represent negative numbers, but it has several advantages.

First, you can add -1 to -4, simply by performing a standard binary addition of
all eight bits (including the sign bit), and discarding anything that doesn’t fit
in the eight bits once you’re done:

image: ../Art/bitshiftSignedAddition_2x.png

Second, the two’s complement representation also lets you shift the bits of
negative numbers to the left and right like positive numbers, and still end
up doubling them for every shift you make to the left, or halving them for
every shift you make to the right. To achieve this, an extra rule is used
when signed integers are shifted to the right: When you shift signed
integers to the right, apply the same rules as for unsigned integers,
but fill any empty bits on the left with the sign bit, rather than
with a zero.

image: ../Art/bitshiftSigned_2x.png

This action ensures that signed integers have the same sign after they are
shifted to the right, and is known as an arithmetic shift.

Because of the special way that positive and negative numbers are stored,
shifting either of them to the right moves them closer to zero. Keeping
the sign bit the same during this shift means that negative integers
remain negative as their value moves closer to zero.






let d: Int8 = 0b01100110
let e: Int8 = -0b01100110

// Left shifting signed integers (arithmetic shift).
assert(d << 3 == 0b00110000)
assert(e << 3 == -0b00110000)

// Right shifting signed integers (arithmetic shift).
assert(d >> 3 == 0b00001100)
assert(e >> 1 == -0b00110011)
assert(e >> 2 == -0b00011010)
assert(e >> 3 == -0b00001101)
assert(e >> 4 == -0b00000111)
assert(e >> 5 == -0b00000100)










// -----------------------------------------------------------------------------

// Getting the components of a color.
let hotPink: UInt32 = 0xFF69B4
let redPart = (hotPink & 0xFF0000) >> 16
let greenPart = (hotPink & 0x00FF00) >> 8
let bluePart = hotPink & 0x0000FF
assert(redPart == 0xFF)
assert(greenPart == 0x69)
assert(bluePart == 0xB4)



























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
postfix operator +++

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
infix operator +-: AdditionPrecedence

func +- (left: Vector2D, right: Vector2D) -> Vector2D {
  return Vector2D(x: left.x + right.x, y: left.y - right.y)
}

let v10 = v7 +- v8
assert(v10.x == 8.0)
assert(v10.y == 0.0)









// Custom ternary operators are not doable per se.















/*


Overflow Operators

If you try to insert a number into an integer constant or variable that cannot hold that value, by default Swift reports an error rather than allowing an invalid value to be created. This behavior gives extra safety when you work with numbers that are too large or too small.

For example, the Int16 integer type can hold any signed integer between -32768 and 32767. Trying to set an Int16 constant or variable to a number outside of this range causes an error:

var potentialOverflow = Int16.max
// potentialOverflow equals 32767, which is the maximum value an Int16 can hold
potentialOverflow += 1
// this causes an error
Providing error handling when values get too large or too small gives you much more flexibility when coding for boundary value conditions.

However, when you specifically want an overflow condition to truncate the number of available bits, you can opt in to this behavior rather than triggering an error. Swift provides three arithmetic overflow operators that opt in to the overflow behavior for integer calculations. These operators all begin with an ampersand (&):

Overflow addition (&+)
Overflow subtraction (&-)
Overflow multiplication (&*)
Value Overflow

Numbers can overflow in both the positive and negative direction.

Here’s an example of what happens when an unsigned integer is allowed to overflow in the positive direction, using the overflow addition operator (&+):

var unsignedOverflow = UInt8.max
// unsignedOverflow equals 255, which is the maximum value a UInt8 can hold
unsignedOverflow = unsignedOverflow &+ 1
// unsignedOverflow is now equal to 0
The variable unsignedOverflow is initialized with the maximum value a UInt8 can hold (255, or 11111111 in binary). It is then incremented by 1 using the overflow addition operator (&+). This pushes its binary representation just over the size that a UInt8 can hold, causing it to overflow beyond its bounds, as shown in the diagram below. The value that remains within the bounds of the UInt8 after the overflow addition is 00000000, or zero.

image: ../Art/overflowAddition_2x.png
Something similar happens when an unsigned integer is allowed to overflow in the negative direction. Here’s an example using the overflow subtraction operator (&-):

var unsignedOverflow = UInt8.min
// unsignedOverflow equals 0, which is the minimum value a UInt8 can hold
unsignedOverflow = unsignedOverflow &- 1
// unsignedOverflow is now equal to 255
The minimum value that a UInt8 can hold is zero, or 00000000 in binary. If you subtract 1 from 00000000 using the overflow subtraction operator (&-), the number will overflow and wrap around to 11111111, or 255 in decimal.

image: ../Art/overflowUnsignedSubtraction_2x.png
Overflow also occurs for signed integers. All addition and subtraction for signed integers is performed in bitwise fashion, with the sign bit included as part of the numbers being added or subtracted, as described in Bitwise Left and Right Shift Operators.

var signedOverflow = Int8.min
// signedOverflow equals -128, which is the minimum value an Int8 can hold
signedOverflow = signedOverflow &- 1
// signedOverflow is now equal to 127
The minimum value that an Int8 can hold is -128, or 10000000 in binary. Subtracting 1 from this binary number with the overflow operator gives a binary value of 01111111, which toggles the sign bit and gives positive 127, the maximum positive value that an Int8 can hold.

image: ../Art/overflowSignedSubtraction_2x.png
For both signed and unsigned integers, overflow in the positive direction wraps around from the maximum valid integer value back to the minimum, and overflow in the negative direction wraps around from the minimum value to the maximum.

Precedence and Associativity

Operator precedence gives some operators higher priority than others; these operators are applied first.

Operator associativity defines how operators of the same precedence are grouped together—either grouped from the left, or grouped from the right. Think of it as meaning “they associate with the expression to their left,” or “they associate with the expression to their right.”

It is important to consider each operator’s precedence and associativity when working out the order in which a compound expression will be calculated. For example, operator precedence explains why the following expression equals 17.

2 + 3 % 4 * 5
// this equals 17
If you read strictly from left to right, you might expect the expression to be calculated as follows:

2 plus 3 equals 5
5 remainder 4 equals 1
1 times 5 equals 5
However, the actual answer is 17, not 5. Higher-precedence operators are evaluated before lower-precedence ones. In Swift, as in C, the remainder operator (%) and the multiplication operator (*) have a higher precedence than the addition operator (+). As a result, they are both evaluated before the addition is considered.

However, remainder and multiplication have the same precedence as each other. To work out the exact evaluation order to use, you also need to consider their associativity. Remainder and multiplication both associate with the expression to their left. Think of this as adding implicit parentheses around these parts of the expression, starting from their left:

2 + ((3 % 4) * 5)
(3 % 4) is 3, so this is equivalent to:

2 + (3 * 5)
(3 * 5) is 15, so this is equivalent to:

2 + 15
This calculation yields the final answer of 17.

For a complete list of Swift operator precedences and associativity rules, see Expressions. For information about the operators provided by the Swift standard library, see Swift Standard Library Operators Reference.

NOTE

Swift’s operator precedences and associativity rules are simpler and more predictable than those found in C and Objective-C. However, this means that they are not exactly the same as in C-based languages. Be careful to ensure that operator interactions still behave in the way you intend when porting existing code to Swift.

Operator Methods

Classes and structures can provide their own implementations of existing operators. This is known as overloading the existing operators.

The example below shows how to implement the arithmetic addition operator (+) for a custom structure. The arithmetic addition operator is a binary operator because it operates on two targets and is said to be infix because it appears in between those two targets.

The example defines a Vector2D structure for a two-dimensional position vector (x, y), followed by a definition of an operator method to add together instances of the Vector2D structure:

struct Vector2D {
    var x = 0.0, y = 0.0
}

extension Vector2D {
    static func + (left: Vector2D, right: Vector2D) -> Vector2D {
        return Vector2D(x: left.x + right.x, y: left.y + right.y)
    }
}
The operator method is defined as a type method on Vector2D, with a method name that matches the operator to be overloaded (+). Because addition isn’t part of the essential behavior for a vector, the type method is defined in an extension of Vector2D rather than in the main structure declaration of Vector2D. Because the arithmetic addition operator is a binary operator, this operator method takes two input parameters of type Vector2D and returns a single output value, also of type Vector2D.

In this implementation, the input parameters are named left and right to represent the Vector2D instances that will be on the left side and right side of the + operator. The method returns a new Vector2D instance, whose x and y properties are initialized with the sum of the x and y properties from the two Vector2D instances that are added together.

The type method can be used as an infix operator between existing Vector2D instances:

let vector = Vector2D(x: 3.0, y: 1.0)
let anotherVector = Vector2D(x: 2.0, y: 4.0)
let combinedVector = vector + anotherVector
// combinedVector is a Vector2D instance with values of (5.0, 5.0)
This example adds together the vectors (3.0, 1.0) and (2.0, 4.0) to make the vector (5.0, 5.0), as illustrated below.

image: ../Art/vectorAddition_2x.png
Prefix and Postfix Operators

The example shown above demonstrates a custom implementation of a binary infix operator. Classes and structures can also provide implementations of the standard unary operators. Unary operators operate on a single target. They are prefix if they precede their target (such as -a) and postfix operators if they follow their target (such as b!).

You implement a prefix or postfix unary operator by writing the prefix or postfix modifier before the func keyword when declaring the operator method:

extension Vector2D {
    static prefix func - (vector: Vector2D) -> Vector2D {
        return Vector2D(x: -vector.x, y: -vector.y)
    }
}
The example above implements the unary minus operator (-a) for Vector2D instances. The unary minus operator is a prefix operator, and so this method has to be qualified with the prefix modifier.

For simple numeric values, the unary minus operator converts positive numbers into their negative equivalent and vice versa. The corresponding implementation for Vector2D instances performs this operation on both the x and y properties:

let positive = Vector2D(x: 3.0, y: 4.0)
let negative = -positive
// negative is a Vector2D instance with values of (-3.0, -4.0)
let alsoPositive = -negative
// alsoPositive is a Vector2D instance with values of (3.0, 4.0)
Compound Assignment Operators

Compound assignment operators combine assignment (=) with another operation. For example, the addition assignment operator (+=) combines addition and assignment into a single operation. You mark a compound assignment operator’s left input parameter type as inout, because the parameter’s value will be modified directly from within the operator method.

The example below implements an addition assignment operator method for Vector2D instances:

extension Vector2D {
    static func += (left: inout Vector2D, right: Vector2D) {
        left = left + right
    }
}
Because an addition operator was defined earlier, you don’t need to reimplement the addition process here. Instead, the addition assignment operator method takes advantage of the existing addition operator method, and uses it to set the left value to be the left value plus the right value:

var original = Vector2D(x: 1.0, y: 2.0)
let vectorToAdd = Vector2D(x: 3.0, y: 4.0)
original += vectorToAdd
// original now has values of (4.0, 6.0)
NOTE

It is not possible to overload the default assignment operator (=). Only the compound assignment operators can be overloaded. Similarly, the ternary conditional operator (a ? b : c) cannot be overloaded.

Equivalence Operators

Custom classes and structures do not receive a default implementation of the equivalence operators, known as the “equal to” operator (==) and “not equal to” operator (!=). It is not possible for Swift to guess what would qualify as “equal” for your own custom types, because the meaning of “equal” depends on the roles that those types play in your code.

To use the equivalence operators to check for equivalence of your own custom type, provide an implementation of the operators in the same way as for other infix operators:

extension Vector2D {
    static func == (left: Vector2D, right: Vector2D) -> Bool {
        return (left.x == right.x) && (left.y == right.y)
    }
    static func != (left: Vector2D, right: Vector2D) -> Bool {
        return !(left == right)
    }
}
The above example implements an “equal to” operator (==) to check if two Vector2D instances have equivalent values. In the context of Vector2D, it makes sense to consider “equal” as meaning “both instances have the same x values and y values”, and so this is the logic used by the operator implementation. The example also implements the “not equal to” operator (!=), which simply returns the inverse of the result of the “equal to” operator.

You can now use these operators to check whether two Vector2D instances are equivalent:

let twoThree = Vector2D(x: 2.0, y: 3.0)
let anotherTwoThree = Vector2D(x: 2.0, y: 3.0)
if twoThree == anotherTwoThree {
    print("These two vectors are equivalent.")
}
// Prints "These two vectors are equivalent."
Custom Operators

You can declare and implement your own custom operators in addition to the standard operators provided by Swift. For a list of characters that can be used to define custom operators, see Operators.

New operators are declared at a global level using the operator keyword, and are marked with the prefix, infix or postfix modifiers:

prefix operator +++
The example above defines a new prefix operator called +++. This operator does not have an existing meaning in Swift, and so it is given its own custom meaning below in the specific context of working with Vector2D instances. For the purposes of this example, +++ is treated as a new “prefix doubling” operator. It doubles the x and y values of a Vector2D instance, by adding the vector to itself with the addition assignment operator defined earlier. To implement the +++ operator, you add a type method called +++ to Vector2D as follows:

extension Vector2D {
    static prefix func +++ (vector: inout Vector2D) -> Vector2D {
        vector += vector
        return vector
    }
}

var toBeDoubled = Vector2D(x: 1.0, y: 4.0)
let afterDoubling = +++toBeDoubled
// toBeDoubled now has values of (2.0, 8.0)
// afterDoubling also has values of (2.0, 8.0)
Precedence for Custom Infix Operators

Custom infix operators each belong to a precedence group. A precedence group specifies an operator’s precedence relative to other infix operators, as well as the operator’s associativity. See Precedence and Associativity for an explanation of how these characteristics affect an infix operator’s interaction with other infix operators.

A custom infix operator that is not explicitly placed into a precedence group is given a default precedence group with a precedence immediately higher than the precedence of the ternary conditional operator.

The following example defines a new custom infix operator called +-, which belongs to the precedence group AdditionPrecedence:

infix operator +-: AdditionPrecedence
extension Vector2D {
    static func +- (left: Vector2D, right: Vector2D) -> Vector2D {
        return Vector2D(x: left.x + right.x, y: left.y - right.y)
    }
}
let firstVector = Vector2D(x: 1.0, y: 2.0)
let secondVector = Vector2D(x: 3.0, y: 4.0)
let plusMinusVector = firstVector +- secondVector
// plusMinusVector is a Vector2D instance with values of (4.0, -2.0)
This operator adds together the x values of two vectors, and subtracts the y value of the second vector from the first. Because it is in essence an “additive” operator, it has been given the same precedence group as additive infix operators such as + and -. For a complete list of the operator precedence groups and associativity settings, for the operators provided by the Swift standard library, see Swift Standard Library Operators Reference. For more information about precedence groups and to see the syntax for defining your own operators and precedence groups, see Operator Declaration.

NOTE

You do not specify a precedence when defining a prefix or postfix operator. However, if you apply both a prefix and a postfix operator to the same operand, the postfix operator is applied first.

*/
