// =============================================================================
//  Collection Types
//  https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/CollectionTypes.html
// =============================================================================


// -----------------------------------------------------------------------------
//  Mutability of Collections
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Arrays
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Sets
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Performing Set Operations
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Dictionaries
// -----------------------------------------------------------------------------

































// All of these are equivalent:
let a01: Array<Int> = Array<Int>.init(arrayLiteral: 0, 1, 2)
let a02: Array<Int> = Array<Int>.init([0, 1, 2])
let a03: Array<Int> = Array<Int>(arrayLiteral: 0, 1, 2)
let a04: Array<Int> = Array<Int>([0, 1, 2])
let a05: Array<Int> = Array.init(arrayLiteral: 0, 1, 2)
let a06: Array<Int> = Array.init([0, 1, 2])
let a07: Array<Int> = Array(arrayLiteral: 0, 1, 2)
let a08: Array<Int> = Array([0, 1, 2])
let a09: Array<Int> = [Int].init(arrayLiteral: 0, 1, 2)
let a10: Array<Int> = [Int].init([0, 1, 2])
let a11: Array<Int> = [Int](arrayLiteral: 0, 1, 2)
let a12: Array<Int> = [Int]([0, 1, 2])
let a13: Array<Int> = [].dynamicType.init(arrayLiteral: 0, 1, 2)
let a14: Array<Int> = [].dynamicType.init([0, 1, 2])
let a15: Array<Int> = [0, 1, 2]
let a16: [Int] = Array<Int>.init(arrayLiteral: 0, 1, 2)
let a17: [Int] = Array<Int>.init([0, 1, 2])
let a18: [Int] = Array<Int>(arrayLiteral: 0, 1, 2)
let a19: [Int] = Array<Int>([0, 1, 2])
let a20: [Int] = Array.init(arrayLiteral: 0, 1, 2)
let a21: [Int] = Array.init([0, 1, 2])
let a22: [Int] = Array(arrayLiteral: 0, 1, 2)
let a23: [Int] = Array([0, 1, 2])
let a24: [Int] = [Int].init(arrayLiteral: 0, 1, 2)
let a25: [Int] = [Int].init([0, 1, 2])
let a26: [Int] = [Int](arrayLiteral: 0, 1, 2)
let a27: [Int] = [Int]([0, 1, 2])
let a28: [Int] = [].dynamicType.init(arrayLiteral: 0, 1, 2)
let a29: [Int] = [].dynamicType.init([0, 1, 2])
let a30: [Int] = [0, 1, 2]
let a31 = Array<Int>.init(arrayLiteral: 0, 1, 2)
let a32 = Array<Int>.init([0, 1, 2])
let a33 = Array<Int>(arrayLiteral: 0, 1, 2)
let a34 = Array<Int>([0, 1, 2])
let a35 = Array.init(arrayLiteral: 0, 1, 2)
let a36 = Array.init([0, 1, 2])
let a37 = Array(arrayLiteral: 0, 1, 2)
let a38 = Array([0, 1, 2])
let a39 = [Int].init(arrayLiteral: 0, 1, 2)
let a40 = [Int].init([0, 1, 2])
let a41 = [Int](arrayLiteral: 0, 1, 2)
let a42 = [Int]([0, 1, 2])
let a45 = [0, 1, 2]

// Interestingly, these lines would cause the program to crash which is why
// they're commented out.
// let a43 = [].dynamicType.init(arrayLiteral: 0, 1, 2)
// let a44 = [].dynamicType.init([0, 1, 2])

assert(a01 == a02 && a02 == a03 && a03 == a04 && a04 == a05 && a05 == a06 &&
       a06 == a07 && a07 == a08 && a08 == a09 && a09 == a10 && a10 == a11 &&
       a11 == a12 && a12 == a13 && a13 == a14 && a14 == a15 && a15 == a16 &&
       a16 == a17 && a17 == a18 && a18 == a19 && a19 == a20 && a20 == a21 &&
       a21 == a22 && a22 == a23 && a23 == a24 && a24 == a25 && a25 == a26 &&
       a26 == a27 && a27 == a28 && a28 == a29 && a29 == a30 && a30 == a31 &&
       a31 == a32 && a32 == a33 && a33 == a34 && a34 == a35 && a35 == a36 &&
       a36 == a37 && a37 == a38 && a38 == a39 && a39 == a40 && a40 == a41 &&
       a41 == a42 && a42 == a45 && a45 == [0, 1, 2])
