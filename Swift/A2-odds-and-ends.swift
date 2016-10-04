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
let a13: Array<Int> = type(of: []).init(arrayLiteral: 0, 1, 2)
let a14: Array<Int> = type(of: []).init([0, 1, 2])
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
let a28: [Int] = type(of: []).init(arrayLiteral: 0, 1, 2)
let a29: [Int] = type(of: []).init([0, 1, 2])
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
/*
let a43 = type(of: []).init(arrayLiteral: 0, 1, 2)
let a44 = type(of: []).init([0, 1, 2])
*/

// Confirm they're all equivalent.
assert([a01, a02, a03, a04, a05, a06, a07, a08, a09, a10, a11, a12, a13,
        a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26,
        a27, a28, a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39,
        a40, a41, a42, a45].filter({ $0 != [0, 1, 2] }).isEmpty)







// -----------------------------------------------------------------------------

// Primitive types.

_ = true                     // A boolean literal.
_ = 1                        // An integer literal.
_ = 2.2                      // A double floating-point literal.
_ = "way"                    // A string literal.
_ = [2, 3, 4]                // An array of integers.
_ = (1.3, "is", false)       // A tuple of double, string, and boolean.




_ = Double.infinity
_ = Float.infinity
_ = Float32.infinity
_ = Float64.infinity
_ = CDouble.infinity






(1...10).forEach {
  print($0)
}


import Foundation

while 0.5 < drand48() {
  print("still going...")
}






switch 1 {
  case 1:
    break
}

switch 1 {
  default:
    break
}

var aa = 1
switch aa {
  default:
    break
}




// ----

import Foundation

// The difference between
var p1: NSObject = NSObject()
// and
var p2: NSObject
p2 = NSObject()
// is that Xcode will complain about the former if `p` doesn't get mutated.


// ----












let tag: String? = nil
let tagResult = tag ?? "<none>"

// Alternate:
//
// let tagResult: String
// if let tag = tag {
//   tagResult = tag
// } else {
//   tagResult = "<none>"
// }







var answer = "42"
_ = type(of: 42)
_ = type(of: answer)
_ = type(of: 2.2)

// `type(of:)` is currently undocumented in the language guide.
// Original proposal:
// - https://github.com/apple/swift-evolution/blob/master/proposals/0136-memory-layout-of-values.md



_ = print.self


_ = Int()


_ = 2.advanced(by: 3)




_ = 0
_ = ""





print(Int.self)


let a = 0...3
_ = a.count
_ = a.self
_ = type(of: a)
print(a)

for _ in 0...3 {
  print("In a loop 4 times.")
}

for _ in a {
  print("This will also loop 4 times.")
}





print("test")
debugPrint("test")



// import UIKit






//
//
//
//
//



//autoreleasepool {}




//@testable
//import MyApp









// `dump()` crashes when trying to use it an `UnsafeMutablePointer`







// // http://stackoverflow.com/a/24102243/1935675
// import Darwin
// exit(0)
