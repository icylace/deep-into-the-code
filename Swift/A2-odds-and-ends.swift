// -----------------------------------------------------------------------------

// Primitive types.

true                     // A boolean literal.
1                        // An integer literal.
2.2                      // A double floating-point literal.
"way"                    // A string literal.
[2, 3, 4]                // An array of integers.
(1.3, "is", false)       // A tuple of double, string, and boolean.




Double.infinity
Float.infinity
Float32.infinity
Float64.infinity
CDouble.infinity






(1...10).forEach {
  print($0)
}


import Foundation

while 0.5 < drand48() {
  print("still going...")
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







42.dynamicType
answer.dynamicType

2.2.dynamicType



_ = print.self


_ = Int()


2.advancedBy(3)




_ = 0
_ = ""





print(Int.self)


let a = 0...3
a.count
a.self
a.dynamicType
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
