// =============================================================================
// =============================================================================


// -----------------------------------------------------------------------------
//  Representing and Throwing Errors
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Handling Errors
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//  Specifying Cleanup Actions
// -----------------------------------------------------------------------------














//assert()
//assertionFailure()
//precondition()
//preconditionFailure()
//fatalError()

//abort()
//exit(1)

enum MyError: ErrorType {
  case Alpha
  case Bravo
  case Tango
}


//do {
////  let theResult = try obj.doDangerousStuff()
//}
//catch MyError.Alpha {
//  // Deal with badness.
//}
//catch MyError.Bravo {
//  // Deal with terribleness.
//}
//catch is ErrorType {
//  // Unexpected error!
//}


defer {}
