// -----------------------------------------------------------------------------
//  Collection type - A type for storing groups of values.
// -----------------------------------------------------------------------------

// If you assign a collection to a variable it will be mutable, meaning it can
// change (or mutate) by having items added, removed, or changed.

// If you assign a collection to a constant it will be immutable, meaning its
// size and contents cannot be changed.

// -----------------------------------------------------------------------------
//  Array - A collection type having ordered values of similar type.
// -----------------------------------------------------------------------------

// The same value can appear in an array multiple times at different positions.

// The array's type is written in full as `Array<Element>`, where `Element`
// is the type of values the array is allowed to store.  The shorthand
// form as `[Element]`.

// Swift's `Array` type is bridged to Foundation's `NSArray` class.

// -----------------------------------------------------------------------------

// TODO



// var c: [[[[[[Int]]]]]] = [[2]]


// You can initialize an array with an array literal.

var a8: [String] = ["Eggs", "Milk"]
// `a8` is declared as "an array of string values".



// An empty array can be created with a certain type using initializer syntax.

var a1 = [Int]()
assert(type(of: a1) == [Int].self)

// An array's read-only `count` property keeps track of how many items it has.

assert(a1.count == 0)

// -----------------------------------------------------------------------------

// If the context already provides type information, such as a function argument
// or an already typed variable or constant, you can create an empty array with
// an empty array literal (`[]`).

a1.append(3)
assert(a1 == [3])
a1 = []
assert(a1 == [])

// An array's `isEmpty` method is a shortcut for checking whether the `count`
// property is equal to 0.

assert(a1.isEmpty == true)

// -----------------------------------------------------------------------------

// An array that initially has a specific value at all of its indexes can be
// created with the `init(repeating:count:)` initializer.

let a2 = Array(repeating: 0.0, count: 12)
let a3: [Double] = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
assert(a2 == a3)

// -----------------------------------------------------------------------------

// An array can be created by adding together two existing arrays with
// compatible types with the addition operator (`+`).

let a4 = [1.2, 3.4, 4.5]
let a5 = Array(repeating: 2.5, count: 3)
let a6 = a4 + a5
let a7: [Double] = [1.2, 3.4, 4.5, 2.5, 2.5, 2.5]
assert(a6 == a7)

// -----------------------------------------------------------------------------




// TODO




/*



// You access and modify an array through its methods and properties,
// or by using subscript syntax.

// Use the Boolean `isEmpty` property as a shortcut for checking whether the
// `count` property is equal to 0.

assert(shoppingList.isEmpty == false)

// You can add a new item to the end of an array by using `append(_:)`.

shoppingList.append("Flour")
// shoppingList now contains 3 items, and someone is making pancakes.

Alternatively, append an array of one or more compatible items with the addition
assignment operator (`+=`):

shoppingList += ["Baking Powder"]
// shoppingList now contains 4 items
shoppingList += ["Chocolate Spread", "Cheese", "Butter"]
// shoppingList now contains 7 items

Retrieve a value from the array by using subscript syntax, passing the index
of the value you want to retrieve within square brackets immediately after
the name of the array:

var firstItem = shoppingList[0]
// firstItem is equal to "Eggs"

NOTE

The first item in the array has an index of 0, not 1.  Arrays are zero-indexed.

You can use subscript syntax to change an existing value at a given index:

shoppingList[0] = "Six eggs"

You can also use subscript syntax to change a range of values at once, even if
the replacement set of values has a different length than the range you are
replacing. The following example replaces "Chocolate Spread", "Cheese", and
"Butter" with "Bananas" and "Apples":

shoppingList[4...6] = ["Bananas", "Apples"]
// shoppingList now contains 6 items

You can't use subscript syntax to add items onto to the end of an array.

To insert an item into the array at a specified index, call the array's
insert(_:at:) method:

shoppingList.insert("Maple Syrup", at: 0)
// shoppingList now contains 7 items
// "Maple Syrup" is now the first item in the list

Similarly, you remove an item from the array with the remove(at:) method.
This method removes the item at the specified index and returns the removed
item (although you can ignore the returned value if you do not need it):

let mapleSyrup = shoppingList.remove(at: 0)
// the item that was at index 0 has just been removed
// shoppingList now contains 6 items, and no Maple Syrup
// the mapleSyrup constant is now equal to the removed "Maple Syrup" string

// If you try to access or modify a value for an index that is outside of an
// array's existing bounds, you will trigger a runtime error.  You can check
// that an index is valid before using it by comparing it to the array's
// count property.  Except when count is 0 (meaning the array is empty),
// the largest valid index in an array will always be count - 1, because
// arrays are indexed from 0.

Any gaps in an array are closed when an item is removed, and so the value at
index 0 is once again equal to "Six eggs":

firstItem = shoppingList[0]
// firstItem is now equal to "Six eggs"

If you want to remove the final item from an array, use the removeLast() method
rather than the remove(at:) method to avoid the need to query the array’s count
property. Like the remove(at:) method, removeLast() returns the removed item:

let apples = shoppingList.removeLast()
// the last item in the array has just been removed
// shoppingList now contains 5 items, and no apples
// the apples constant is now equal to the removed "Apples" string

// -----------------------------------------------------------------------------

Iterating Over an Array

You can iterate over the entire set of values in an array with the for-in loop:

for item in shoppingList {
  print(item)
}
// Six eggs
// Milk
// Flour
// Baking Powder
// Bananas

If you need the integer index of each item as well as its value, use the
`enumerated()` method to iterate over the array instead.  For each item
in the array, `enumerated()` returns a tuple composed of an integer and
the item.  The integers start at zero and count up by one for each
item; if you enumerate over a whole array, these integers match
the items' indices.  You can decompose the tuple into temporary
constants or variables as part of the iteration.

for (index, value) in shoppingList.enumerated() {
  print("Item \(index + 1): \(value)")
}
// Item 1: Six eggs
// Item 2: Milk
// Item 3: Flour
// Item 4: Baking Powder
// Item 5: Bananas

*/


// =============================================================================
//  References:
//
//  The Swift Programming Language (Swift 3) - Collection Types
//  https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/CollectionTypes.html
// =============================================================================
