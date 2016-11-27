Use the nil-coalescing operator instead of its equivalent ternary expression.
- reason: readability







In most cases, type parameters have descriptive names, such as Key and Value in
Dictionary<Key, Value> and Element in Array<Element>, which tells the reader
about the relationship between the type parameter and the generic type or
function it’s used in. However, when there isn’t a meaningful relationship
between them, it’s traditional to name them using single letters such as
T, U, and V, such as T in the swapTwoValues(_:_:) function above.

See also:
https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Generics.html#//apple_ref/doc/uid/TP40014097-CH26-ID183

Always give type parameters upper camel case names (such as T and MyTypeParameter) to indicate that they are a placeholder for a type, not a value.

See also:
https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Generics.html#//apple_ref/doc/uid/TP40014097-CH26-NoLink_194









Typically, you use the if statement to evaluate simple
conditions with only a few possible outcomes.
The switch statement is better
suited to more complex conditions with multiple possible permutations and is
useful in situations where pattern matching can help select an appropriate
code branch to execute.

See also:
- https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/ControlFlow.html#//apple_ref/doc/uid/TP40014097-CH9-ID127








// If a constant or variable needs to work with the absence of a value under
// certain conditions, always declare it as an optional value of the
// appropriate type.

// See also:
// - https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-NoLink_36













Leave out the type where possible when assigning enumeration values.
reason: readability









Use shorthand array notation instead of full array notation.

See also:
- https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/CollectionTypes.html#//apple_ref/doc/uid/TP40014097-CH8-ID108









It is good practice to create immutable collections in all cases where the
collection does not need to change.  Doing so makes it easier for you to
reason about your code and enables the compiler to optimize the
performance of the collections you create.

See also:
- https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/CollectionTypes.html#//apple_ref/doc/uid/TP40014097-CH8-NoLink_65










Give types UpperCamelCase names (such as SomeClass and
SomeStructure here) to match the capitalization of standard Swift types
(such as String, Int, and Bool). Conversely, always give properties and
methods lowerCamelCase names (such as frameRate and incrementCount) to differentiate them from type names.

See also:
- https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/ClassesAndStructures.html#//apple_ref/doc/uid/TP40014097-CH13-NoLink_107










If a property always takes the same initial value, provide a default value
rather than setting a value within an initializer. The end result is the same,
but the default value ties the property’s initialization more closely to its
declaration. It makes for shorter, clearer initializers and enables you to
infer the type of the property from its default value. The default value also
makes it easier for you to take advantage of default initializers and
initializer inheritance.

See also:
- https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Initialization.html#//apple_ref/doc/uid/TP40014097-CH18-NoLink_131








Name Boolean constants and variables in a way that helps keep code readable
and concise, while avoiding double negatives or confusing logic statements.



Avoid combining multiple instances of the ternary conditional operator into one compound statement.


Only use semicolons when they're required.


If a stored value in your code is not going to change, always declare
it as a constant.  Only use variables for storing values that need to
be able to change.

See also:
- https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-NoLink_27


Avoid using keywords as constant or variable names unless you have
absolutely no choice.

See also:
- https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-NoLink_29


Keep different block comments on separate lines.


Don't use block comments at the beginning of the same line as code.
Don't use block comments from inside single-line statements.


Use `UInt` only when you specifically need an unsigned integer type with the
same size as the platform's native word size.  If this is not the case,
`Int` is preferred, even when the values to be stored are known to be
non-negative.  A consistent use of `Int` for integer values aids code
interoperability, avoids the need to convert between different number
types, and matches integer type inference.

See also:
- https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-NoLink_31


// In situations where either `Double` or `Float` would be appropriate,
// `Double` is preferred.
//
// See also:
// - https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-NoLink_32


// Use `Int` for all general-purpose integer constants and variables even if
// they are known to be non-negative.  Using the default integer type in
// everyday situations means that integer constants and variables are
// immediately interoperable in your code and will match the inferred
// type for integer literal values.
//
// Use other integer types only when they are specifically needed for the task
// at hand because of explicitly-sized data from an external source or for
// performance, memory usage, or other necessary optimization.  Using
// explicitly-sized types in these situations helps to catch any
// accidental value overflows and implicitly documents the
// nature of the data being used.
//
// See also:
// - https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-ID324


// STYLE:
//
// Tuples are useful for temporary groups of related values.  They are not
// suited to the creation of complex data structures.  If your data
// structure is likely to persist beyond a temporary scope, model
// it as a class or structure, rather than as a tuple.
//
// See also:
// - https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-NoLink_34


// STYLE:
//
// Do not use an implicitly unwrapped optional when there is a possibility
// of a variable becoming `nil` at a later point.  Always use a normal
// optional type if you need to check for a `nil` value during the
// lifetime of a variable.
//
// See also:
// https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-NoLink_41


// STYLE:
//
// Assertions are not a substitute for designing your code in such a way that
// invalid conditions are unlikely to arise.  Nonetheless, in situations
// where invalid conditions are possible an assertion is an effective
// way to ensure that such conditions are highlighted and noticed
// during development before your app is published.
//
// See also:
// - https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-NoLink_43


// STYLE:
//
// Always assign a function call to an underscore when you don't
// want its return value.


// STYLE:
//
// Place parameters with default values at the end of a function's parameter
// list.  This ensures that all calls to the function use the same order for
// their nondefault arguments, and makes it clear that the same function is
// being called in each case.
//
// - https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Functions.html#//apple_ref/doc/uid/TP40014097-CH10-NoLink_92


// STYLE:
//
// Because protocols are types, begin their names with a capital letter
// (such as FullyNamed and RandomNumberGenerator) to match the names of
// other types in Swift (such as Int, String, and Double).
//
// - https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Protocols.html#//apple_ref/doc/uid/TP40014097-CH25-NoLink_183






Don't use single-element tuples.  They off no advantages over regular literals or variables.










// STYLE
//
// Although the unary plus operator doesn't actually do anything, you can use it
// to provide symmetry in your code for positive numbers when also using the
// unary minus operator for negative numbers.
//
// - https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/BasicOperators.html#//apple_ref/doc/uid/TP40014097-CH6-ID68
