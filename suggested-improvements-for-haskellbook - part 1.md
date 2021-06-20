# Suggested Improvements for the Haskell Book v1.0RC4 - Part 1

_Note:_ Page numbers used here are the ones for the "haskell-programming-1.0RC4-screen" version of the book.

## Page xvii, Section "Acknowledgements", Paragraph 2

**Content in question:** "Their reviews helped ensure the book is suitable for both beginners and comprehensive."

**Critique:** This line doesn't appear to be grammatically correct or at the very least it's worded awkwardly.

**Suggested improvement:** "Their reviews helped ensure the book is both suitable for beginners and comprehensive."

## Page 7, Footnote 3

**Content in question:** "Note that this is the same as the identity function in mathematical notation: f(x) = x."

**Critique:** This line implies that the lambda calculus version of the identity function is not a type of mathematical notation. Lambda calculus is a part of mathematical logic which itself is part of mathematics.

**Suggested improvement:** "Note that this is the same as the identity function in common mathematical notation: f(x) = x."

## Page 12, Step 2

**Content in question:** "We've not reduced or applied anything here, but made the currying explicit."

**Critique:** This line glosses over the fact that the expression it refers to omits the parentheses that, like in all the previous examples of currying, explicitly show the nested levels of currying.

**Suggested improvement:** Add an intermediary step between step 1 and step 2 which includes the curried expression with explicit parentheses. Move the line "We've not reduced or applied anything here, but made the currying explicit." so it becomes the description for the new intermediary step. The original step 2's description should then be changed to mention that the curried expression's parentheses were not strictly necessary. Finally, update the numbering of the steps to account for the new intermediary step (i.e. The intermediary step becomes the new step 2, the original step 2 becomes the new step 3, etc.).

## Page 16, Step 3

**Content in question:** "Substituting (𝝺x.xx) for each occurence"

**Critique:** "occurence" is a mispelling of "occurrence".

**Suggested improvement:** "Substituting (𝝺x.xx) for each occurrence"

## Page 22, Step f

**Content in question:** "All of our terms are in normal order now."

**Critique:** Weren't they always in normal order but now they're just in beta normal form?

**Suggested improvement:** "All of our terms are in beta normal form now."

## Page 65, Section 2.13

**Content in question:** "How to desugar Haskell code; Gabriel Gonzalez"

**Critique:** The other follow-up online resources mentioned thus far in the book have had their links included.

**Suggested improvement:** Provide a link to the blog post which, at the time of writing this, is http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html

## Page 81

**Content in question:** "The infix operator, (!!),..."

**Critique:** The wording may confuse the reader into thinking `!!` is called the "infix operator". Its name is actually "list index (subscript) operator" (source: http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.List.html).

**Suggested improvement:** "The list index operator, (!!), which is also an infix operator,..."

## Page 86, Definition 3

**Content in question:** "The concatenation function in Haskell is (++)..."

**Critique:** The wording implies there's only one concatenation function in Haskell even though Haskell has both the append operator (`++`) and the `concat` function which both do concatenation.

**Suggested improvement:** "The append operator (++) is a concatenation function in Haskell..."

## Page 89, Paragraph 4

**Content in question:** "The whole thing is called a data declaration."

**Critique:** The synonymous term "datatype declaration" is also used later in the book (e.g. page 120, paragraph 2). Explicitly establishing that "data declaration" and "datatype declaration" mean the same thing will save the reader from wondering if there's actually some subtle difference between them that they're missing.

**Suggested improvement:** "The whole thing is called a data declaration (or can also be called a datatype declaration)."

## Page 106, Code Example "greetIfCool1.hs"

**Content in question:** `if cool` is followed by `then` on the next line.

**Critique:** `then` should be on the same line as `if cool`. Having `if` and its following `then` on the same line is a common pattern which would allow the `putStrLn` expressions to line up for better visual consistency.

**Suggested improvement:** Put `then` on the same line as `if cool` and leave `putStrLn "eyyyyy. What's shakin'?"` on its own line.

## Page 107, Code Example "greetIfCool2.hs"

**Content in question:** `if cool coolness` is followed by `then` on the next line.

**Critique:** `then` should be on the same line as `if cool coolness`. Having `if` and its following `then` on the same line is a common pattern which would allow the `putStrLn` expressions to line up for better visual consistency.

**Suggested improvement:** Put `then` on the same line as `if cool coolness` and leave `putStrLn "eyyyyy. What's shakin'?"` on its own line.

## Page 116, Definition 6

**Content in question:** The phrase "type constant".

**Critique:** "type constant" is not previously defined. The reader should not be expected to know what that is.

**Suggested improvement:** Define "type constant" some place earlier in the chapter.

## Page 141, Last Paragraph

**Content in question:** "That is, its parameters are polymorphic."

**Critique:** Aren't functions with polymorphic return types also polymorphic functions? Example: `read :: Read a => String -> a` (source: https://eli.thegreenplace.net/2018/return-type-polymorphism-in-haskell/)

**Suggested improvement:** "That is, its parameters and/or return value are polymorphic."

## Page 143, Paragraph 1

**Content in question:** "It will have to resolve into a concrete type at some point in order to evaluate."

**Critique:** Is this really true? In the prior example given, the type of `(-10) + 6.3` is `Fractional a => a`. It's not a concrete type like `Float` or `Double` but it evaluates to `-3.7` still having the type `Fractional a => a`.

**Suggested improvement:** Some sort of clarification should be given here.

## Pages 165-166, Section 6.3, Paragraph 3

**Content in question:** "Each of these instances is a type class that Bool implements, and the instances are the unique specifications of how Bool makes use of the methods from that type class."

**Critique:** The wording here suggests that instances of type classes are type classes themselves, which is not true.

**Suggested improvement:** "Each of these instances reference a type class that Bool implements, and the instances are the unique specifications of how Bool makes use of the methods from that type class."

## Page 166, List Item 5

**Content in question:** "Don't use it. No seriously, don't."

**Critique:** There's no immediate explanation as to why not and also no indication that it will be discussed later in section 6.11, leaving the reader to worry that such a strongly-worded warning will remain without justification.

**Suggested improvement:** "Don't use it. No seriously, don't. We'll find out why soon in section 6.11."

## Page 190, Section "Ord instances", Paragraphs 2-3

**Content in question:** The code example which defines the `DayOfWeek` datatype and its surrounding paragraphs.

**Critique:** There's potential for confusion here. This section only directly mentions the `Eq` instance from a previous section, and not the original `DayOfWeek` declaration associated with that instance. However, the words "you should still have the Eq instance we wrote for this datatype in scope" implies that that original `DayOfWeek` declaration would also be in scope and that it can be superceded with the new `DayOfWeek` declaration this section offers, but actually attempting that results in a multiple declarations error.

**Suggested improvement:** Replace the sentence right before the code example with "We'll modify our previously used DayOfWeek datatype to demonstrate". Update the code example to include `Eq` alongside `Ord` and `Show`. Reduce the paragraph following the code example so it reads as: "You can't have an Ord instance unless you also have an Eq instance, so the compiler will complain if you don't do one (not both) of those two things."

## Page 216, Section 7.2, Subsection "Setting parameters"

**Content in question:** "which is always at the left margin,"

**Critique:** There are scenarios where functions can be indented. For example, functions defined in a `where`.

**Suggested improvement:** "which is always closest to the left margin,"

## Page 256, Section 7.9, Paragraph 3

**Content in question:** "Pointfree is an extension of that idea but now we drop the argument altogether:"

**Critique:** Grammar. The word "but", when not used as an adverb, introduces a contrasting thing in relation to what was stated. Therefore, the sentence in question seems self-contradictory.

**Suggested improvement:** "Pointfree is an extension of that idea letting us drop the argument altogether:"

## Page 280, Section 8.3, Paragraph 2

**Content in question:** "Note that if you're using a Windows computer, this example may freeze your GHCi and not throw an exception."

**Critique:** This warning is too specific to Windows. GHCi 8.6.5 running on macOS 10.14.5 won't short-circuit the infinite loop computation when I tried it. Granted I didn't let it run for long.

**Suggested improvement:** "Beware this example may freeze your GHCi and not throw an exception should you attempt to try it."

## Page 280, Section 8.3, Paragraph 5

**Content in question:** "that didn't result in a bottom value."

**Critique:** On page 268, definition 5, "bottom" is defined as a non-value that indicates a value is unable to be returned. Therefore, "bottom value" is an oxymoron.

**Suggested improvement:** "that didn't result in being an occurrence of bottom."

## Page 302, Paragraph 3

**Content in question:** "Be aware that enumFromTo must have its first argument be lower than the second argument:"

**Critique:** The first argument can be equal to the second argument to prevent an empty list result. For example, `enumFromTo 3 3` evaluates to `[3]`. Also the use of the word "must" here is a bit disingenuous. I understand that the line "Otherwise you'll get an empty list." qualifies it but I believe it can be worded more precisely.

**Suggested improvement:** Replace the content in question with: "When using enumFromTo, you'll likely want to have its first argument less than or equal to the second argument:" and replace the REPL example with:

```haskell
Prelude> enumFromTo 1 3
[1,2,3]
Prelude> enumFromTo 3 3
[3]
Prelude> enumFromTo 3 1
[]
```

## Page 303, Paragraph 2

**Content in question:** "Replace the undefined, an value which results in an error when evaluated, with your own definition."

**Critique:** Grammar. "an value" should be "a value".

**Suggested improvement:** "Replace the undefined, a value which results in an error when evaluated, with your own definition."

## Page 312, Subsection "List comprehensions with Strings", Code Example 1

**Content in question:** "elem :: Eq a => a -> [a] -> Bool"

**Critique:** The footnote on this page references `Foldable` and how it's included in the type of `elem` even though the code example does not reflect this. In GHCi 8.6.5 at least, `:t elem` yields `elem :: (Foldable t, Eq a) => a -> t a -> Bool`.

**Suggested improvement:** "elem :: (Foldable t, Eq a) => a -> t a -> Bool".

## Page 318, Paragraph 3

**Content in question:** "This string concatenation is in neither WHNF nor NF, this is because the outermost component of the expression is a function,..."

**Critique:** Grammar. There appears to be a run-on sentence present.

**Suggested improvement:** "This string concatenation is in neither WHNF nor NF. This is because the outermost component of the expression is a function,..."

## Page 323, Section 9.9, Paragraph 2

**Content in question:** "map can only be used with []."

**Critique:** The usage of brackets to denote lists in general can be mistaken for denoting empty lists specifically.

**Suggested improvement:** "map can only be used with lists."

## Page 349, Code Example 2

**Content in question:** "--<-- This one finally matches"

**Critique:** As a matter of style, the comment syntax should have a space separating itself from the comment text. This should also fix the way it is highlighted in the book relative to how other comments are highlighted.

**Suggested improvement:** "-- <-- This one finally matches"

## Page 350, Code Example & REPL Example

**Content in question:** From the code example, the line `["(",x,"+",y,")"]) "0" xs`. From the REPL example, the line `"(1+(2+(3+(4+(5+0)))))"`.

**Critique:** Adding spaces would help with legibility both in the code as well as the output.

**Suggested improvement:** For the line in the code example, use instead `["(", x, " + ", y, ")"]) "0" xs`. For the line in the REPL example, use instead `"(1 + (2 + (3 + (4 + (5 + 0)))))"`.

## Page 355, Section 10.5, REPL Example

**Content in question:** The first line, `Prelude> let conc = concat`, the second line, `Prelude> let f x y = conc ["(",x,"+",y,")"]`, and the final line, `"(((((0+1)+2)+3)+4)+5)"`.

**Critique:** Creating a `conc` alias for the `concat` function is unnecessary. Also, adding spaces would help with legibility both in the code as well as the output.

**Suggested improvement:** Remove that first line which has `conc = concat`. Change the second line to `Prelude> let f x y = concat ["(", x, " + ", y, ")"]`, and change the final line to `"(((((0 + 1) + 2) + 3) + 4) + 5)"`.

## Page 379, Definition 3

**Content in question:** "A tail call is the final result of a function."

**Critique:** By that definition, the `3` in the function `\_ -> 3` is a tail call, which is incorrect. A tail call is a call to a function that happens as the last step of the function using that call.

**Suggested improvement:** "A tail call is a function application appearing at the end of a function."

## Page 381, Section 10.12

**Content in question:** "Antoni Diller. Introduction to Haskell."

**Critique:** The relevant link for this online resource should be included.

**Suggested improvement:** Provide a link to the course which, at the time of writing this, is https://www.cantab.net/users/antoni.diller/haskell/haskell.html

## Page 398, Section 11.8, Paragraph 5

**Content in question:** "In part this is because Int and related types Int8, Int16, and Int32 have clearly delineated upper and lower bounds,..."

**Critique:** `Int64` should be mentioned for completeness.

**Suggested improvement:** "In part this is because Int and related types Int8, Int16, Int32, and Int64 have clearly delineated upper and lower bounds,..."

## Page 401, Subsection "Unary constructors", Paragraph 2

**Content in question:** "Datatypes that only contain a unary constructor always have
the same cardinality as the type they contain."

**Critique:** A couple things. First, consistency. The preceding paragraph uses the term "unary data constructor". Second and more importantly to avoid ambiguity, unary data constructors should be mentioned explicitly because unary type constructors exist, too. For example, `Maybe`. The Haskell wiki gives another example, `Tree` (source: https://wiki.haskell.org/Constructor#Type_constructor). On a sidenote, unary type constructors appear to not be acknowledged as such anywhere in the book. I think they should be to help the reader to the remember the distinction.

**Suggested improvement:** "Datatypes that only contain a unary data constructor always have the same cardinality as the type they contain."

## Page 408

**Content in question:** The whole page.

**Critique:** In GHCi 8.6.5, `Numba (-128)` does not require the NegativeLiterals extension for it to work without getting a warning. However, `Numba -128` does require NegativeLiterals. So, the REPL warning that starts with "Literal 128 is out of the" is something I have not encountered.

**Suggested improvement:** Remove the discussion about NegativeLiterals since it's irrelevant because the code example is using `Numba (-128)` which already works.

## Page 419, REPL Example

**Content in question:** "First (Second (SheepInfo "Baaaaa" 5 5)) :: Sum (Sum a SheepInfo) b"

**Critique:** The result of ":t First (Second sheep)" that I got in GHCi 8.6.5 was "First (Second sheep) :: Sum (Sum a SheepInfo) b".

**Suggested improvement:** "First (Second sheep) :: Sum (Sum a SheepInfo) b"

## Page 435, Section 11.16, List Item 3

**Content in question:** "empty list value constructor".

**Critique:** Use the standard term "data constructor" to maintain consistency with other parts of the book that mention term-level constructors.

**Suggested improvement:** "empty list data constructor".

## Page 458, List Item 1

**Content in question:** "Our mkPerson type takes a Name and Age returns an Either result."

**Critique:** The wording is a little awkward.

**Suggested improvement:** "Our mkPerson type takes a Name and Age and returns an Either result."

## Page 480, Section 12.6, Definition 1

**Content in question:** "A higher-kinded type type is any type..."

**Critique:** The second occurrence of the word "type" is redundant.

**Suggested improvement:** "A higher-kinded type is any type..."
