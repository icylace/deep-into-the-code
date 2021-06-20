# Suggested Improvements for the Haskell Book v1.0RC2

_Note:_ Page numbers used here are not PDF page numbers but the book page numbers for the "haskell-programming-1.0RC2-screen" version of the book.

## Page xvii, Section "Acknowledgements", Paragraph 2

**Content in question:** "Their reviews helped ensure the book is suitable for both beginners and comprehensive."

**Critique:** This line doesn't appear to be grammatically correct.

**Suggested improvement:** Tweak the line to read "Their reviews helped ensure the book is both suitable for beginners and comprehensive."

## Page 7, Footnote 3

**Content in question:** "Note that this is the same as the identity function in mathematical notation: f(x) = x."

**Critique:** This line implies that the lambda calculus version of the identity function is not a type of mathematical notation. Lambda calculus is a part of mathematical logic which itself is part of mathematics.

**Suggested improvement:** Qualify the statement so it reads "Note that this is the same as the identity function in common mathematical notation: f(x) = x."

## Page 12, Step 2

**Content in question:** "We’ve not reduced or applied anything here, but made the currying explicit."

**Critique:** This line glosses over the fact that the expression it refers to omits the parentheses that, like in all the previous examples of currying, explicitly show the nested levels of currying.

**Suggested improvement:** Add an intermediary step between step 1 and step 2 which includes the curried expression with explicit parentheses. Move the line "We’ve not reduced or applied anything here, but made the currying explicit." so it becomes the description for the new intermediary step. Step 2's description should then be changed to mention that the curried expression's parentheses were not strictly necessary. Finally, update the numbering of the steps to account for the new intermediary step (i.e. The intermediary step becomes step 2, the original step 2 becomes step 3, etc.).

## Page 16, Step 3

**Content in question:** "Substituting (𝝺x.xx) for each occurence"

**Critique:** "occurence" is a mispelling of "occurrence".

**Suggested improvement:** Replace "occurence" with "occurrence".

## Page 22, Step f

**Content in question:** "All of our terms are in normal order now."

**Critique:** Weren't they always in normal order but now they're just in beta normal form?

**Suggested improvement:** Replace with "All of our terms are in beta normal form now."

## Page 64, Section 2.13

**Content in question:** The second listed follow-up resource is missing a link.

**Critique:** This puts it in contrast with the other follow-up resources mentioned up to this point in the book.

**Suggested improvement:** Provide a link to the blog post which, at the time of writing this, is http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html

## Page 80

**Content in question:** "The infix operator, (!!),..."

**Critique:** `!!` is not called the "infix operator". It is described in the GHC.List module as the "list index (subscript) operator".

**Suggested improvement:** Replace with "The list index (subscript) operator, (!!),..."

## Page 85, Definition 3

**Content in question:** "The concatenation function in Haskell is (++)..."

**Critique:** Both the append operator (`++`) and the `concat` function does concatenation in Haskell.

**Suggested improvement:** Replace with "The append operator (++) is a concatenation function in Haskell..."

## Page 104, Code Example "greetIfCool1.hs"

**Content in question:** `if cool` is followed by `then` on the next line.

**Critique:** `then` should be on the same line as `if cool`. Having `if` and its following `then` on the same line is a common pattern which would allow the `putStrLn` expressions to line up better.

**Suggested improvement:** Put `then` on the same line as `if cool` but leave `putStrLn "eyyyyy. What's shakin'?"` on its own line.

## Page 105, Code Example "greetIfCool2.hs"

**Content in question:** `if cool coolness` is followed by `then` on the next line.

**Critique:** `then` should be on the same line as `if cool coolness`. Having `if` and its following `then` on the same line is a common pattern which would allow the `putStrLn` expressions to line up better.

**Suggested improvement:** Put `then` on the same line as `if cool coolness` but leave `putStrLn "eyyyyy. What's shakin'?"` on its own line.

## Page 114, List Item 6

**Content in question:** The phrase "type constant".

**Critique:** "type constant" is not previously defined.

**Suggested improvement:** Define "type constant" some place earlier in the chapter.

## Page 118, Section 5.1, Paragraph 2

TODO:
investigation: the "datatype declaration" phrase is used, may should be mentioned whereever "data declaration" was defined ?

## Page 123, Paragraph 3

TODO:
investigation: does "typeclass-constrained polymorphic type variable" need its own definition ?

## Page 128, Paragraph 3

**Content in question:** "and then return a result that is a function and needs to be applied to a second argument."

**Critique:** Questionable grammar.

**Suggested improvement:** "and then return a result that is a function that needs to be applied to a second argument."

## Page 130, Paragraph 3

**Content in question:** "result is equal to 1 - 11 - 10."

**Critique:** Incorrect subtraction and awkward wording.

**Suggested improvement:** "result is equal to 1 - 11 which equals 10."

## Page 131, Paragraph 2

**Content in question:** "indexanonymous function ! syntax"

**Critique:** Confusing and unnecessary.

**Suggested improvement:** Remove that line entirely.

## Page 139, Last Paragraph

TODO: Find example of a legit function that has a polymorphic return type.
https://eli.thegreenplace.net/2018/return-type-polymorphism-in-haskell/

**Content in question:** "That is, its parameters are polymorphic."

**Critique:** What about a function that has a polymorphic return type?

**Suggested improvement:** "That is, its parameters and/or return value are polymorphic."

## Page 141, Paragraph 1

**Content in question:** "It will have to resolve into a concrete type at some point in order to evaluate."

**Critique:** Is this really true? In the prior example given, the type of `(-10) + 6.3` is `Fractional a => a`, at least according to GHCi 8.4.3. It's not a concrete type like `Float` or `Double` but it still evaluates.

**Suggested improvement:** Some sort of clarification should be given here.

## Pages 164-165, Section 6.3, Paragraph 3

**Content in question:** "Each of these instances is a typeclass that Bool implements, and the instances are the unique specifications of how Bool makes use of the methods from that typeclass."

**Critique:** The wording here suggests that instances of typeclasses are typeclasses themselves, which is not true.

**Suggested improvement:** "Each of these instances reference a typeclass that Bool implements, and the instances are the unique specifications of how Bool makes use of the methods from that typeclass."

## Page 165, List Item 5

**Content in question:** "Don't use it. No seriously, don't."

**Critique:** There's no immediate explanation as to why not and also no indication that it will be discussed later in section 6.11, leaving the reader to worry that such a strongly-worded warning will remain without justification.

**Suggested improvement:** "Don't use it. No seriously, don't. We'll find out why later."

## Pages 189-90, Section "Ord instances", Paragraph 2-3

**Content in question:** The code example which defines the `DayOfWeek` datatype.

**Critique:** There's potential for confusion here. This section only directly mentions the `Eq` instance from a previous section, and not the original `DayOfWeek` declaration associated with that instance. However, the words "you should still have the Eq instance we wrote for this datatype in scope" implies that that original `DayOfWeek` declaration would also be in scope and that it can be replaced with a new `DayOfWeek` declaration this section offers, but actually doing that results in a multiple declarations error.

**Suggested improvement:** I believe the intent was that the `DayOfWeek` declaration in this section is actually just a modified version of the `DayOfWeek` declaration and not a "new" `DayOfWeek` declaration which was what I initially thought. To make this more clear, replace the sentence right before the code example with "We'll modify our previously used DayOfWeek datatype to demonstrate".

## Page 196, Second to Last Paragraph

**Content in question:** "indexmain@main"

**Critique:** What exactly is this? Assuming this is directly related to the `main` function mentioned directly before it, why is this shown? It appears pointless.

**Suggested improvement:** Remove it.

## Page 197, Paragraph Before Section "Working with Show"

**Content in question:** "indexmain@main"

**Critique:** What exactly is this? It appears pointless.

**Suggested improvement:** Remove it.

## Page 216, Section 7.2, Subsection "Setting parameters"

**Content in question:** "which is always at the left margin,"

**Critique:** Indentation is not taken into account.

**Suggested improvement:** "which is always closest to the left margin,"

## Page 257, Section 7.9, Paragraph 3

**Content in question:** "Pointfree is an extension of that idea but now we drop the argument altogether:"

**Critique:** Grammar. The word "but", when not used as an adverb, introduces a contrasting thing in relation to what was stated. Therefore, the sentence in question seems self-contradictory.

**Suggested improvement:** "Pointfree is an extension of that idea letting us drop the argument altogether:"

## Page 283, Section 8.3, Paragraph 2

**Content in question:** "Note that if you're using a Windows computer, this example may freeze your GHCi and not throw an exception."

**Critique:** The warning is too specific to Windows. I currently use GHCi 8.4.3 running on macOS 10.12.16 on my 2014 MacBook Pro and the GHCi code example didn't short-circuit the infinite loop computation when I ran it. Granted I didn't let it run for long.

**Suggested improvement:** "Beware this example may freeze your GHCi and not throw an exception should you attempt to try it."

## Page 283, Section 8.3, Paragraph 5

**Content in question:** "that didn't result in a bottom value."

**Critique:** On page 270 "bottom" is defined as a non-value that denotes code that cannot return a value. On page 283, the same page as the content in question, "bottom" is defined as a term used to refer to computations that do not result in a value. Therefore, "bottom value" is an oxymoron.

**Suggested improvement:** "that didn't result in being an example of bottom."

## Page 306, Last Paragraph

**Content in question:** "Be aware that enumFromTo must have its first argument be lower than the second argument:"

**Critique:** The first argument can be at least equal to the second argument to prevent an empty list result. For instance, `enumFromTo 3 3` evaluates to `[3]`.

**Suggested improvement:** "Be aware that enumFromTo must have its first argument not be lower than the second argument:"

## Page 307, Last Paragraph

**Content in question:** "Replace the undefined, an value which results in an error when evaluated, with your own definition."

**Critique:** Grammar. "an value" should be " a value".

**Suggested improvement:** "Replace the undefined, a value which results in an error when evaluated, with your own definition."

## Page 310, Second to Last Paragraph

**Content in question:** "In the final example above, why does it only return a single a?"

**Critique:** The line should be referring to the example that comes after it because the example that came before it only dealt with numbers.

**Suggested improvement:** "In the final example below, why does it only return a single a?"

## Page 311, Exercise 1

**Content in question:** "wallfish" in the code example.

**Critique:** Where does "wallfish" come from? Shouldn't that be "sheryl"?

**Suggested improvement:** Replace "wallfish" with "sheryl".

## Page 316, Subsection "List comprehensions with Strings", Code Sample 1

**Content in question:** "elem :: Eq a => a -> [a] -> Bool"

**Critique:** The footnote on this page references `Foldable` and how it's included in the type of `elem` even though the code sample does not reflect this. In GHCi 8.4.3 at least, `:t elem` yields `elem :: (Foldable t, Eq a) => a -> t a -> Bool`.

**Suggested improvement:** "elem :: (Foldable t, Eq a) => a -> t a -> Bool".

## Page 322, Paragraph 4

**Content in question:** "This string concatenation is in neither WHNF nor NF, this is because the outermost component of the expression is a function,..."

**Critique:** Grammar. There appears to be a run-on sentence present.

**Suggested improvement:** "This string concatenation is in neither WHNF nor NF. This is because the outermost component of the expression is a function,..."

## Page 327, Section 9.9, Paragraph 2

**Content in question:** "Now we hit our base case and and hit our base case:"

**Critique:** Redundancy can be reduced.

**Suggested improvement:** "Now we hit our base case:"

## Page 354, Paragraph 3

**Content in question:** "map can only be used with []."

**Critique:** The usage of brackets to denote lists in general can be confused with their regular use as denoting only empty lists.

**Suggested improvement:** "map can only be used with lists."

## Page 354, Code Sample 2

**Content in question:** "--<-- This one finally matches"

**Critique:** As a matter of style, the comment syntax should have a space separating itself from the comment text. This should also fix the way it is highlighted in relation to how other comments are highlighted.

**Suggested improvement:** "-- <-- This one finally matches"

## Page 355, Code Sample 2 & 3

**Content in question:** From code sample 2, the line `["(",x,"+",y,")"]) "0" xs`. From code sample 3, the line `"(1+(2+(3+(4+(5+0)))))"`.

**Critique:** Adding spaces would help with legibility both in the code as well as the output.

**Suggested improvement:** For the line in code sample 2, use instead `["(", x, " + ", y, ")"]) "0" xs`. For the line in code sample 3, use instead `"(1 + (2 + (3 + (4 + (5 + 0)))))"`.

## Page 360, Section 10.5, Code Sample 2

**Content in question:** The first line, `Prelude> let conc = concat`, the second line, `Prelude> let f x y = conc ["(",x,"+",y,")"]`, and the final line, `"(((((0+1)+2)+3)+4)+5)"`.

**Critique:** Creating a `conc` alias for the `concat` function is unnecessary. Also, adding spaces would help with legibility both in the code as well as the output.

**Suggested improvement:** Remove first line, change the second line to `Prelude> let f x y = concat ["(", x, " + ", y, ")"]`, and change the final line to `"(((((0 + 1) + 2) + 3) + 4) + 5)"`.

## Page 385, Definition 3

**Content in question:** "A tail call is the final result of a function."

**Critique:** A tail call is a call to a function that happens as the last step of another function. It's possible to have a function that does not call another function (ex. `(\_ -> 3)`).

**Suggested improvement:** "A tail call is a function application appearing at the end of a function."

## Page 399, Paragraph 3

**Content in question:** The blank line directly after the paragraph.

**Critique:** Formatting.  No other paragraph that is followed by another paragraph has a blank line separating them.

**Suggested improvement:** Remove the blank line.

## Page 404, Paragraph 1

**Content in question:** "In part this is because Int and related types Int8, Int16, and Int32 have clearly delineated upper and lower bounds, defined by the amount of memory they are permitted to use."

**Critique:** Int64 should be mentioned for completeness.

**Suggested improvement:** "In part this is because Int and related types Int8, Int16, Int32, and Int64 have clearly delineated upper and lower bounds, defined by the amount of memory they are permitted to use."

## Page 406, Subsection "Unary constructors", Paragraph 2

**Content in question:** "Datatypes that only contain a unary constructor always have
the same cardinality as the type they contain."

**Critique:** For sake of clarity, data constructors should be mentioned explicitly.

**Suggested improvement:** "Datatypes that only contain a unary data constructor always have
the same cardinality as the type they contain."

## Page 413-414

**Content in question:** The majority of page 413 and the beginning of page 414 up until section 11.11.

**Critique:** When using GHC 8.6.1 or GHCi 8.6.1, `Numba (-128)` does not require NegativeLiterals for it to work without getting a warning.

**Suggested improvement:** Remove the discussion concerning getting `Numba (-128)` to work because it's unnecessary.

## Page 425, Code Example 1

**Content in question:** "First (Second (SheepInfo "Baaaaa" 5 5)) :: Sum (Sum a SheepInfo) b"

**Critique:** The result of ":t First (Second sheep)" that I got in GHCi 8.6.1 was "First (Second sheep) :: Sum (Sum a SheepInfo) b".

**Suggested improvement:** "First (Second sheep) :: Sum (Sum a SheepInfo) b"

## Page 444, List Item 3

**Content in question:** "empty list value constructor".

**Critique:** Use the standard term "data constructor" to maintain consistency with other parts of the book that mention term-level constructors.

**Suggested improvement:** "empty list data constructor".

## Page 467, List Item 1

**Content in question:** "Our mkPerson type takes a Name and Age returns an Either result."

**Critique:** The wording is a little awkward.

**Suggested improvement:** "Our mkPerson type takes a Name and Age and returns an Either result."

## Page 491, Section 12.6, Definition 1

**Content in question:** "A higher-kinded type type is any type"

**Critique:** The second occurrence of the word "type" is redundant.

**Suggested improvement:** "A higher-kinded type is any type"

## Page 496, Paragraph 2

**Content in question:** "We’ll demonstrate this later. indexmain@main"

**Critique:** "indexmain@main" makes no sense and should be removed.

**Suggested improvement:** "We’ll demonstrate this later."

## Page 499, Code Sample 1

**Content in question:** "$ stack exec hello"

**Critique:** In the last code sample on page 496 a similar but slightly different command was used which was "$ stack exec -- hello". This inconsistency should be addressed.

**Suggested improvement:** Either be consistent about which command to use, or explain the distinction between both commands by the time the second command is used.

## Page 501, Subsection "Exposing modules"

**Content in question:** "indexmain@main"

**Critique:** What exactly is this? It appears pointless.

**Suggested improvement:** Remove it.

## Page 508, After List Item 6

**Content in question:** "indexmain@main"

**Critique:** What exactly is this? It appears pointless.

**Suggested improvement:** Remove it.

## Every Page of the "haskell-programming-1.0RC2-screen" Version of the Book

**Content in question:** Non-code text.

**Critique:** I'm able to copy-and-paste from the code examples in the "haskell-programming-1.0RC2-screen" version of the book to my text editor but I can't do that with its regular paragraph text. When I attempt to do using Adobe Reader I just get empty boxes where I expect readable characters to be. When I try it with macOS Preview I instead get spaces. However, this problem doesn't exist in the "haskell-programming-1.0RC2-ereader" version of the book where every type of text content can be copy-and-pasted as expected.

**Suggested improvement:** Generate the "haskell-programming-1.0RC2-screen" version of the book in such a way as to allow consistent copy-and-pasting.

## Every Page of both the "haskell-programming-1.0RC2-screen" and "haskell-programming-1.0RC2-ereader" Versions of the Book

**Content in question:** Code text.

**Critique:** When I copy-and-paste code examples into my text editor to help get me started on exercises, the pasted code does not include the indentation as shown in the book. Restoring the indentation is inconvenient and interrupts the flow of working exercises.

**Suggested improvement:** If possible, generate all version of the book in such a way as to have indentation preserved when a reader does a copy-and-paste from the book into their editor of choice.
