# Suggested Improvements for "Haskell Programming from First Principles" v1.0

_Note:_ Page numbers used here are not PDF page numbers but the book page numbers for the "haskellbook_haskell-programming-1.0-screen.pdf" version of the book.

## All Code Samples

**Content in question:** Code text.

**Critique:** When I copy-and-paste code examples into my text editor to help get me started on exercises, the pasted code does not include the indentation as shown in the book. Restoring the indentation is inconvenient and interrupts the flow of working exercises.

**Suggested improvement:** If possible, generate all version of the book in such a way as to have indentation preserved when a reader does a copy-and-paste from the book into their editor of choice.

## Page ix, Table of Contents Section for Chapter 20

**Content in question:** "20Foldable"

**Critique:** A space is missing.

**Suggested improvement:** "20 Foldable"

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

## Page 65, Section 2.13 "Follow-up resources", Item 2

**Content in question:** The listed follow-up resource is missing a link.

**Critique:** This puts it in contrast with the other follow-up online resources mentioned thus far in the book in which they have had their links included.

**Suggested improvement:** Provide a link to the blog post which, at the time of writing this, is: https://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html

## Page 81

**Content in question:** "The infix operator, `!!`,"

**Critique:** `!!` is not called the "infix operator". It is actually described in the GHC.List module as the "List index (subscript) operator". Source: https://hackage.haskell.org/package/base-4.15.0.0/docs/src/GHC-List.html#%21%21

**Suggested improvement:** Replace with "The list index (subscript) operator, `!!`,"

## Page 86, Item 3

**Content in question:** "The _concatenation_ function in Haskell is `++`,"

**Critique:** Both the append operator (`++`) and the `concat` function does concatenation in Haskell.

**Suggested improvement:** Replace with "The append operator `++` is a concatenation function in Haskell,"

## Page 89, Paragraph 4

**Content in question:** "The whole thing is called a data declaration."

**Critique:** The synonymous term "datatype declaration" is also used later in the book (e.g. Page 107, Section 4.7 "Tuples", Paragraph 2 as well as Page 120, Section 5.1 "Types", Paragraph 2). Explicitly establishing that "data declaration" and "datatype declaration" mean the same thing will save the reader from wondering if there's actually some subtle difference between them that they might be missing.

**Suggested improvement:** "The whole thing is called a data declaration (a.k.a. datatype declaration)."

## Page 106, Code Example "greetIfCool1.hs"

**Content in question:** `if cool` is followed by `then` on the next line.

**Critique:** `then` should be on the same line as `if cool`. Having `if` and its following `then` on the same line is a common pattern which would allow the `putStrLn` expressions to align consistently.

**Suggested improvement:** Put `then` on the same line as `if cool` but leave `putStrLn "eyyyyy. What's shakin'?"` on its own line.

## Page 106, Code Example "greetIfCool2.hs"

**Content in question:** `if cool coolness` is followed by `then` on the next line.

**Critique:** `then` should be on the same line as `if cool coolness`. Having `if` and its following `then` on the same line is a common pattern which would allow the `putStrLn` expressions to align consistently.

**Suggested improvement:** Put `then` on the same line as `if cool coolness` but leave `putStrLn "eyyyyy. What's shakin'?"` on its own line.

## Page 115, Item 6

**Content in question:** The phrase "type constant".

**Critique:** "type constant" is not previously defined.

**Suggested improvement:** Define "type constant" some place earlier in the chapter.

## Page 120, Section 5.1 "Types", Paragraph 2

TODO:
investigation: the "datatype declaration" phrase is used, may should be mentioned whereever "data declaration" was defined ?

## Page 125, Section "Type class-constrained type variables", Paragraph 3

TODO:
investigation: does "type class-constrained polymorphic type variable" need its own definition ?

## Page 130, Paragraph 3

**Content in question:** "and then return a result that is also a function and needs to be applied to a second argument."

**Critique:** Questionable grammar.

**Suggested improvement:** "and then return a result that is also a function that needs to be applied to a second argument."

## Page 141, Last Paragraph on the Page

**Content in question:** "That is, its parameters are polymorphic."

**Critique:** Aren't functions with polymorphic return types also polymorphic functions? Example: `read :: Read a => String -> a`. Source: https://eli.thegreenplace.net/2018/return-type-polymorphism-in-haskell/ in the "Return-type polymophism" section.

**Suggested improvement:** "That is, its parameters and/or return value are polymorphic."

## Pages 166-167, Section 6.3 "Back to Bool", Paragraph 3

**Content in question:** "Each one is a type class that `Bool` implements, and the instances are the unique specifications of how `Bool` makes use of the methods from that type class."

**Critique:** The wording here suggests that instances of type classes are type classes themselves, which is not true.

**Suggested improvement:** "Each one is an implementation `Bool` has for a type class, and the instances are the unique specifications of how `Bool` makes use of the methods from that type class."

## Page 167, Item 5

**Content in question:** "Don't use it. No seriously, don't."

**Critique:** There's no immediate explanation as to why not and also no indication that it will be discussed later in section 6.11, leaving the reader to worry that such a strongly-worded warning will go without justification.

**Suggested improvement:** "Don't use it. No seriously, don't. We'll find out why soon in section 6.11."

## Page 219, Section 7.2 "Arguments and parameters", Subsection "Setting parameters"

**Content in question:** "which is always at the left margin,"

**Critique:** Indentation is not taken into account. There are scenarios where functions can be indented. For example, functions defined inside a `where` clause.

**Suggested improvement:** "which is always closest to the left margin,"

## Page 259, Section 7.9 "Point-free style", Paragraph 3

**Content in question:** "Point-free is an extension of that idea, but now we drop the argument altogether"

**Critique:** Grammar. The word "but", when not used as an adverb, introduces a contrasting thing in relation to what was stated. Therefore, the sentence in question seems self-contradictory.

**Suggested improvement:** "Point-free is an extension of that idea letting us drop the argument altogether:"

## Page 283, Paragraph 1

TODO:

**Content in question:** "Note that if you're using a Windows computer, this example may freeze your GHCi instead of throwing an exception."

**Critique:** This warning is too specific to Windows. I had used GHCi 8.4.3 running on macOS 10.12.16 on a 2014 MacBook Pro and the GHCi code example didn't short-circuit the infinite loop computation when I ran it. GHCi 8.6.5 running on macOS 10.14.5 also won't short-circuit. Granted in both instances I didn't let it run for long.

**Suggested improvement:** "Beware this example may freeze your GHCi and not throw an exception should you attempt to try it."

## Page 283, Paragraph 4

**Content in question:** "that doesn't result in a bottom value."

**Critique:** On page 271, item 5, "bottom" is defined as a non-value that denotes code that cannot return a value. On page 283, the same page as the content in question, "bottom" is defined as a term used to refer to computations that do not result in a value. Therefore, "bottom value" is an oxymoron.

**Suggested improvement:** "that doesn't result in being an example of bottom."

## Page 315, Subsection "List comprehensions with strings", Code Sample 1

**Content in question:** "`elem :: Eq a => a -> [a] -> Bool`"

**Critique:** The footnote on this page references `Foldable` and how it's included in the type of `elem` even though the code sample does not reflect this. In GHCi 8.4.3 at least, `:t elem` yields `elem :: (Foldable t, Eq a) => a -> t a -> Bool`.

**Suggested improvement:** "`elem :: (Foldable t, Eq a) => a -> t a -> Bool`".

## Page 326, Section 9.9 "Transforming lists of values", Paragraph 2

**Content in question:** "`map` can only be used with `[]`."

**Critique:** The usage of brackets to denote lists in general can be confused with their regular use as denoting only empty lists.

**Suggested improvement:** "`map` can only be used with lists."

## Page 354, Code Sample 1 & 2

**Content in question:** From code sample 2, the line `["(",x,"+",y,")"]) "0" xs`. From code sample 3, the line `"(1+(2+(3+(4+(5+0)))))"`.

**Critique:** Adding spaces would help with legibility both in the code as well as the output.

**Suggested improvement:** For the line in code sample 2, use instead `["(", x, " + ", y, ")"]) "0" xs`. For the line in code sample 3, use instead `"(1 + (2 + (3 + (4 + (5 + 0)))))"`.

## Page 359, Section 10.5 "Fold left", Code Sample 2

**Content in question:** The first line, `Prelude> conc = concat`, the second line, `Prelude> f x y = conc ["(",x,"+",y,")"]`, and the final line, `"(((((0+1)+2)+3)+4)+5)"`.

**Critique:** Creating a `conc` alias for the `concat` function is unnecessary. Also, adding spaces would help with legibility both in the code as well as the output.

**Suggested improvement:** Remove first line, change the second line to `Prelude> f x y = concat ["(", x, " + ", y, ")"]`, and change the final line to `"(((((0 + 1) + 2) + 3) + 4) + 5)"`.

## Page 383, Section 10.11 "Definitions", Item 3

**Content in question:** "A _tail call_ is the final result of a function."

**Critique:** By that definition, the `3` in the function `\_ -> 3` is a tail call, which is incorrect. A tail call is a call to a function that happens as the last step of the function using that call.

**Suggested improvement:** "A _tail call_ is a function application appearing at the end of a function."

## Page 384, Section 10.12 "Follow-up resources", Item 1

**Content in question:** "Antoni Diller. _Introduction to Haskell._"

**Critique:** The link for this online resource should be updated.

**Suggested improvement:** Use the HTTPS version of the link which at the time of writing this is: https://www.cantab.net/users/antoni.diller/haskell/haskell.html

## Page 401, Section 11.8 "What makes these datatypes algebraic?", Paragraph 5

**Content in question:** "In part, this is because `Int` and related types `Int8`, `Int16`, and `Int32` have clearly delineated upper and lower bounds,"

**Critique:** `Int64` should be mentioned for sake of completeness.

**Suggested improvement:** "In part, this is because `Int` and related types `Int8`, `Int16`, `Int32`, and `Int64` have clearly delineated upper and lower bounds,"

## Page 404, Subsection "Unary constructors", Paragraph 2

**Content in question:** "Datatypes that only contain a unary constructor always have the same cardinality as the type they contain."

**Critique:** For sake of clarity, data constructors should be mentioned explicitly.

**Suggested improvement:** "Datatypes that only contain a unary data constructor always have the same cardinality as the type they contain."

## Page 410

// TODO:

**Content in question:** The majority of page 410 and up until section 11.11 "Product types".

**Critique:** When using GHC 8.10.4, `Numba (-128)` technically does not require the NegativeLiterals language extension for it to work without getting a warning. However, doing so will result in `Numba 127`. With NegativeLiterals active the result is `Numba (-128)`.

**Suggested improvement:** Update the discussion to reflect the warning level.

## Page 421, Code Example 1

**Content in question:** "`First (Second (SheepInfo "Baaaaa" 5 5)) :: Sum (Sum a SheepInfo) b`"

**Critique:** The result of ":t First (Second sheep)" in GHCi 8.6.1 or later is "First (Second sheep) :: Sum (Sum a SheepInfo) b".

**Suggested improvement:** "First (Second sheep) :: Sum (Sum a SheepInfo) b"

## Page 438, Item 3

**Content in question:** "empty list value constructor".

**Critique:** Use the standard term "data constructor" to maintain consistency with other parts of the book that mention term-level constructors.

**Suggested improvement:** "empty list data constructor".

## Page 491, Command Line Example 1

**Content in question:** "`$ stack exec hello`"

**Critique:** In the last code sample on page 488 a similar but slightly different command was used which was "`$ stack exec -- hello`". This inconsistency should be addressed.

**Suggested improvement:** Either be consistent about which command to use, or explain the distinction between both commands by the time the second command is used.

## Page 498, Paragraph 1

**Content in question:** "You can set your `Prelude` prompt permanently, if you wish, by changing it in your GHCi configuration file, but instructions for doing that are outside of the scope of the current chapter."

**Critique:** Motivated readers should be pointed to where they can learn to edit their GHCi configuration.

**Suggested improvement:** Add a footnote to the page, referred to from within the same paragraph as the content in question, which links to the part of GHCi's documentation that talks about managing its configuration: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-files

## Page 509, Paragraph 1

**Content in question:** "All modules listed below are part of the main `base` library that comes with your GHC install, unless otherwise noted."

**Critique:** It seems the paragraph is referring to the code example directly preceding it. So, "below" should really say "above".

**Suggested improvement:** "All modules listed above are part of the main `base` library that comes with your GHC install, unless otherwise noted."

## Page 514, Code Example 2

**Content in question:** "`l < maxWordLength`"

**Critique:** The naming of the variable `maxWordLength` implies that I can choose to set `minWordLength` and `maxWordLength` to the same number in order to select words that are of a fixed length. For example, if I kept `minWordLength` as 5 but set `maxWordLength` also as 5 then I should get just a list of words that are 5 characters in length. The code in question prevents that intuition from being realized.

**Suggested improvement:** `l <= maxWordLength`

## Page 514, Paragraph 3

**Content in question:** "We have to subtract one from the length of the word list in order to index it, because `length` starts counting from 1, but an index of the list starts from 0."

**Critique:** `length` actually starts counting from 0 because `length []` gives back 0.

**Suggested improvement:** "We have to subtract one from the length of the word list in order to index it, because that is the index position of the last item in the list."

## Page 520, Item 7

**Content in question:** "`guessChar` is the character that the player has guessed."

**Critique:** Item 4 has a more accurate description of what `guessChar` is and that should be reflected here also.

**Suggested improvement:** "`guessChar` is either the character the player has guessed correctly or a `Nothing` representing a character that has yet to be guessed correctly."

## Page 566, Subsection "Using QuickCheck", Item 5, Code Example

**Content in question:** "`-- quot rem`"

**Critique:** This comment adds no useful information and shows what the code already expresses.

**Suggested improvement:** Remove the comment in question.

## Page 584, Code Example

**Content in question:** "`mconcat = foldr mappend mempty`"

**Critique:** `mconcat` is listed with the monoidal laws as if it's one of them
which it's not. It's just a function with a convenient default implementation.

**Suggested improvement:** Remove it. Let the mention of it at the beginning of
page 585 speak for itself.

## Page 596, Section 15.11 "Madness", Paragraph 1

**Content in question:** "You may have seen mad libs before. The idea is to take a template of phrases, fill them in with blindly selected categories of words, and see if saying the final version is amusing."

**Critique:** Mad Libs should be capitalized because it is a proper noun. Also, the word categories are not selected blindly but the words themselves are.

**Suggested improvement:** "You may have seen Mad Libs before. The idea is to take a template of phrases, fill them in with blindly selected words from predetermined categories, and see if saying the final version is amusing."

## Page 605, Subsection "NonEmpty, a useful datatype", Paragraph 4

**Content in question:** "Since that data constructor is symbolic rather than alphanumeric, it can't be used as a prefix:"

**Critique:** Prefix notation is possible with parentheses.

**Suggested improvement:** "Since that data constructor is symbolic rather than alphanumeric, it can be used as a prefix only by encasing it within parentheses:

data R' = (:!!:) Int String`"

Also, remove "Using it as a prefix will cause a syntax error:

`parse error on input ':!!:'`"

## Page 606, Paragraph 1

**Content in question:** "On the other hand, an alphanumeric data constructor can't be used as an infix:

```
data S = Int Prefix String
```
"

**Critique:** Infix notation is possible with backticks.

**Suggested improvement:** "On the other hand, an alphanumeric data constructor can be used as an infix only by encasing it within backticks:

```
data S = Int `Prefix` String
```
"

Also, remove "It will cause another error:

```
Not in scope: type constructor
  or class ‘Prefix’
A data constructor of that name
  is in scope;
did you mean DataKinds?
```
"

## Page 663, Code Example

**Content in question:** `return (input ++ "and me too!")`

**Critique:** Given the context of the surrounding sentences, it seems the intent is to be consistent with the preceding REPL example's `fmap (++ " and me too!") getLine`.

**Suggested improvement:** `return (input ++ " and me too!")`

## Page 673, Section 16.19 "Follow-up resources", Item 3

**Content in question:** "Gabriel Gonzalez. _The functor design pattern._"

**Critique:** Online resources should have their links included.

**Suggested improvement:** Provide a link to the blog post which, at the time of writing this, is: https://www.haskellforall.com/2012/09/the-functor-design-pattern.html

## Page 680, Code Example 1

**Content in question:** `mappend :: f f f` and `$ :: (a -> b) a b`

**Critique:** Despite having what I assume is an intentional similarity to the table of types given towards the end of page 679, the code in question should still be written with syntactical correctness in mind.

**Suggested improvement:** Use `mappend :: f -> f -> f` and `$ :: (a -> b) -> a -> b` respectively.

## Page 689, Paragraph 1

**Content in question:** "Then, we use the tie-fighter to apply that to the second value,"

**Critique:** "tie-fighter" should be spelled "TIE fighter" because TIE is an acronym for "twin ion engine". Source: https://starwars.fandom.com/wiki/TIE_Series#Technology

**Suggested improvement:** "Then, we use the TIE fighter to apply that to the second value,"

## Page 706, Code Example 1

**Content in question:** `-- and we hit a situation where want to map`

**Critique:** Grammar.

**Suggested improvement:** `-- and we hit a situation where we want to map`

## Page 774, Section 18.9 "Follow-up resources", Item 3

**Content in question:** "Gabriel Gonzalez. _How to desugar Haskell code._"

**Critique:** Online resources should have their links included.

**Suggested improvement:** Provide a link to the blog post which, at the time of writing this, is: https://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html

## Page 746, Subsection "Using the Maybe Monad"

**Content in question:** "You developed some intutions above for `do` syntax and the list `Monad`;"

**Critique:** Spelling. "intutions" should be "intuitions".

**Suggested improvement:** "You developed some intuitions above for `do` syntax and the list `Monad`;"

## Page 756, Code Example 1

**Content in question:** "`return :: Monad m => a -> m aq`"

**Critique:** The `q` seems to be a typo.

**Suggested improvement:** "`return :: Monad m => a -> m a`"

## Page 795, Paragraph 1

**Content in question:** "Where we import the module name followed by parentheses, such as with `replicateM` or `liftIO`, we are saying we only want to import the functions or values of those names and nothing else."

**Critique:** The first part of the sentence is referring to module names but `replicateM` and `liftIO` are not module names.

**Suggested improvement:** "Where we import the module name followed by parentheses, such as with `Control.Monad` or `Control.Monad.IO.Class`, we are saying we only want to import the functions or values listed within the parentheses."

## Page 800, Code Example

**Content in question:** "`[5]`"

**Critique:** The blank line immediately above the "`[5]`" suggests that the "`[5]`" is associated with the line below it even though it's actually associated with the line above it.

**Suggested improvement:** Remove the aforementioned blank line immediately above the "`[5]`".

## Page 801, Code Example

**Content in question:** "`[8]`"

**Critique:** The blank line immediately above the "`[8]`" suggests that the "`[8]`" is associated with the line below it even though it's actually associated with the line above it.

**Suggested improvement:** Remove the aforementioned blank line immediately above the "`[8]`".

## Page 820, Section 20.7 "Follow-up resources", Item 1

**Content in question:** The only listed resource.

**Critique:** Online resources should have their links included.

**Suggested improvement:** Provide a link to the blog post which, at the time of writing this, is: https://blog.jakuba.net/2014-07-30-foldable-and-traversable/

## Page 842, Section 21.13 "Follow-up resources"

**Content in question:** "Jakub Arnold. _Foldable and Traversable._"

**Critique:** Online resources should have their links included.

**Suggested improvement:** Provide a link to the blog post which, at the time of writing this, is: https://blog.jakuba.net/2014-07-30-foldable-and-traversable/

## Page 878, REPL Example

**Content in question:** "`Prelude| rx = random (snd sg3)`"

**Critique:** `sg3` is not mentioned anywhere. Perhaps `sg` was intended instead.

**Suggested improvement:** "`Prelude| rx = random (snd sg)`"









// -----------------------------------------------------------------------------

// TODO:

## Page 192, Section "Ord instances", Paragraphs 2-3

**Content in question:** The code example which defines the `DayOfWeek` datatype and its surrounding paragraphs.

**Critique:** There's potential for confusion here. This section only directly mentions the `Eq` instance from a previous section, and not the original `DayOfWeek` declaration associated with that instance. However, the words "you should still have the Eq instance we wrote for this datatype in scope" implies that that original `DayOfWeek` declaration would also be in scope and that it can be superceded with the new `DayOfWeek` declaration this section offers, but actually attempting that results in a multiple declarations error.

**Suggested improvement:** Replace the sentence right before the code example with "We'll modify our previously used DayOfWeek datatype to demonstrate". Update the code example to include `Eq` alongside `Ord` and `Show`. Reduce the paragraph following the code example so it reads as: "You can't have an Ord instance unless you also have an Eq instance, so the compiler will complain if you don't do one (not both) of those two things."

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

## Page 404, Subsection "Unary constructors", Paragraph 2

**Content in question:** "Datatypes that only contain a unary constructor always have the same cardinality as the type they contain."

**Critique:** A couple things. First, consistency. The preceding paragraph uses the term "unary data constructor". Second and more importantly to avoid ambiguity, unary data constructors should be mentioned explicitly because unary type constructors exist, too. For example, `Maybe`. The Haskell wiki gives another example, `Tree` (source: https://wiki.haskell.org/Constructor#Type_constructor). On a sidenote, unary type constructors appear to not be acknowledged as such anywhere in the book. I think they should be to help the reader to the remember the distinction.

**Suggested improvement:** "Datatypes that only contain a unary data constructor always have the same cardinality as the type they contain."

## Page 722, Paragraph 1

**Content in question:** "Note that the second `z'` was an infinite list."

**Critique:** Maybe I missed it but I didn't notice any `z'` being an infinite list.

**Suggested improvement:** Assuming I'm correct, remove the line.

## Page 733

**Content in question:**

**Critique:** like on 732

**Suggested improvement:**

like on 722
like on 725
like on 726
like on 727
like on 744

## Page 738

**Content in question:**

**Critique:** syntax highlighting

**Suggested improvement:**

## Page 740

**Content in question:**

**Critique:** operator should be in parenthesis

**Suggested improvement:**

## Page 780, REPL Example 2

**Content in question:**

-- but note what happens here:
Prelude> f <> g
fromList [('a',1)]

**Critique:**

**Suggested improvement:**

## Page 796, Paragraphs 3 and 4

**Content in question:**

**Critique:** consistency of URI and URL

**Suggested improvement:**

## Page 804

**Content in question:**

**Critique:**

**Suggested improvement:**

add URL for post that was mentioned on page 777
Aditya Bhargava's "Making A Website With Haskell"
https://adit.io/posts/2013-04-15-making-a-website-with-haskell.html

## Page 814

**Content in question:**

**Critique:**

**Suggested improvement:** "`toList :: Foldable t => t a -> [a]`"

## Page 815

**Content in question:**

**Critique:**

**Suggested improvement:** "`null :: Foldable t => t a -> Bool`"

## Page 815

**Content in question:**

**Critique:**

**Suggested improvement:** "`length :: Foldable t => t a -> Int`"

## Page 816

**Content in question:**

**Critique:**

**Suggested improvement:** "`elem :: (Foldable t, Eq a) => a -> t a -> Bool`"

## Page 817

**Content in question:**

**Critique:**

**Suggested improvement:**
"`maximum :: (Foldable t, Ord a) => t a -> a
minimum :: (Foldable t, Ord a) => t a -> a`"

## Page 823, Code Examples 1 and 2

**Content in question:** In both code examples, `traverse :: Applicative f => (a -> f b) -> t a -> f (t b)`

**Critique:** In the type of `traverse`, the `t` should have an instance of `Traversable`. At the top of page 826, the type of `traverse` includes `Traversable`.

**Suggested improvement:** In both code examples, `traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)`

## Page 877, REPL Example 3

**Content in question:**

**Critique:** `random` has a different type now

**Suggested improvement:**
