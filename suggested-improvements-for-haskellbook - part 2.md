# Suggested Improvements for the Haskell Book v1.0RC4 - Part 2

_Note:_ Page numbers used here are the ones for the "haskell-programming-1.0RC4-screen" version of the book.

## Page ix

**Content in question:** "20Foldable"

**Critique:** A space is missing.

**Suggested improvement:** "20 Foldable"

## Page 489, Command Line Example 1

**Content in question:** `$ stack exec hello`

**Critique:** In the last code example on page 486 a similar but slightly different command was used which was `$ stack exec -- hello`. This inconsistency should be addressed.

**Suggested improvement:** Either be consistent about which command to use, or explain the distinction between both commands by the time the second command is used.

## Page 495, Paragraph 6

**Content in question:** "You can set your Prelude prompt permanently, if you wish, by changing it in your GHCi configuration file, but instructions for doing that are somewhat out of the scope of the current chapter."

**Critique:** Motivated readers should be pointed to where they can learn to edit their GHCi configuration.

**Suggested improvement:** Add a footnote to the page, referenced from the end of the content in question, which links to the part of GHCi's documentation that talks about managing its configuration: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-files

## Page 506, Paragraph 1

**Content in question:** "All modules listed below are part of the main _base_ library that comes with your GHC install unless otherwise noted."

**Critique:** It seems the paragraph is referring to the code example directly preceding it. So, "below" should really say "above".

**Suggested improvement:** "All modules listed above are part of the main _base_ library that comes with your GHC install unless otherwise noted."

## Page 511, Code Example 2

**Content in question:** `l < maxWordLength`

**Critique:** The naming of the variable `maxWordLength` implies that I can choose to set `minWordLength` and `maxWordLength` to the same number in order to select words that are of a fixed length. For example, if I kept `minWordLength` as 5 but set `maxWordLength` also as 5 then I should get just a list of words that are 5 characters in length. The code in question prevents that intuition from being realized.

**Suggested improvement:** `l <= maxWordLength`

## Page 511, Paragraph 4

**Content in question:** "We have to subtract one from the length of the word list in order to index it because `length` starts counting from 1 but an index of the list starts from 0."

**Critique:** `length` actually starts counting from 0 because `length []` gives back 0.

**Suggested improvement:** "We have to subtract one from the length of the word list in order to index it because that is the index position of the last item in the list."

## Page 517, List Item 7

**Content in question:** "`guessChar` is the character that the player has guessed."

**Critique:** List item 4 has a more accurate description of what `guessChar` is and that should be reflected here also.

**Suggested improvement:** "`guessChar` is either the character the player has guessed correctly or a `Nothing` representing a character that has yet to be guessed correctly."

## Page 519, Code Example and Paragraph 2

**Content in question:** From the code example: `if (length guessed) > 7 then`. From paragraph 2: "Notice the way it’s written says you lose and exits the game once you've guessed seven characters, even if the final (seventh) guess is the final letter to fill into the word."

**Critique:** The code in question will trigger the game over message once the number of guesses is 8 or more. However, this conflicts with what the text in question states.

**Suggested improvement:** Given that the 7-character limit is refenced multiple times in this chapter, the solution here would be to update the code in question to: `if (length guessed) >= 7 then`.

## Page 524, List Item c

**Content in question:** "”Yay!"

**Critique:** A curly opening double quote should be used here.

**Suggested improvement:** "“Yay!"

## Page 563, List Item 5, Code Example

**Content in question:** `-- quot rem`

**Critique:** This comment adds no useful information and shows what the code already expresses.

**Suggested improvement:** Remove the comment in question.

## Page 570, Section 15.2, Paragraph 4

**Content in question:** "In Haskell, these algebras can be implemented with type classes?; the type classes define the set of operations."

**Critique:** The use of a question here makes the whole statement awkward because the section is about defining what an algebra is and what that means within Haskell. It is expository instead of inquisitive.

**Suggested improvement:** "In Haskell, these algebras can be implemented with type classes. The type classes define the set of operations."

## Page 574, Paragraph 1

**Content in question:** "And if we look a possible definition of `Semigroup` and `Monoid` for lists, we can see how this all lines up:"

**Critique:** Grammar. The beginning is missing a preposition.

**Suggested improvement:** "And if we look at a possible definition of `Semigroup` and `Monoid` for lists, we can see how this all lines up:"

## Page 581, Code Example

**Content in question:** `mconcat = foldr mappend mempty`

**Critique:** `mconcat` is listed with the monoidal laws as if it's one of them
which it's not. It's just a function with a convenient default implementation.

**Suggested improvement:** Remove it. Let the mention of it at the beginning of
page 582 speak for itself.

## Page 593, Section 15.11, Paragraph 1

**Content in question:** "You may have seen mad libs before. The idea is to take a template of phrases, fill them in with blindly selected categories of words, and see if saying the final version is amusing."

**Critique:** Mad Libs should be capitalized because it is a proper noun. Also, the word categories are not selected blindly but the words themselves are.

**Suggested improvement:** "You may have seen Mad Libs before. The idea is to take a template of phrases, fill them in with blindly selected words from predetermined categories, and see if saying the final version is amusing."

## Page 594, Section 15.12, Paragraph 2

**Content in question:** "You can check the associativity of some simple arithemetic expressions..."

**Critique:** "arithemetic" should be spelled "arithmetic".

**Suggested improvement:** "You can check the associativity of some simple arithmetic expressions..."

## Page 602, Paragraph 3

**Content in question:** "Since that data constructor is symbolic rather than alphanumeric, it can't be used as a prefix:"

**Critique:** Prefix notation is possible with parentheses: `data Q' = (:!!:) Int String`

**Suggested improvement:** Mention this possibility.

## Page 602, Paragraph 5

**Content in question:** "On the other hand, an alphanumeric data constructor can't be used as an infix:"

**Critique:** Infix notation is possible with backticks: "data P' = Int `Prefix` String"

**Suggested improvement:** Mention this possibility.

## Page 604, Section 15.14, PAragraph 4

**Content in question:** "It is to be hoped that `Semigroup` will be made a superclass of `Monoid` in an upcoming version of GHC."

**Critique:** `Semigroup` is a superclass of `Monoid` since base-4.11.0.0. (Source: http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:Monoid)

**Suggested improvement:** Replace the sentence in question with "That is why `Semigroup` is a superclass of `Monoid`."

## Page 611, Exercise 7

**Content in question:** The hint text.

**Critique:** The given hint should be shown after the code example to maintain consistency with other previously given exercises, specifically semigroup exercises 3, 9, and 10 which where given in the first part of section 15.15.

**Suggested improvement:** Place the hint after the code example.

## Page 614, Section 15.17

**Content in question:** Both listed resources.

**Critique:** As in previous chapters where online resources are listed, the ones here should include relevant URLs.

**Suggested improvement:** For the first resource, include the URL: https://simple.wikipedia.org/wiki/Algebraic_structure. For the second resource, include the URL: http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html.

## Page 633, Code Example

**Content in question:** `data CountingBad a =`

**Critique:** The `data` keyword is for some reason linked to "news:data".

**Suggested improvement:** Make `data` normal source code text by unlinking it.

## Page 663, Code Example

**Content in question:** `return (input ++ "and me too!")`

**Critique:** Given the context of the surrounding sentences, it seems the intent is to be consistent with the preceding REPL example's `fmap (++ " and me too!") getLine`.

**Suggested improvement:** `return (input ++ " and me too!")`

## Page 673, Section 16.19

**Content in question:** "Gabriel Gonzalez; The functor design pattern."

**Critique:** Online resources should have their links included.

**Suggested improvement:** Provide a link to the blog post which, at the time of writing this, is: http://www.haskellforall.com/2012/09/the-functor-design-pattern.html

## Page 680, Code Example 1

**Content in question:** `mappend :: f f f` and `$ :: (a -> b) a b`

**Critique:** Despite having what I assume is an intentional similarity to the table of types given towards the end of page 679, the code in question should still be written with syntactical correctness in mind.

**Suggested improvement:** Use `mappend :: f -> f -> f` and `$ :: (a -> b) -> a -> b` respectively.

## Page 689, Paragraph 1

**Content in question:** "Then we use the tie-fighter to apply that to the second value,..."

**Critique:** "tie-fighter" should be spelled "TIE fighter" because TIE is an acronym for "twin ion engine" (source: https://starwars.fandom.com/wiki/TIE_Series).

**Suggested improvement:** "Then we use the TIE fighter to apply that to the second value,..."

## Page 706, Code Example 1

**Content in question:** `-- and we hit a situation where want to map`

**Critique:** Grammar.

**Suggested improvement:** `-- and we hit a situation where we want to map`

## Page 721, Paragraph 2

**Content in question:** Note that the second `z'` was an infinite list.

**Critique:** Maybe I missed it but I didn't notice any `z'` being an infinite list.

**Suggested improvement:** Assuming I'm correct, remove the line.

TODO: ## Page 733

TODO: **Content in question:**

TODO: **Critique:** like on 732

TODO: **Suggested improvement:**

TODO: like on 722

TODO: like on 725

TODO: like on 726

TODO: like on 727

TODO: like on 744

TODO: ## Page 738

TODO: **Content in question:**

TODO: **Critique:** syntax highlighting

TODO: **Suggested improvement:**

TODO: ## Page 740

TODO: **Content in question:**

TODO: **Critique:** operator should be in parenthesis

TODO: **Suggested improvement:**

## Page 746, Paragraph 3

**Content in question:** "You developed some intutions above for `do` syntax and the list `Monad`;"

**Critique:** Spelling. "intutions" should be "intuitions".

**Suggested improvement:** "You developed some intuitions above for `do` syntax and the list `Monad`;"

## Page 755, Code Example 2

**Content in question:** `return :: Monad m => a -> m aq`

**Critique:** The `q` seems to be a typo.

**Suggested improvement:** `return :: Monad m => a -> m a`

## Page 773, Section 18.9

**Content in question:** "Gabriel Gonzalez; How to desugar Haskell code"

**Critique:** Online resources should have their links included.

**Suggested improvement:** Provide a link to the blog post which, at the time of writing this, is: http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html

TODO: ## Page 780, REPL Example 2

TODO: **Content in question:**

TODO: -- but note what happens here:
TODO: Prelude> f <> g
TODO: fromList [('a',1)]

TODO: **Critique:**

TODO: **Suggested improvement:**

TODO: ## Page 804

TODO: **Content in question:**

TODO: **Critique:**

TODO: **Suggested improvement:**

TODO: add URL for post that was mentioned on page 777
TODO: Aditya Bhargava's "Making A Website With Haskell"
TODO: http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html

## Page 795, Paragraph 1

**Content in question:** "Where we import the module name followed by parentheses, such as with `replicateM` or `liftIO`, we are saying we only want to import the functions or values of that name and nothing else."

**Critique:** The first part of the sentence is referring to module names but `replicateM` and `liftIO` are not module names.

**Suggested improvement:** "Where we import the module name followed by parentheses, such as with `Control.Monad` or `Control.Monad.IO.Class`, we are saying we only want to import the functions or values listed within the parentheses."

TODO: ## Page 796, Paragraphs 3 and 4

TODO: **Content in question:**

TODO: **Critique:** consistency of URI and URL

TODO: **Suggested improvement:**

## Page 800, Code Example

**Content in question:** `[5]`

**Critique:** The gap above the `[5]` is big enough to imply that the `[5]` is associated with the line below it, though it's really associated with the line above it.

**Suggested improvement:** Shorten the gap above the `[5]`.

## Page 801, Code Example

**Content in question:** `[8]`

**Critique:** The gap above the `[8]` is big enough to imply that the `[8]` is associated with the line below it, though it's really associated with the line above it.

**Suggested improvement:** Shorten the gap above the `[8]`.

## Page 804, Section 19.8

**Content in question:** The only listed resource.

**Critique:** Online resources should have their links included.

**Suggested improvement:** Provide a link to the blog post which, at the time of writing this, is: http://www.serpentine.com/blog/2012/09/12/the-case-of-the-mysterious-explosion-in-space/

TODO: ## Page 814

TODO: **Content in question:**

TODO: **Critique:**

TODO: **Suggested improvement:**
TODO: toList :: Foldable t => t a -> [a]

TODO: ## Page 815

TODO: **Content in question:**

TODO: **Critique:**

TODO: **Suggested improvement:**
TODO: null :: Foldable t => t a -> Bool

TODO: ## Page 815

TODO: **Content in question:**

TODO: **Critique:**

TODO: **Suggested improvement:**
TODO: length :: Foldable t => t a -> Int

TODO: ## Page 816

TODO: **Content in question:**

TODO: **Critique:**

TODO: **Suggested improvement:**
TODO: elem :: (Foldable t, Eq a) => a -> t a -> Bool

TODO: ## Page 817

TODO: **Content in question:**

TODO: **Critique:**

TODO: **Suggested improvement:**
TODO: maximum :: (Foldable t, Ord a) => t a -> a
TODO: minimum :: (Foldable t, Ord a) => t a -> a

## Page 820, Section 20.7

**Content in question:** The only listed resource.

**Critique:** Online resources should have their links included.

**Suggested improvement:** Provide a link to the blog post which, at the time of writing this, is: https://blog.jakuba.net/2014/07/30/Foldable-and-Traversable/

## Page 823, Code Examples 1 and 2

**Content in question:** In both code examples, `traverse :: Applicative f => (a -> f b) -> t a -> f (t b)`

**Critique:** In the type of `traverse`, the `t` should have an instance of `Traversable`. On page 826, the type of `traverse` includes `Traversable`.

**Suggested improvement:** In both code examples, `traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)`

## Page 842, Section 21.13

**Content in question:**
"Foldable and Traversable; Jakub Arnold."

**Critique:** Online resources should have their links included.

**Suggested improvement:** Provide a link to the blog post which, at the time of writing this, is: https://blog.jakuba.net/2014-07-30-foldable-and-traversable/

TODO: ## Page 877, REPL Example 3

TODO: **Content in question:**

TODO: **Critique:**
TODO: `random` has a different type now

TODO: **Suggested improvement:**

## Page 878, REPL Example

**Content in question:** `Prelude> rx :: (Int, StdGen); rx = random (snd sg3)`

**Critique:** `sg3` is not mentioned anywhere. Perhaps `sg` was intended instead.

**Suggested improvement:** `Prelude> rx :: (Int, StdGen); rx = random (snd sg)`

# ------------------------------------------------------------------------------

## Every Page of both the "haskell-programming-1.0RC4-screen" and "haskell-programming-1.0RC4-ereader" Versions of the Book

**Content in question:** Code text.

**Critique:** When I copy-and-paste code examples into my text editor to help get me started on exercises, the pasted code does not include the indentation as shown in the book. Restoring the indentation is inconvenient and interrupts the flow of working on exercises.

**Suggested improvement:** If possible, generate both versions of the book in such a way as to have indentation preserved when a reader does a copy-and-paste from the book into their code editor.
