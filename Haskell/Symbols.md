# Haskell Symbols

## General Notation

| Symbol        | Context                  | Read as                                               |
| ------------- | ------------------------ | ----------------------------------------------------- |
| `_`           |                          | "whatever" / "anything" / "blank" / "wildcard"        |
| `--`          |                          | "comment" / "note"                                    |
| `->`          | case branch              | "then"                                                |
| `,`           |                          | ""                                                    |
| `;`           |                          | ""                                                    |
| `!`           |                          | "strict"                                              |
| `?`           | ImplicitParams extension | "implicit" / "dynamic" / "dynamically-bound variable" |
| `.`           | numeric literal          | "point"                                               |
| `'`           | identifier name          | "prime"                                               |
| `''`          | identifier name          | "double prime"                                        |
| `(`...`)`     | expression               | "" / "subexpression"                                  |
| `{`...`}`     | expression               | "" / "explicit block"                                 |
| `{-`...`-}`   |                          | "nested comment" / "block comment" / "note"           |
| `{-#`...`#-}` |                          | "pragma"                                              |
| `//`          | using Data.Array         | "update with"                                         |
| `&&`          |                          | "and" / "logical and" / "boolean and"                 |
| `#`           | using MagicHash          | "hash"                                                |
| `=`           |                          | "equals" / "is" / "is set to" / "is defined as"       |
| `||`          |                          | "or" / "logical or" / "boolean or"                    |
| `~`           |                          | "lazy"                                                |

## Type Notation

| Symbol    | Context          | Read as                                                   |
| --------- | ---------------- | --------------------------------------------------------- |
| `->`      | type signature   | "to"                                                      |
| `::`      | kind signature   | "has kind" / "of kind" / "having kind" / "is a"           |
| `::`      | type annotation  | "as type" / "as a" / "as an"                              |
| `::`      | type signature   | "has type" / "of type" / "having type" / "is a" / "is an" |
| `{`...`}` | data constructor | "record having"                                           |
| `*`       | kind signature   | "star" / "boxed type"                                     |
| `=>`      | type declaration | "in" / "implies" / "then"                                 |
| `|`       | type declaration | "or"                                                      |
| space     | type constructor | ""                                                        |

## Function Notation

| Symbol        | Context                  | Read as                                                     |
| ------------- | ------------------------ | ----------------------------------------------------------- |
| ` \``... `\`` |                          | "" / "inline"                                               |
| `->`          |                          | "to"                                                        |
| `.`           | expression               | "dot" / "compose" / "composed with"                         |
| `(`...`)`     | prefix operator notation | ""                                                          |
| `@`           | pattern match            | "as"                                                        |
| `\\`          | function declaration     | "lambda"                                                    |
| `&`           |                          | "then" / "piped to"                                         |
| `=`           | function declaration     | "returns" / "equals" / "is" / "is set to" / "is defined as" |
| `|`           | guard                    | "in the case of"                                            |
| `$`           |                          | "of" / "applied to"                                         |
| space         | function application     | "of" / "applied to"                                         |

## Arithmetic Notation

| Symbol | Context          | Read as           |
| ------ | ---------------- | ----------------- |
| `-`    |                  | "minus"           |
| `-`    | negation         | "negative"        |
| `*`    |                  | "times"           |
| `/`    |                  | "divide"          |
| `%`    | using Data.Ratio | "over"            |
| `^`    |                  | "to the power of" |
| `^^`   |                  | "to the power of" |
| `+`    |                  | "plus"            |

## Text Notation

| Symbol | Context                 | Read as           |
| ------ | ----------------------- | ----------------- |
| `'`    | character literal       | ""                |
| `"`    | end of string literal   | ""                |
| `"`    | start of string literal | "" / "the string" |
| `\\`   | multiline string        | ""                |

## Comparison Notation

| Symbol | Context | Read as                    |
| ------ | ------- | -------------------------- |
| `/=`   |         | "not equal to"             |
| `<`    |         | "less than"                |
| `<=`   |         | "less than or equal to"    |
| `==`   |         | "equals" / "is equal to"   |
| `>`    |         | "greater than"             |
| `>=`   |         | "greater than or equal to" |

## List Notation

| Symbol    | Context            | Read as              |
| --------- | ------------------ | -------------------- |
| `:`       |                    | "cons"               |
| `!!`      |                    | "index" / "at index" |
| `..`      | bounded range      | "to" / "up to"       |
| `..`      | unbounded range    | "and so on" / "etc"  |
| `[]`      |                    | "empty list"         |
| `[`...`]` |                    | "list of"            |
| `++`      |                    | "append" / "concat"  |
| `|`       | list comprehension | "such that"          |

## Tuple Notation

| Symbol    | Context | Read as                |
| --------- | ------- | ---------------------- |
| `()`      |         | "unit"                 |
| `(,)`     |         | "pair" / "pair of"     |
| `(,,)`    |         | "triple" / "triple of" |
| `(`...`)` |         | "tuple" / "tuple of"   |

## Functor Notation

| Symbol | Context | Read as                      |
| ------ | ------- | ---------------------------- |
| `<$`   |         | "map-replace by"             |
| `<$>`  |         | "map" / "fmap" / "infix map" |
| `$>`   |         | "map-replaces"               |

## Applicative Notation

| Symbol | Context                   | Read as                       |
| ------ | ------------------------- | ----------------------------- |
| `*>`   | using Control.Applicative | "then"                        |
| `<*`   | using Control.Applicative | "after" / "from"              |
| `<**>` | using Control.Applicative | "ap" / "apply" / "applied to" |
| `<*>`  | using Control.Applicative | "ap" / "apply" / "applied to" |
| `<|>`  |                           | "or"                          |

## Monadic Notation

| Symbol | Context     | Read as                                   |
| ------ | ----------- | ----------------------------------------- |
| `<-`   | do-notation | "draws from" / "is drawn from" / "bind"   |
| `<=<`  |             | "left fish" / "compose" / "composed with" |
| `=<<`  |             | "bound by"                                |
| `>=>`  |             | "right fish" / "then" / "piped to"        |
| `>>`   |             | "then"                                    |
| `>>=`  |             | "bind"                                    |

## Sequence Notation

| Symbol | Context             | Read as                                |
| ------ | ------------------- | -------------------------------------- |
| `!?`   | using Data.Sequence | "lookup"                               |
| `<|`   | using Data.Sequence | "prepend to"                           |
| `><`   | using Data.Sequence | "concat" / "combine" / "combined with" |
| `|>`   | using Data.Sequence | "append"                               |

## Arrow Notation

| Symbol | Context                | Read as                             |
| ------ | ---------------------- | ----------------------------------- |
| `-<`   |                        | "takes"                             |
| `-<<`  |                        | "takes"                             |
| `***`  | using Control.Arrow    | "split merge" / "split merge with"  |
| `&&&`  | using Control.Arrow    | "fanout" / "fanout with"            |
| `^<<`  | using Control.Arrow    | "precompose" / "precomposed with"   |
| `^>>`  | using Control.Arrow    | "precomposed into"                  |
| `+++`  | using Control.Arrow    | "split merge" / "split merge with"  |
| `<+>`  | using Control.Arrow    | "plus" / "mplus"                    |
| `<<^`  | using Control.Arrow    | "postcompose" / "postcomposed with" |
| `<<<`  | using Control.Category | "compose" / "composed with"         |
| `>>^`  | using Control.Arrow    | "postcomposed into"                 |
| `>>>`  | using Control.Category | "into" / "composed into"            |
| `|||`  | using Control.Arrow    | "fanin" / "fanin with"              |

## Module Notation

| Symbol    | Context            | Read as           |
| --------- | ------------------ | ----------------- |
| `()`      | module declaration | "exports nothing" |
| `(`...`)` | module declaration | "exports"         |
| `(`...`)` | module importing   | "specifically"    |

## Template Haskell Notation

| Symbol      | Context                          | Read as                         |
| ----------- | -------------------------------- | ------------------------------- |
| `[|`...`|]` |                                  | "quote"                         |
| `<+>`       | using Language.Haskell.TH.PprLib | "beside" / "plus" / "mplus"     |
| `<>`        | using Language.Haskell.TH.PprLib | "beside" / "append" / "mappend" |
| `$+$`       | using Language.Haskell.TH.PprLib | "beside"                        |
| `$$`        | using Language.Haskell.TH.PprLib | "beside"                        |

## Bitwise Notation

| Symbol | Context         | Read as               |
| ------ | --------------- | --------------------- |
| `.&.`  | using Data.Bits | "and" / "bitiwse and" |
| `.|.`  | using Data.Bits | "or" / "bitiwse or"   |

## Parallel Programming Notation

| Symbol | Context                           | Read as                                                    |
| ------ | --------------------------------- | ---------------------------------------------------------- |
| `-|`   | using Control.Parallel.Strategies | "then" / "piped to" / "piped sequentially to"              |
| `-||`  | using Control.Parallel.Strategies | "then" / "piped to" / "piped in parallel to"               |
| `.|`   | using Control.Parallel.Strategies | "compose" / "composed with" / "composed sequentially with" |
| `.||`  | using Control.Parallel.Strategies | "compose" / "composed with" / "composed in parallel with"  |
| `>|`   | using Control.Parallel.Strategies | "deprecated of"                                            |
| `>||`  | using Control.Parallel.Strategies | "deprecated of"                                            |
| `$|`   | using Control.Parallel.Strategies | "of" / "applied to" / "applied sequentially to"            |
| `$||`  | using Control.Parallel.Strategies | "of" / "applied to" / "applied in parallel to"             |

## File System Notation

| Symbol | Context               | Read as                      |
| ------ | --------------------- | ---------------------------- |
| `-<.>` | using System.FilePath | "dot" / "with new extension" |
| `<.>`  | using System.FilePath | "dot" / "with extension"     |
| `</>`  | using System.FilePath | "slash" / "with path"        |

## QuickCheck Notation

| Symbol | Context | Read as                     |
| ------ | ------- | --------------------------- |
| `.&.`  |         | "fickle and"                |
| `.&&.` |         | "and"                       |
| `.||.` |         | "or"                        |
| `><`   |         | "combine" / "combined with" |

## Parser Notation

| Symbol | Context                               | Read as                                   |
| ------ | ------------------------------------- | ----------------------------------------- |
| `<?>`  | using Text.Parsec                     | "or" / "or if failed then"                |
| `<|?>` | using Text.Parsec.Perm                | "add" / "add either...or"                 |
| `<|>`  | using Text.Parsec                     | "or"                                      |
| `<||>` | using Text.Parsec.Perm                | "add"                                     |
| `<$?>` | using Text.Parsec.Perm                | "create with" / "either create with...or" |
| `<$$>` | using Text.Parsec.Perm                | "create with"                             |
| `<++`  | using Text.ParserCombinators.ReadP    | "or"                                      |
| `<++`  | using Text.ParserCombinators.ReadPrec | "or"                                      |

## Builder Notation

| Symbol | Context                            | Read as                      |
| ------ | ---------------------------------- | ---------------------------- |
| `>*<`  | using Data.ByteString.Builder.Prim | "combine" / "combined with"  |
| `>$<`  | using Data.ByteString.Builder.Prim | "map" / "fmap" / "infix map" |

## References

- [Haskell (alternative) - Operator Glossary](https://haskell-lang.org/tutorial/operators)

- [Haskell/GHC symbol search cheatsheet](https://github.com/takenobu-hs/haskell-symbol-search-cheatsheet)

- [Are there pronounceable names for common Haskell operators?](https://stackoverflow.com/q/7746894)

  - https://stackoverflow.com/a/16801782
  - https://stackoverflow.com/a/39758181
  - https://stackoverflow.com/a/7747115
  - https://stackoverflow.com/a/7747149

- [What exactly is the kind “\*” in Haskell?](https://stackoverflow.com/q/27095011)

  - https://stackoverflow.com/a/27095012
