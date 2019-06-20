# Haskell Symbols

## General Notation

| Symbol          | Context         | Read as                                         |
| --------------- | --------------- | ----------------------------------------------- |
| `_`             |                 | "whatever" / "anything" / "blank" / "wildcard"  |
| `--`            |                 | "comment" / "note"                              |
| `->`            | case branch     | "then"                                          |
| `,`             |                 | ""                                              |
| `;`             |                 | ""                                              |
| `!`             |                 | "strict"                                        |
| `.`             | numeric literal | "point"                                         |
| `'`             | identifier name | "prime"                                         |
| `''`            | identifier name | "double prime"                                  |
| `(` ... `)`     | expression      | "" / "subexpression"                            |
| `{` ... `}`     | expression      | "" / "explicit block"                           |
| `{-` ... `-}`   |                 | "nested comment" / "block comment" / "note"     |
| `{-#` ... `#-}` |                 | "pragma"                                        |
| `//`            | Data.Array      | "update with"                                   |
| `&&`            |                 | "and" / "logical and" / "boolean and"           |
| `=`             |                 | "equals" / "is" / "is set to" / "is defined as" |
| `\|`            | multi-way if    | "in the case of"                                            |
| `\|\|`          |                 | "or" / "logical or" / "boolean or"              |
| `~`             |                 | "lazy"                                          |

## Type Notation

| Symbol      | Context          | Read as                                                   |
| ----------- | ---------------- | --------------------------------------------------------- |
| `->`        | type signature   | "to"                                                      |
| `::`        | kind signature   | "has kind" / "of kind" / "having kind" / "is a"           |
| `::`        | type annotation  | "as type" / "as a" / "as an"                              |
| `::`        | type signature   | "has type" / "of type" / "having type" / "is a" / "is an" |
| `[` ... `]` |                  | "list of"                                                 |
| `{..}`      | pattern match    | "record having"                                           |
| `{` ... `}` | pattern match    | "record having"                                           |
| `{` ... `}` | data constructor | "record having"                                           |
| `*`         | kind signature   | "star" / "boxed type"                                     |
| `=>`        | type signature   | "in" / "implies" / "then"                                 |
| `\|`        |                  | "or"                                                      |

## Function Notation

| Symbol              | Context                  | Read as                                                     |
| ------------------- | ------------------------ | ----------------------------------------------------------- |
| `` ` `` ... `` ` `` |                          | "" / "inline"                                               |
| `->`                | anonymous function       | "to" / "returns"                                            |
| `.`                 | expression               | "dot" / "compose" / "composed with"                         |
| `(` ... `)`         | prefix operator notation | ""                                                          |
| `@`                 | pattern match            | "as"                                                        |
| `\`                 | anonymous function       | "lambda"                                                    |
| `&`                 |                          | "then" / "piped to"                                         |
| `=`                 | function declaration     | "returns" / "equals" / "is" / "is set to" / "is defined as" |
| `<-`                | pattern guard            | "matches on" / "matching on"                                |
| `\|`                | guard                    | "in the case of"                                            |
| `$`                 |                          | "of" / "applied to"                                         |
| whitespace          | function application     | "of" / "applied to"                                         |

## Arithmetic Notation

| Symbol | Context    | Read as              |
| ------ | ---------- | -------------------- |
| `-`    |            | "minus" / "subtract" |
| `-`    | negation   | "negative"           |
| `*`    |            | "times"              |
| `/`    |            | "divide"             |
| `%`    | Data.Ratio | "over"               |
| `^`    |            | "to the power of"    |
| `^^`   |            | "to the power of"    |
| `+`    |            | "plus"               |

## Text Notation

| Symbol | Context                 | Read as           |
| ------ | ----------------------- | ----------------- |
| `'`    | character literal       | ""                |
| `"`    | end of string literal   | ""                |
| `"`    | start of string literal | "" / "the string" |
| `\`    | multiline string        | ""                |

## Comparison Notation

| Symbol | Context | Read as                       |
| ------ | ------- | ----------------------------- |
| `/=`   |         | "is not equal to"             |
| `<`    |         | "is less than"                |
| `<=`   |         | "is less than or equal to"    |
| `==`   |         | "equals" / "is equal to"      |
| `>`    |         | "is greater than"             |
| `>=`   |         | "is greater than or equal to" |

## List Notation

| Symbol      | Context            | Read as                      |
| ----------- | ------------------ | ---------------------------- |
| `:`         |                    | "cons"                       |
| `!!`        |                    | "index" / "at index" / "sub" |
| `..`        | bounded range      | "to" / "through" / "up to"   |
| `..`        | unbounded range    | "and so on" / "etc"          |
| `[]`        |                    | "empty list"                 |
| `[` ... `]` |                    | "list of"                    |
| `++`        |                    | "append" / "concat"          |
| `\|`        | list comprehension | "such that"                  |

## Tuple Notation

| Symbol      | Context | Read as                |
| ----------- | ------- | ---------------------- |
| `,`         |         | "" / "and"             |
| `()`        |         | "unit"                 |
| `(,)`       |         | "pair" / "pair of"     |
| `(,,)`      |         | "triple" / "triple of" |
| `(` ... `)` |         | "tuple" / "tuple of"   |

## Functor Notation

| Symbol | Context | Read as                      |
| ------ | ------- | ---------------------------- |
| `<$`   |         | "map-replace by"             |
| `<$>`  |         | "map" / "fmap" / "infix map" |
| `$>`   |         | "map-replaces"               |

## Applicative Notation

| Symbol | Context             | Read as                       |
| ------ | ------------------- | ----------------------------- |
| `*>`   | Control.Applicative | "then"                        |
| `<*`   | Control.Applicative | "after" / "from"              |
| `<**>` | Control.Applicative | "ap" / "apply" / "applied to" |
| `<*>`  | Control.Applicative | "ap" / "apply" / "applied to" |
| `<\|>` |                     | "or"                          |

## Monadic Notation

| Symbol | Context     | Read as                                                     |
| ------ | ----------- | ----------------------------------------------------------- |
| `<-`   | do-notation | "is bound to" / "binds to" / "draws from" / "is drawn from" |
| `<=<`  |             | "compose" / "composed with" / "left fish"                   |
| `=<<`  |             | "bound by"                                                  |
| `>=>`  |             | "then" / "piped to" / "right fish"                          |
| `>>`   |             | "then"                                                      |
| `>>=`  |             | "bind" / "binds"                                            |

## Sequence Notation

| Symbol | Context       | Read as                                |
| ------ | ------------- | -------------------------------------- |
| `!?`   | Data.Sequence | "lookup"                               |
| `<\|`  | Data.Sequence | "prepend to"                           |
| `><`   | Data.Sequence | "concat" / "combine" / "combined with" |
| `\|>`  | Data.Sequence | "append"                               |

## Arrow Notation

| Symbol   | Context          | Read as                             |
| -------- | ---------------- | ----------------------------------- |
| `-<`     |                  | "takes"                             |
| `-<<`    |                  | "takes"                             |
| `***`    | Control.Arrow    | "split merge" / "split merge with"  |
| `&&&`    | Control.Arrow    | "fanout" / "fanout with"            |
| `^<<`    | Control.Arrow    | "precompose" / "precomposed with"   |
| `^>>`    | Control.Arrow    | "precomposed into"                  |
| `+++`    | Control.Arrow    | "split merge" / "split merge with"  |
| `<+>`    | Control.Arrow    | "plus" / "mplus"                    |
| `<<^`    | Control.Arrow    | "postcompose" / "postcomposed with" |
| `<<<`    | Control.Category | "compose" / "composed with"         |
| `>>^`    | Control.Arrow    | "postcomposed into"                 |
| `>>>`    | Control.Category | "into" / "composed into"            |
| `\|\|\|` | Control.Arrow    | "fanin" / "fanin with"              |

## Module Notation

| Symbol      | Context            | Read as              |
| ----------- | ------------------ | -------------------- |
| `.`         | module name        | "dot"                |
| `()`        | module declaration | "exports nothing"    |
| `(..)`      | module importing   | "with all its names" |
| `(` ... `)` | module declaration | "exports"            |
| `(` ... `)` | module importing   | "specifically"       |

## File System Notation

| Symbol | Context         | Read as                      |
| ------ | --------------- | ---------------------------- |
| `-<.>` | System.FilePath | "dot" / "with new extension" |
| `<.>`  | System.FilePath | "dot" / "with extension"     |
| `</>`  | System.FilePath | "slash" / "with path"        |

## Template Haskell Notation

| Symbol             | Context                    | Read as                         |
| ------------------ | -------------------------- | ------------------------------- |
| `[\|` ... `\|]`    |                            | "quote"                         |
| `[e\|` ... `\|]`   |                            | "quote"                         |
| `[d\|` ... `\|]`   |                            | "quote declarations"            |
| `[t\|` ... `\|]`   |                            | "quote type"                    |
| `[p\|` ... `\|]`   |                            | "quote pattern"                 |
| `[`x`\|` ... `\|]` |                            | "quote" x                       |
| `$`x               |                            | "splice in" x                   |
| `$(` ... `)`       |                            | "splice in"                     |
| `<+>`              | Language.Haskell.TH.PprLib | "beside" / "plus" / "mplus"     |
| `<>`               | Language.Haskell.TH.PprLib | "beside" / "append" / "mappend" |
| `$+$`              | Language.Haskell.TH.PprLib | "beside"                        |
| `$$`               | Language.Haskell.TH.PprLib | "beside"                        |

## Bitwise Notation

| Symbol  | Context   | Read as               |
| ------- | --------- | --------------------- |
| `.&.`   | Data.Bits | "and" / "bitiwse and" |
| `.\|.`  | Data.Bits | "or" / "bitiwse or"   |

## Parallel Programming Notation

| Symbol                 | Context                     | Read as                                                    |
| ---------------------- | --------------------------- | ---------------------------------------------------------- |
| `-\|`                  | Control.Parallel.Strategies | "then" / "piped to" / "piped sequentially to"              |
| `-\|\|`                | Control.Parallel.Strategies | "then" / "piped to" / "piped in parallel to"               |
| `.\|`                  | Control.Parallel.Strategies | "compose" / "composed with" / "composed sequentially with" |
| `.\|\|`                | Control.Parallel.Strategies | "compose" / "composed with" / "composed in parallel with"  |
| `>\|`   _(deprecated)_ | Control.Parallel.Strategies | "of" / "applied to" / "applied sequentially to"            |
| `>\|\|` _(deprecated)_ | Control.Parallel.Strategies | "of" / "applied to" / "applied in parallel to"             |
| `$\|`                  | Control.Parallel.Strategies | "of" / "applied to" / "applied sequentially to"            |
| `$\|\|`                | Control.Parallel.Strategies | "of" / "applied to" / "applied in parallel to"             |

## QuickCheck Notation

| Symbol              | Context         | Read as                     |
| ------------------- | --------------- | --------------------------- |
| `.&.`               | Test.QuickCheck | "or instead"                |
| `.&&.`              | Test.QuickCheck | "and" / "and also"          |
| `.\|\|.`            | Test.QuickCheck | "or"                        |
| `><` _(deprecated)_ | Test.QuickCheck | "combine" / "combined with" |

## Parser Notation

| Symbol   | Context                         | Read as                                     |
| -------- | ------------------------------- | ------------------------------------------- |
| `<?>`    | Text.Parsec                     | "or" / "or if failed then"                  |
| `<\|?>`  | Text.Parsec.Perm                | "add" / "add either ... or"                 |
| `<\|>`   | Text.Parsec                     | "or"                                        |
| `<\|\|>` | Text.Parsec.Perm                | "add"                                       |
| `<$?>`   | Text.Parsec.Perm                | "create with" / "either create with ... or" |
| `<$$>`   | Text.Parsec.Perm                | "create with"                               |
| `<++`    | Text.ParserCombinators.ReadP    | "or"                                        |
| `<++`    | Text.ParserCombinators.ReadPrec | "or"                                        |

## Builder Notation

| Symbol | Context                      | Read as                      |
| ------ | ---------------------------- | ---------------------------- |
| `>*<`  | Data.ByteString.Builder.Prim | "combine" / "combined with"  |
| `>$<`  | Data.ByteString.Builder.Prim | "map" / "fmap" / "infix map" |

## Language Extension Notation

| Symbol        | Context        | Read as                              |
| ------------- | -------------- | ------------------------------------ |
| `?`           | ImplicitParams | "implicit" / "dynamic"               |
| `(#` ... `#)` | UnboxedSums    | "unboxed sum" / "unboxed sum of"     |
| `(#` ... `#)` | UnboxedTuples  | "unboxed tuple" / "unboxed tuple of" |
| `#`           | MagicHash      | "hash"                               |
| `\|`          | UnboxedSums    | "or"                                 |

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
