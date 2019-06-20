-- Some content copied from [PureScript by Example](https://leanpub.com/purescript).

module PureScript where

-- Backwards composition (`<<<`)
-- Forwards composition (`>>>`)


-- Backwards composition (`<<<`)
-- Forwards composition (`>>>`)

f = (+) 1
g = (*) 10

a = f <<< g
b = f >>> g

_ = a 2   -- `21`
_ = b 2   -- `30`






-- not in tail position
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- in tail position
fact :: Int -> Int -> Int
fact 0 acc = acc
fact n acc = fact (n - 1) (acc * n)







even :: Int -> Boolean
even 0 = true
even 1 = false
even n = even (n - 2)





pairs n = concatMap (\i -> map (\j -> [i, j]) (1 .. n)) (1 .. n)

pairs 3
-- [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]





pairs n = concatMap (\i -> map (\j -> [i, j]) (i .. n)) (1 .. n)

pairs 3
-- [[1,1],[1,2],[1,3],[2,2],[2,3],[3,3]]











-- The key observation which enables tail recursion optimization is the following: a recursive call in tail position
-- to a function can be replaced with a jump, which does not allocate a stack frame. A call is in tail position
-- when it is the last call made before a function returns.






-- Writing algorithms directly in terms of combinators such as map and fold has the added advantage of code
-- simplicity - these combinators are well-understood, and as such, communicate the intent of the algorithm
-- much better than explicit recursion.








-- Pattern matching is a common technique in functional programming and allows the developer to write
-- compact functions which express potentially complex ideas, by breaking their implementation down into
-- multiple cases.







-- A function written using pattern matching works by pairing sets of conditions with their results. Each line is
-- called an alternative or a case. The expressions on the left of the equals sign are called patterns, and each case
-- consists of one or more patterns, separated by spaces. Cases describe which conditions the arguments must
-- satisfy before the expression on the right of the equals sign should be evaluated and returned. Each case is
-- tried in order, and the first case whose patterns match their inputs determines the return value.







-- A guard is a boolean-valued expression which must be satisfied in addition to the constraints imposed by the
-- patterns.

-- Guards appear on the left of the equals symbol, separated from the list of
-- patterns by a pipe character (|).




gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m
          then gcd (n - m) m
          else gcd n (m - n)

gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 n = n
gcd n m | n > m     = gcd (n - m) m
        | otherwise = gcd n (m - n)












isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _  = false




-- TODO: find out if Haskell can pattern match like the following:

takeFive :: Array Int -> Int
takeFive [0, 1, a, b, _] = a * b
takeFive _               = 0







showPerson :: { first :: String, last :: String } -> String
showPerson { first: x, last: y } = y <> ", " <> x









type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false







-- Patterns can be named to bring additional names into scope when using nested patterns. Any pattern can be
-- named by using the @ symbol.








sortPair :: Array Int -> Array Int
sortPair arr@[x, y]
  | x <= y    = arr
  | otherwise = [y, x]
sortPair arr = arr








-- Functions which return a value for any combination of inputs are called total functions, and functions which
-- do not are called partial.







-- The only way to consume a value of an algebraic data type is to use a pattern to match its constructor.






-- Each constructor can be used as a pattern, and the arguments to the constructor can themselves be bound using patterns of their own.






-- Row polymorphism: Ability to pattern match records on only the fields needed.











-- A type class instance contains implementations of the functions defined in a type class, specialized to a
-- particular type.





-- We say that the `Boolean` type _belongs to the Show type class_.




-- The Field type class identifies those types which support numeric operators such as addition, subtraction,
-- multiplication and division.

-- The Semigroup type class identifies those types which support an append operation to combine two values.

-- The Monoid type class (provided by the purescript-monoid package) extends the Semigroup type class with
-- the concept of an empty value, called mempty

-- If the Monoid type class identifies those types which act as the result of a fold, then the Foldable type class
-- identifies those type constructors which can be used as the source of a fold.






-- Foldable captures the notion of an ordered container.







module Stream where

import Data.Array as Array
import Data.Maybe (Maybe)
import Data.String as String

-- class Stream stream element where
--   uncons :: stream -> Maybe { head :: element, tail :: stream }

class Stream stream element | stream -> element where
  uncons :: stream -> Maybe { head :: element, tail :: stream }

instance streamArray :: Stream (Array a) a where
  uncons = Array.uncons

instance streamString :: Stream String Char where
  uncons = String.uncons

genericTail xs = map _.tail (uncons xs)


-- > :type genericTail
-- forall stream element. Stream stream element => stream -> Maybe stream


-- > genericTail "testing"
-- (Just "esting")








class Monoid m <= Action m a where
  act :: m -> a -> a








newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

class Eq a <= Hashable a where
  hash :: a -> HashCode

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash

instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true  = hashCode 1

instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode

instance hashArray :: Hashable a => Hashable (Array a) where
  hash = foldl combineHashes (hashCode 0) <<< map hash

instance hashString :: Hashable String where
  hash = hash <<< toCharArray











lift3 :: forall a b c d f
       . Apply f
      => (a -> b -> c -> d)
      -> f a
      -> f b
      -> f c
      -> f d
lift3 f x y z = f <$> x <*> y <*> z







-- > address <$> Just "123 Fake St." <*> Just "Faketown" <*> Just "CA"
-- Just (Address { street: "123 Fake St.", city: "Faketown", state: "CA" })

-- > address <$> Just "123 Fake St." <*> Nothing <*> Just "CA"
-- Nothing














import Prelude

fullName first middle last = last <> ", " <> first <> " " <> middle

fullName "Phillip" "A" "Freeman"
-- Freeman, Phillip A

import Data.Maybe

fullName <$> Just "Phillip" <*> Just "A" <*> Just "Freeman"
-- Just ("Freeman, Phillip A")

fullName <$> Just "Phillip" <*> Nothing <*> Just "Freeman"
-- Nothing

withError Nothing  err = Left err
withError (Just a) _   = Right a

fullNameEither first middle last =
  fullName <$> (first  `withError` "First name was missing")
           <*> (middle `withError` "Middle name was missing")
           <*> (last   `withError` "Last name was missing")

-- > :type fullNameEither
-- Maybe String -> Maybe String -> Maybe String -> Either String String

fullNameEither (Just "Phillip") (Just "A") (Just "Freeman")
-- (Right "Freeman, Phillip A")

fullNameEither (Just "Phillip") Nothing (Just "Freeman")
-- (Left "Middle name was missing")

fullNameEither (Just "Phillip") (Just "A") Nothing
-- (Left "Last name was missing")

fullNameEither Nothing Nothing Nothing
-- (Left "First name was missing")




combineList :: forall f a. Applicative f => List (f a) -> f (List a)
combineList Nil         = pure Nil
combineList (Cons x xs) = Cons <$> x <*> combineList xs





import Data.List
import Data.Maybe

combineList (fromFoldable [Just 1, Just 2, Just 3])
-- (Just (Cons 1 (Cons 2 (Cons 3 Nil))))

combineList (fromFoldable [Just 1, Nothing, Just 2])
-- Nothing








import Data.AddressBook
import Data.AddressBook.Validation

validateAddress $ address "" "" ""
-- (Invalid [ "Field 'Street' cannot be empty"
--          , "Field 'City' cannot be empty"
--          , "Field 'State' must have length 2"
--          ])

validateAddress $ address "" "" "CA"
-- (Invalid [ "Field 'Street' cannot be empty"
--          , "Field 'City' cannot be empty"
--          ])




matches :: String -> R.Regex -> String -> V Errors Unit
matches _ regex value | R.test regex value =
  pure unit
matches field _ _ =
  invalid ["Field '" <> field <> "' did not match the required format"]



validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)


validatePhoneNumber $ phoneNumber HomePhone "555-555-5555"
-- Valid (PhoneNumber { type: HomePhone, number: "555-555-5555" })

validatePhoneNumber $ phoneNumber HomePhone "555.555.5555"
-- Invalid (["Field 'Number' did not match the required format"])







arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] =
  invalid ["Field '" <> field <> "' must contain at least one value"]
arrayNonEmpty _ _ =
  pure unit





validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
person <$> (nonEmpty "First Name" o.firstName *> pure o.firstName)
       <*> (nonEmpty "Last Name" o.lastName   *> pure o.lastName)
       <*> validateAddress o.address
       <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)







-- traverse :: forall a b f. Applicative f => (a -> f b) -> List a -> f (List b)
traverse _ Nil = pure Nil
traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs









import Data.Maybe
import Data.Traversable

traverse (nonEmpty "Example") Nothing
-- (Valid Nothing)

traverse (nonEmpty "Example") (Just "")
-- (Invalid ["Field 'Example' cannot be empty"])

traverse (nonEmpty "Example") (Just "Testing")
-- (Valid (Just unit))






-- Overlapping instances rule:  Whenever a
-- type class instance is required at a function call site, PureScript will use the information inferred by the type
-- checker to choose the correct instance. At that time, there should be exactly one appropriate instance for that
-- type. If there are multiple valid instances, the compiler will issue a warning.






-- Functional dependency:  A functional dependency asserts a functional
-- relationship between the type arguments of a multi-parameter type class.  This functional dependency tells
-- the compiler that there is a function from stream types to (unique) element types, so if the compiler knows
-- the stream type, then it can commit to the element type.





-- This chapter also gave an introduction to the notion of type class laws, a technique for proving properties
-- about code which uses type classes for abstraction. Type class laws are part of a larger subject called equational
-- reasoning, in which the properties of a programming language and its type system are used to enable logical
-- reasoning about its programs. This is an important idea, and will be a theme which we will return to
-- throughout the rest of the book.






-- For any fixed list size n, there is a function of n arguments which builds a list of size n out of those arguments.
-- For example, if n is 3, the function is \x y z -> x : y : z : Nil. This function has type a -> a -> a ->
-- List a. We can use the Applicative instance for List to lift this function over f, to get a function of type f
-- a -> f a -> f a -> f (List a). But, since we can do this for any n, it makes sense that we should be able
-- to perform the same lifting for any list of arguments.









-- • We introduced the concept of an applicative functor which generalizes the idea of function application
-- to type constructors which capture some notion of side-effect.
-- • We saw how applicative functors gave a solution to the problem of validating data structures, and how
-- by switching the applicative functor we could change from reporting a single error to reporting all
-- errors across a data structure.
-- • We met the Traversable type class, which encapsulates the idea of a traversable functor, or a container
-- whose elements can be used to combine values with side-effects.





-- Array comprehensions provide syntactic sugar for the concatMap function from the Data.Array module.












import Prelude

import Control.Plus (empty)
import Data.Array ((..))

countThrows :: Int -> Array (Array Int)
countThrows n = do
  x <- 1 .. 6
  y <- 1 .. 6
  if x + y == n
  then pure [x, y]
  else empty






-- In general, a monad for some type constructor m provides a way to use do notation with values of type m a.
-- Note that in the array comprehension above, every line contains a computation of type Array a for some type
-- a. In general, every line of a do notation block will contain a computation of type m a for some type a and our
-- monad m. The monad m must be the same on every line (i.e. we fix the side-effect), but the types a can differ
-- (i.e. individual computations can have different result types).











child :: XML -> String -> Maybe XML


userCity :: XML -> Maybe XML
userCity root = do
  prof <- child root "profile"
  addr <- child prof "address"
  city <- child addr "city"
  pure city








do value <- someComputation
  whatToDoNext

bind someComputation \value -> whatToDoNext

someComputation >>= \value -> whatToDoNext








userCity :: XML -> Maybe XML
userCity root =
  child root "profile" >>= \prof ->
    child prof "address" >>= \addr ->
      child addr "city" >>= \city ->
        pure city







-- monad laws

do
  x <- expr
  pure x

do
  x <- pure y
  next

c1 = do
  y <- do
    x <- m1
    m2
  m3

c2 = do
  x <- m1
  y <- m2
  m3

c3 = do
  x <- m1
  do
    y <- m2
    m3











foldl :: forall a b
       . (a -> b -> a)
      -> a
      -> List b
      -> a





foldM :: forall m a b
       . Monad m
      => (a -> b -> m a)
      -> a
      -> List b
      -> m a
foldM _ a Nil = pure a
foldM f a (b:bs) = do
  a' <- f a b
  foldM f a' bs




safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a / b)





import Data.List

foldM safeDivide 100 (fromFoldable [5, 2, 2])
-- (Just 5)

foldM safeDivide 100 (fromFoldable [2, 0, 4])
-- Nothing








-- Every instance of the Monad type class is also an instance of the Applicative type class, by virtue of the
-- superclass relationship between the two classes.

-- However, there is also an implementation of the Applicative type class which comes “for free” for any
-- instance of Monad, given by the ap function:

ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b
ap mf ma = do
  f <- mf
  a <- ma
  pure (f a)










map f a = do
  x <- a
  pure (f x)







lift2 :: forall f a b c. Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift2 f a b = f <$> a <*> b















module Main where

import Prelude

import Control.Monad.Eff.Random (random)
import Control.Monad.Eff.Console (logShow)

main = do
  n <- random
  logShow n







import Control.Monad.Eff

-- > :kind Eff
-- # Control.Monad.Eff.Effect -> Type -> Type


-- Control.Monad.Eff.Effect is the kind of effects, which represents type-level labels for different types
-- of side-effects. To understand this, note that the two labels we saw in main above both have kind Control.
-- Monad.Eff.Effect:


import Control.Monad.Eff.Console
import Control.Monad.Eff.Random

-- > :kind CONSOLE
-- Control.Monad.Eff.Effect

-- > :kind RANDOM
-- Control.Monad.Eff.Effect





-- The # kind constructor is used to construct kinds for rows, i.e. unordered, labelled sets.







-- We can now read the type of main above:

forall eff. Eff (console :: CONSOLE, random :: RANDOM | eff) Unit

-- The first argument to Eff is (console :: CONSOLE, random :: RANDOM | eff). This is a row which contains
-- the CONSOLE effect and the RANDOM effect. The pipe symbol | separates the labelled effects from the row variable
-- eff which represents any other side-effects we might want to mix in.
-- The second argument to Eff is Unit, which is the return type of the computation.














fullName :: forall r. { firstName :: String, lastName :: String | r } -> String
fullName person = person.firstName <> " " <> person.lastName

-- The kind of the type on the left of the function arrow must be Type, because only types of kind Type have
-- values.

-- The curly braces are actually syntactic sugar, and the full type as understood by the PureScript compiler is as
-- follows:

fullName :: forall r. Record (firstName :: String, lastName :: String | r) -> String


-- Record is a built-in type constructor defined in the Prim module.

-- > :kind Record
-- # Type -> Type

-- Record is a type constructor which takes a row of types and constructs a type. This is what allows us
-- to write row-polymorphic functions on records.







-- If we annotate the previous example with a closed row of effects:

main :: Eff (console :: CONSOLE, random :: RANDOM) Unit
main = do
  n <- random
  print n

-- (note the lack of the row variable eff here), then we cannot accidentally include a subcomputation which
-- makes use of a different type of effect. In this way, we can control the side-effects that our code is allowed to
-- have.










-- Functions such as print and random are called actions. Actions have the Eff type on the right hand side of
-- their functions, and their purpose is to introduce new effects.

-- This is in contrast to handlers, in which the Eff type appears as the type of a function argument. While actions
-- add to the set of required effects, a handler usually subtracts effects from the set.




-- Functions such as `print` and `random` are called _actions_. Actions have the `Eff` type on the right hand side of their functions, and their purpose is to _introduce_ new effects.

-- This is in contrast to _handlers_, in which the `Eff` type appears as the type of a function argument. While actions _add_ to the set of required effects, a handler usually _subtracts_ effects from the set.














newSTRef    :: forall a h eff. a -> Eff (st :: ST h | eff) (STRef h a)
readSTRef   :: forall a h eff. STRef h a -> Eff (st :: ST h | eff) a
writeSTRef  :: forall a h eff. STRef h a -> a -> Eff (st :: ST h | eff) a
modifySTRef :: forall a h eff. STRef h a -> (a -> a) -> Eff (st :: ST h | eff) a







import Prelude

import Control.Monad.Eff (Eff, forE)
import Control.Monad.ST (ST, newSTRef, readSTRef, modifySTRef)

simulate :: forall eff h. Number -> Number -> Int -> Eff (st :: ST h | eff) Number
simulate x0 v0 time = do
  ref <- newSTRef { x: x0, v: v0 }
  forE 0 (time * 1000) \_ -> do
    modifySTRef ref \o ->
      { v: o.v - 9.81 * 0.001
      , x: o.x + o.v * 0.001
      }
    pure unit
  final <- readSTRef ref
  pure final.x

-- Note that even though this function uses mutable state, it is still a pure function, so long as the reference cell
-- ref is not allowed to be used by other parts of the program.






runST :: forall a eff. (forall h. Eff (st :: ST h | eff) a) -> Eff eff a







simulate' :: Number -> Number -> Number -> Number
simulate' x0 v0 time = runPure (runST (simulate x0 v0 time))




import Main
simulate' 100.0 0.0 0.0
-- 100.00
simulate' 100.0 0.0 1.0
-- 95.10
simulate' 100.0 0.0 2.0
-- 80.39
simulate' 100.0 0.0 3.0
-- 55.87
simulate' 100.0 0.0 4.0
-- 21.54








-- If we inline the definition of simulate at the call to runST, as follows:

simulate :: Number -> Number -> Int -> Number
simulate x0 v0 time = runPure $ runST do
  ref <- newSTRef { x: x0, v: v0 }
  forE 0 (time * 1000) \_ -> do
    modifySTRef ref \o ->
      { v: o.v - 9.81 * 0.001
      , x: o.x + o.v * 0.001
      }
    pure unit
  final <- readSTRef ref
  pure final.x

-- then the compiler will notice that the reference cell is not allowed to escape its scope, and can safely turn it
-- into a var. Here is the generated JavaScript for the body of the call to runST:

var ref = { x: x0, v: v0 };

Control_Monad_Eff.forE(0)(time * 1000 | 0)(function (i) {
  return function __do() {
    ref = (function (o) {
      return {
        v: o.v - 9.81 * 1.0e-3,
        x: o.x + o.v * 1.0e-3
      };
    })(ref);
    return Prelude.unit;
  };
})();

return ref.x;







-- -----------------------------------------------------------------------------

-- Foreign Fuction Interface (FFI)


module Test where

gcd :: Int -> Int -> Int
gcd 0 m = m
gcd n 0 = n
gcd n m
  | n > m = gcd (n - m) m
  | otherwise = gcd (m - n) n


-- Usage in JavaScript (assuming that the code was compiled with `pulp build`,
-- which compiles PureScript modules to CommonJS modules):
--
-- ```js
-- const Test = require("Test")
-- Test.gcd(15)(20)
-- ```


-- You might also like to bundle JavaScript code for the browser, using `pulp build -O --to file.js`. In that
-- case, you would access the Test module from the global PureScript namespace, which defaults to PS:
--
-- ```js
-- const Test = PS.Test
-- Test.gcd(15)(20)
-- ```




-- PureScript aims to preserve names during code generation as much as possible. In particular, most identifiers
-- which are neither PureScript nor Javascript keywords can be expected to be preserved, at least for names of
-- top-level declarations.

-- If you decide to use a Javascript keyword as an identifier, the name will be escaped with a double dollar
-- symbol. For example,

-- null = []

-- generates the following Javascript:
-- var $$null = [];

-- In addition, if you would like to use special characters in your identifier names, they will be escaped using a
-- single dollar symbol. For example,

-- example' = 100

-- generates the following Javascript:

-- var example$prime = 100;

-- Where compiled PureScript code is intended to be called from JavaScript, it is recommended that identifiers
-- only use alphanumeric characters, and avoid JavaScript keywords. If user-defined operators are provided for
-- use in PureScript code, it is good practice to provide an alternative function with an alphanumeric name for
-- use in JavaScript.





-- For every constructor of an algebraic data type, the PureScript compiler creates a new JavaScript object type
-- by defining a function. Its constructors correspond to functions which create new JavaScript objects based on
-- those prototypes.

data ZeroOrOne a = Zero | One a

-- The PureScript compiler generates the following code:

-- ```js
-- function One(value0) {
--   this.value0 = value0;
-- };
--
-- One.create = function (value0) {
--   return new One(value0);
-- };
--
-- function Zero() {
-- };
--
-- Zero.value = new Zero();
-- ```

-- This algebraic data type:

data Two a b = Two a b

-- generates this JavaScript code:

-- ```js
-- function Two(value0, value1) {
--   this.value0 = value0;
--   this.value1 = value1;
-- };

-- Two.create = function (value0) {
--   return function (value1) {
--     return new Two(value0, value1);
--   };
-- };
-- ```

-- A newtype is like an algebraic data type, restricted to
-- having a single constructor taking a single argument.
















shout :: forall a. Show a => a -> String
shout a = show a <> "!!!"

-- The generated JavaScript looks like this:
--
-- ```js
-- var shout = function (dict) {
--   return function (a) {
--     return show(dict)(a) + "!!!";
--   };
-- };
-- ```
--
-- Notice that shout is compiled to a (curried) function of two arguments, not one. The first argument dict is
-- the type class dictionary for the Show constraint. dict contains the implementation of the show function for
-- the type a.
--
-- We can call this function from JavaScript by passing an explicit type class dictionary from the Prelude as the
-- first parameter:
--
-- ```js
-- shout(require('Prelude').showNumber)(42);
-- ```










-- Parametrically polymorphic: Expressions that cannot use any information about
-- the type of its argument in its implementation.













-- foreign import declaration

module Data.URI where

foreign import encodeURIComponent :: String -> String

-- We also need to write a foreign Javascript module. If the module above is saved as src/Data/URI.purs, then
-- the foreign Javascript module should be saved as src/Data/URI.js:

-- ```js
-- "use strict"
--
-- exports.encodeURIComponent = encodeURIComponent
-- ```

import Data.URI

encodeURIComponent "Hello World"
-- "Hello%20World"















-- function head(arr) {
--   return arr[0];
-- }

-- To keep things simple, we can throw an exception in the case of an empty array. Strictly speaking, pure
-- functions should not throw exceptions, but it will suffice for demonstration purposes, and we can indicate the
-- lack of safety in the function name:

foreign import unsafeHead :: forall a. Array a -> a

-- In our foreign Javascript module, we can define unsafeHead as follows:

-- exports.unsafeHead = function (arr) {
--   if (arr.length) {
--     return arr[0]
--   } else {
--     throw new Error("unsafeHead: empty array")
--   }
-- }

-- Suppose we wanted to define a new type Undefined a whose representation at runtime was like that for the
-- type a, but also allowing the undefined value.

-- We can define a foreign type using the FFI using a foreign type declaration. The syntax is similar to defining
-- a foreign function:

foreign import data Undefined :: Type -> Type

-- exports.head = function (arr) {
--   return arr[0]
-- }

foreign import head :: forall a. Array a -> Undefined a

foreign import isUndefined :: forall a. Undefined a -> Boolean

-- exports.isUndefined = function (value) {
--   return value === undefined
-- }

isEmpty :: forall a. Array a -> Boolean
isEmpty = isUndefined <<< head

-- This is good practice in general: foreign functions should
-- be kept as small as possible, and application logic moved into PureScript code wherever possible.





-- PureScript’s Prelude contains an interesting set of examples of foreign types. As we have covered already,
-- PureScript’s function types only take a single argument, and can be used to simulate functions of multiple
-- arguments via currying. This has certain advantages - we can partially apply functions, and give type class
-- instances for function types - but it comes with a performance penalty. For performance critical code, it is
-- sometimes necessary to define genuine JavaScript functions which accept multiple arguments. The Prelude
-- defines foreign types which allow us to work safely with such functions.

-- For example, the following foreign type declaration is taken from the Prelude in the Data.Function.Uncurried
-- module:

foreign import data Fn2 :: Type -> Type -> Type -> Type

-- We can create a function of two arguments by using the mkFn2 function, as follows:

import Data.Function.Uncurried

divides :: Fn2 Int Int Boolean
divides = mkFn2 \n m -> m % n == 0

-- and we can apply a function of two arguments by using the runFn2 function:

runFn2 divides 2 10
-- true

runFn2 divides 3 10
-- false

-- The key here is that the compiler inlines the mkFn2 and runFn2 functions whenever they are fully applied. The
-- result is that the generated code is very compact:

-- exports.divides = function(n, m) {
--   return m % n === 0;
-- };


-- The definition of the Eff type constructor is given in the Control.Monad.Eff module as follows:

foreign import data Eff :: # Effect -> Type -> Type

-- Recall that the Eff type constructor is parameterized by a row of effects and a return type, which is reflected
-- in its kind.

-- As a simple example, consider the random function defined in the purescript-random package. Recall that its
-- type was:

foreign import random :: forall eff. Eff (random :: RANDOM | eff) Number

-- The definition of the random function is given here:

-- exports.random = function() {
--   return Math.random();
-- };

-- Notice that the random function is represented at runtime as a function of no arguments. It performs the side
-- effect of generating a random number, and returns it, and the return value matches the runtime representation
-- of the Number type: it is a non-null JavaScript number.

-- As a slightly more interesting example, consider the log function defined by the Control.Monad.Eff.Console
-- module in the purescript-console package. The log function has the following type:

foreign import log :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit

-- And here is its definition:

-- exports.log = function (s) {
--   return function () {
--     console.log(s);
--   };
-- };

-- The effects RANDOM and CONSOLE are also defined as foreign types. Their kinds are defined to be Effect, the
-- kind of effects. For example:

foreign import data RANDOM :: Effect

-- In fact, it is possible to define new effects in this way, as we will soon see.

-- Expressions of type Eff eff a can be invoked from JavaScript like regular JavaScript methods. For example,
-- since the main function is required to have type Eff eff a for some set of effects eff and some type a, it can
-- be invoked as follows:

-- require('Main').main();







foreign import data ALERT :: Effect

foreign import alert :: forall eff. String -> Eff (alert :: ALERT | eff) Unit

-- exports.alert = function (msg) {
--   return function () {
--     window.alert(msg)
--   }
-- }






import Data.Foreign
import Data.Foreign.Generic
import Data.Foreign.JSON

parseJSON :: String -> F Foreign
decodeJSON :: forall a. Decode a => String -> F a

-- The type constructor F is actually just a type synonym, defined in Data.Foreign:

type F = Except (NonEmptyList ForeignError)






import Control.Monad.Except

runExcept (decodeJSON "\"Testing\"" :: F String)
-- Right "Testing"

runExcept (decodeJSON "true" :: F Boolean)
-- Right true

runExcept (decodeJSON "[1, 2, 3]" :: F (Array Int))
-- Right [1, 2, 3]

runExcept (decodeJSON "[1, 2, true]" :: F (Array Int))
-- (Left (NonEmptyList (NonEmpty (ErrorAtIndex 2 (TypeMismatch "Int" "Boolean")) Nil)))






import Prelude

import Data.Foreign.NullOrUndefined

runExcept (unNullOrUndefined <$> decodeJSON "42" :: F (NullOrUndefined Int))
-- (Right (Just 42))

runExcept (unNullOrUndefined <$> decodeJSON "null" :: F (NullOrUndefined Int))
-- (Right Nothing)

runExcept (map unNullOrUndefined <$> decodeJSON "[1, 2, null]" :: F (Array (NullOrUndefined Int)))
-- (Right [(Just 1),(Just 2),Nothing])











-- To derive a Decode instance for our FormData type (so that we may deserialize it from its JSON representation),
-- we first use the derive keyword to derive an instance of the Generic type class, which looks like this:

derive instance genericFormData :: Generic FormData _

-- Next, we simply define the decode function using the genericDecode function, as follows:

instance decodeFormData :: Decode FormData where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

-- In fact, we can also derive an encoder in the same way:

instance encodeFormData :: Encode FormData where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

-- It is important that we use the same options in the decoder and encoder, otherwise our encoded JSON
-- documents might not get decoded correctly.

loadSavedData = do
  item <- getItem "person"
  let savedData :: Either (NonEmptyList ForeignError) (Maybe FormData)
      savedData = runExcept do
        jsonOrNull <- traverse readString =<< readNullOrUndefined item
        traverse decodeJSON jsonOrNull








import Data.Foldable (traverse_)
import Control.Monad.State
import Control.Monad.State.Class

sumArray :: Array Number -> State Number Unit
sumArray = traverse_ \n -> modify \sum -> sum + n






-- The Control.Monad.State module provides the following API:

get :: forall s. State s s
put :: forall s. s -> State s Unit
modify :: forall s. (s -> s) -> State s Unit



-- The Control.Monad.State module provides three functions for running a computation in the State monad:

evalState :: forall s a. State s a -> s -> a
execState :: forall s a. State s a -> s -> s
runState :: forall s a. State s a -> s -> Tuple a s




execState
  (do
    sumArray [1, 2, 3]
    sumArray [4, 5]
    sumArray [6]
  )
  0
-- 21









-- The Control.Monad.Reader module provides the following API:

ask :: forall r. Reader r r
local :: forall r a. (r -> r) -> Reader r a -> Reader r a

-- The ask action can be used to read the current configuration, and the local action can be used to run a
-- computation with a modified configuration.

-- For example, suppose we were developing an application controlled by permissions, and we wanted to use
-- the Reader monad to hold the current user’s permissions object. We might choose the type r to be some type
-- Permissions with the following API:

hasPermission :: String -> Permissions -> Boolean
addPermission :: String -> Permissions -> Permissions


createUser :: Reader Permissions (Maybe User)
createUser = do
  permissions <- ask
  if hasPermission "admin" permissions
  then map Just newUser
  else pure Nothing

-- To elevate the user’s permissions, we might use the local action to modify the Permissions object during
-- the execution of some computation:

runAsAdmin :: forall a. Reader Permissions a -> Reader Permissions a
runAsAdmin = local (addPermission "admin")

createUserAsAdmin :: Reader Permissions (Maybe User)
createUserAsAdmin = runAsAdmin createUser

-- To run a computation in the Reader monad, the runReader function can be used to provide the global
-- configuration:

runReader :: forall r a. Reader r a -> r -> a







gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m
          then gcd (n - m) m
          else gcd n (m - n)



import Control.Monad.Writer
import Control.Monad.Writer.Class

gcdLog :: Int -> Int -> Writer (Array String) Int
gcdLog n 0 = pure n
gcdLog 0 m = pure m
gcdLog n m = do
  tell ["gcdLog " <> show n <> " " <> show m]
  if n > m
  then gcdLog (n - m) m
  else gcdLog n (m - n)



execWriter :: forall w a. Writer w a -> w
runWriter :: forall w a. Writer w a -> Tuple a w



import Control.Monad.Writer
import Control.Monad.Writer.Class

runWriter (gcdLog 21 15)
-- Tuple 3 ["gcdLog 21 15","gcdLog 6 15","gcdLog 6 9","gcdLog 6 3","gcdLog 3 3"]





import Control.Monad.State.Trans

-- > :kind StateT
-- Type -> (Type -> Type) -> Type -> Type

-- The first type argument is the type of the state we wish to use, as was the case for State. Let’s use a state of
-- type String:

-- > :kind StateT String
-- (Type -> Type) -> Type -> Type

-- The next argument is a type constructor of kind Type -> Type. It represents the underlying monad, which
-- we want to add the effects of StateT to. For the sake of an example, let’s choose the Either String monad:

-- > :kind StateT String (Either String)
-- Type -> Type

-- We are left with a type constructor. The final argument represents the return type, and we might instantiate
-- it to Number for example:

-- > :kind StateT String (Either String) Number
-- Type




import Control.Monad.Trans

class MonadTrans t where
  lift :: forall m a. Monad m => m a -> t m a




-- The following computation reads the underlying state, and then throws an error if the state is
-- the empty string:

import Data.String (drop, take)

split :: StateT String (Either String) String
split = do
  s <- get
  case s of
    "" -> lift $ Left "Empty string"
    _ -> do
      put (drop 1 s)
      pure (take 1 s)


runStateT split "test"
-- Right (Tuple "t" "est")

runStateT split ""
-- Left "Empty string"

runStateT ((<>) <$> split <*> split) "test"
-- (Right (Tuple "te" "st"))




class MonadError e m where
  throwError :: forall a. e -> m a
  catchError :: forall a. m a -> (e -> m a) -> m a

instance monadErrorExceptT :: Monad m => MonadError e (ExceptT e m)

runExceptT :: forall e m a. ExceptT e m a -> m (Either e a)





import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Control.Monad.Error.Class
import Control.Monad.Except.Trans

writerAndExceptT :: ExceptT String (Writer (Array String)) String
writerAndExceptT = do
  lift $ tell ["Before the error"]
  throwError "Error!"
  lift $ tell ["After the error"]
  pure "Return value"

-- If we test this function in PSCi, we can see how the two effects of accumulating a log and throwing an error
-- interact. First, we can run the outer ExceptT computation of type by using runExceptT, leaving a result of type
-- Writer String (Either String String). We can then use runWriter to run the inner Writer computation:

runWriter $ runExceptT writerAndExceptT
-- Tuple (Left "Error!") ["Before the error"]

-- Note that only those log messages which were written before the error was thrown actually get appended to
-- the log.



-- As we have seen, monad transformers can be used to build new monads on top of existing monads. For some
-- monad transformer t1 and some monad m, the application t1 m is also a monad. That means that we can
-- apply a second monad transformer t2 to the result t1 m to construct a third monad t2 (t1 m). In this way,
-- we can construct a stack of monad transformers, which combine the side-effects provided by their constituent
-- monads.

-- Usage of a monad transformer stack:

type Errors = Array String
type Log = Array String
type Parser = StateT String (WriterT Log (ExceptT Errors Identity))

split :: Parser String
split = do
  s <- get
  lift $ tell ["The state is " <> show s]
  case s of
    "" -> lift $ lift $ throwError ["Empty string"]
    _ -> do
      put (drop 1 s)
      pure (take 1 s)


runParser p s = runIdentity $ runExceptT $ runWriterT $ runStateT p s


runParser split "test"
-- (Right (Tuple (Tuple "t" "est") ["The state is test"]))

runParser ((<>) <$> split <*> split) "test"
-- (Right (Tuple (Tuple "te" "st") ["The state is test", "The state is est"]))

runParser split ""
-- (Left ["Empty string"])






-- When we looked at the State monad at the start of this chapter, I gave the following types for the actions of
-- the State monad:

get :: forall s. State s s
put :: forall s. s -> State s Unit
modify :: forall s. (s -> s) -> State s Unit

-- In reality, the types given in the Control.Monad.State.Class module are more general than this:

get :: forall m s. MonadState s m => m s
put :: forall m s. MonadState s m => s -> m Unit
modify :: forall m s. MonadState s m => (s -> s) -> m Unit







-- In the case of the split function above, the monad stack we constructed is an instance of each of the
-- MonadState, MonadWriter and MonadError type classes. This means that we don’t need to call lift at all!
-- We can just use the actions get, put, tell and throwError as if they were defined on the monad stack itself:

split :: Parser String
split = do
  s <- get
  tell ["The state is " <> show s]
  case s of
    "" -> throwError "Empty string"
    _ -> do
      put (drop 1 s)
      pure (take 1 s)







-- The purescript-control package defines a number of abstractions for working with computations which
-- can fail. One of these is the Alternative type class:

class Functor f <= Alt f where
  alt :: forall a. f a -> f a -> f a

class Alt f <= Plus f where
  empty :: forall a. f a

class (Applicative f, Plus f) <= Alternative f



-- The Data.List module provides two useful functions for working with type constructors in the Alternative
-- type class:

many :: forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)
some :: forall f a. Alternative f => Lazy (f (List a)) => f a -> f (List a)



import Split
import Control.Alternative

runParser (many split) "test"
-- (Right (Tuple (Tuple ["t", "e", "s", "t"] "")
--               [ "The state is \"test\""
--               , "The state is \"est\""
--               , "The state is \"st\""
--               , "The state is \"t\""
--               ]))





-- The Control.MonadPlus module defines a subclass of the Alternative type class, called MonadPlus. MonadPlus
-- captures those type constructors which are both monads and instances of Alternative:

class (Monad m, Alternative m) <= MonadZero m

class MonadZero m <= MonadPlus m



guard :: forall m. MonadZero m => Boolean -> m Unit




upper :: Parser String
upper = do
  s <- split
  guard $ toUpper s == s
  pure s




lower :: Parser String
lower = do
  s <- split
  guard $ toLower s == s
  pure s




upperOrLower = some upper <|> some lower

-- This parser will match characters until the case changes:

runParser upperOrLower "abcDEF"
-- (Right (Tuple (Tuple ["a","b","c"] ("DEF"))
--               [ "The state is \"abcDEF\""
--               , "The state is \"bcDEF\""
--               , "The state is \"cDEF\""
--               ]))




components = many upperOrLower

runParser components "abCDeFgh"
-- (Right (Tuple (Tuple [["a","b"],["C","D"],["e"],["F"],["g","h"]] "")
--               [ "The state is \"abCDeFgh\""
--               , "The state is \"bCDeFgh\""
--               , "The state is \"CDeFgh\""
--               , "The state is \"DeFgh\""
--               , "The state is \"eFgh\""
--               , "The state is \"Fgh\""
--               , "The state is \"gh\""
--               , "The state is \"h\""
--               ]))





type RWS r w s = RWST r w s Identity






type PlayerName = String

newtype GameEnvironment = GameEnvironment
  { playerName :: PlayerName
  , debugMode :: Boolean
  }




import qualified Data.Map as M
import qualified Data.Set as S

newtype GameState = GameState
  { items :: M.Map Coords (S.Set GameItem)
  , player :: Coords
  , inventory :: S.Set GameItem
  }



data GameItem = Candle | Matches




type Log = L.List String

type Game = RWS GameEnvironment Log GameState




has :: GameItem -> Game Boolean
has item = do
  GameState state <- get
  pure $ item `S.member` state.inventory


pickUp :: GameItem -> Game Unit
pickUp item = do
  GameState state <- get
  case state.player `M.lookup` state.items of
    Just items | item `S.member` items -> do
      let newItems = M.update (Just <<< S.delete item) state.player state.items
          newInventory = S.insert item state.inventory
      put $ GameState state { items = newItems
                            , inventory = newInventory
                            }
      tell (L.singleton ("You now have the " <> show item))
    _ -> tell (L.singleton "I don't see that item here.")

  GameEnvironment env <- ask
  if env.debugMode
  then do
    state <- get
    tell (L.singleton (show state))
  else tell (L.singleton "Not running in debug mode.")



game :: Array String -> Game Unit


data RWSResult state result writer = RWSResult state result writer

runRWS :: forall r w s a. RWS r w s a -> r -> s -> RWSResult s a w



runGame
  :: forall eff
   . GameEnvironment
  -> Eff ( exception :: EXCEPTION
         , readline  :: RL.READLINE
         , console   :: CONSOLE
         | eff
         ) Unit



type LineHandler eff a = String -> Eff eff a

setLineHandler
  :: forall eff a
   . Interface
  -> LineHandler (readline :: READLINE | eff) a
  -> Eff (readline :: READLINE | eff) Unit



runGame env = do
  interface <- createConsoleInterface noCompletion
  setPrompt "> " 2 interface
  setLineHandler interface $ lineHandler initialGameState
  prompt interface




lineHandler
  :: GameState
  -> String
  -> Eff ( exception :: EXCEPTION
         , console   :: CONSOLE
         , readline  :: RL.READLINE
         | eff
         ) Unit
lineHandler currentState input = do
  case runRWS (game (split " " input)) env currentState of
    RWSResult state _ written -> do
      for_ written log
      setLineHandler interface $ lineHandler state
  prompt interface
  pure unit




runY
  :: forall a eff
   . YargsSetup
  -> Y (Eff (exception :: EXCEPTION, console :: CONSOLE | eff) a)
  -> Eff (exception :: EXCEPTION, console :: CONSOLE | eff) a


main = runY (usage "$0 -p <player name>") $ map runGame env
  where
    env :: Y GameEnvironment
    env = gameEnvironment
      <$> yarg "p" ["player"]
            (Just "Player name")
            (Right "The player name is required")
            false
      <*> flag "d" ["debug"]
            (Just "Use debug mode")










foreign import data FS :: Effect

type ErrorCode = String
type FilePath  = String





foreign import readFileImpl
  :: forall eff
   . Fn3 FilePath
         (String -> Eff (fs :: FS | eff) Unit)
         (ErrorCode -> Eff (fs :: FS | eff) Unit)
         (Eff (fs :: FS | eff) Unit)


-- exports.readFileImpl = function (path, onSuccess, onFailure) {
--   return function () {
--     require("fs").readFile(path, { encoding: "utf-8" }, function (error, data) {
--       if (error) {
--         onFailure(error.code)()
--       } else {
--         onSuccess(data)()
--       }
--     })
--   }
-- }







foreign import writeFileImpl
  :: forall eff
   . Fn4 FilePath
         String
         (Eff (fs :: FS | eff) Unit)
         (ErrorCode -> Eff (fs :: FS | eff) Unit)
         (Eff (fs :: FS | eff) Unit)

-- exports.writeFileImpl = function (path, data, onSuccess, onFailure) {
--   return function () {
--     require("fs").writeFile(path, data, { encoding: "utf-8" }, function (error) {
--       if (error) {
--         onFailure(error.code)()
--       } else {
--         onSuccess()
--       }
--     })
--   }
-- }





readFile
  :: forall eff
   . FilePath
  -> (Either ErrorCode String -> Eff (fs :: FS | eff) Unit)
  -> Eff (fs :: FS | eff) Unit
readFile path k = runFn3 readFileImpl path (k <<< Right) (k <<< Left)


writeFile
  :: forall eff
   . FilePath
  -> String
  -> (Either ErrorCode Unit -> Eff (fs :: FS | eff) Unit)
  -> Eff (fs :: FS | eff) Unit
writeFile path text k = runFn4 writeFileImpl path text (k $ Right unit) (k <<< Left)





newtype ContT r m a = ContT ((a -> m r) -> m r)




-- A continuation is just another name for a callback.
-- A continuation captures the remainder of a computation.




type Async eff = ContT Unit (Eff eff)





readFileCont :: forall eff. FilePath -> Async (fs :: FS | eff) (Either ErrorCode String)
readFileCont path = ContT $ readFile path


writeFileCont :: forall eff . FilePath -> String -> Async (fs :: FS | eff) (Either ErrorCode Unit)
writeFileCont path text = ContT $ writeFile path text


-- With that, we can write our copy-file routine by simply using do notation for the ContT monad transformer:

copyFileCont :: forall eff . FilePath -> FilePath -> Async (fs :: FS | eff) (Either ErrorCode Unit)
copyFileCont src dest = do
  e <- readFileCont src
  case e of
    Left err      -> pure $ Left err
    Right content -> writeFileCont dest content

-- Note how the asynchronous nature of readFileCont is hidden by the monadic bind expressed using do
-- notation - this looks just like synchronous code, but the ContT monad is taking care of wiring our asynchronous
-- functions together.




import Prelude
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Cont.Trans (runContT)

main = runContT (copyFileCont "/tmp/1.txt" "/tmp/2.txt") logShow





type Milliseconds = Int

foreign import data TIMEOUT :: Effect

setTimeoutCont :: forall eff. Milliseconds -> Async (timeout :: TIMEOUT | eff) Unit





newtype ExceptT e m a = ExceptT (m (Either e a))






readFileContEx :: forall eff. FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) String
readFileContEx path = ExceptT $ readFileCont path

writeFileContEx :: forall eff. FilePath -> String -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
writeFileContEx path text = ExceptT $ writeFileCont path text






copyFileContEx :: forall eff. FilePath -> FilePath -> ExceptT ErrorCode (Async (fs :: FS | eff)) Unit
copyFileContEx src dest = do
  content <- readFileContEx src
  writeFileContEx dest content










foreign import data HTTP :: Effect

type URI = String

foreign import getImpl
  :: forall eff
   . Fn3 URI
         (String -> Eff (http :: HTTP | eff) Unit)
         (String -> Eff (http :: HTTP | eff) Unit)
         (Eff (http :: HTTP | eff) Unit)

-- exports.getImpl = function(uri, done, fail) {
--   return function() {
--     require('request')(uri, function(err, _, body) {
--       if (err) {
--         fail(err)();
--       } else {
--         done(body)();
--       }
--     });
--   };
-- };






get :: forall eff. URI -> Async (http :: HTTP | eff) (Either String String)
get req = ContT \k -> runFn3 getImpl req (k <<< Right) (k <<< Left)








class (Monad m, Applicative f) <= Parallel f m | m -> f, f -> m where
  sequential :: forall a. f a -> m a
  parallel :: forall a. m a -> f a








import Prelude
import Control.Apply (lift2)
import Control.Monad.Cont.Trans (runContT)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Parallel (parallel, sequential)

main = flip runContT logShow do
  sequential $
    lift2 append
      <$> parallel (readFileCont "/tmp/1.txt")
      <*> parallel (readFileCont "/tmp/2.txt")













merge :: Array Int -> Array Int -> Array Int


import Merge

merge [1, 3, 5] [2, 4, 6]
-- [1, 2, 3, 4, 5, 6]




main = do
  quickCheck \xs ys -> isSorted $ merge (sort xs) (sort ys)
  quickCheck \xs ys -> xs `isSubarrayOf` merge xs ys



isSorted :: forall a. Ord a => Array a -> Boolean
isSubarrayOf :: forall a. Eq a => Array a -> Array a -> Boolean







mergePoly :: forall a. Ord a => Array a -> Array a -> Array a










ints :: Array Int -> Array Int
ints = id

quickCheck \xs ys -> isSorted $ ints $ mergePoly (sort xs) (sort ys)
quickCheck \xs ys -> ints xs `isSubarrayOf` mergePoly xs ys

-- Here, xs and ys both have type Array Int, since the ints function has been used to disambiguate the unknown type.






class Arbitrary t where
  arbitrary :: Gen t


newtype Byte = Byte Int

instance arbitraryByte :: Arbitrary Byte where
  arbitrary = map intToByte arbitrary
  where intToByte n | n >= 0    = Byte (n `mod` 256)
                    | otherwise = intToByte (-n)




newtype Sorted a = Sorted (Array a)

sorted :: forall a. Sorted a -> Array a
sorted (Sorted xs) = xs

instance arbSorted :: (Arbitrary a, Ord a) => Arbitrary (Sorted a) where
  arbitrary = map (Sorted <<< sort) arbitrary






data Tree a = Leaf | Branch (Tree a) a (Tree a)

-- The Tree module defines the following API:

insert :: forall a. Ord a => a -> Tree a -> Tree a
member :: forall a. Ord a => a -> Tree a -> Boolean
fromArray :: forall a. Ord a => Array a -> Tree a
toArray :: forall a. Tree a -> Array a




import Tree

member 2 $ insert 1 $ insert 2 Leaf
-- true

member 1 Leaf
-- false




instance arbTree :: (Arbitrary a, Ord a) => Arbitrary (Tree a) where
  arbitrary = map fromArray arbitrary





quickCheck \t a -> member a $ insert a $ treeOfInt t






import Data.String

mergeWith length
          ["", "ab", "abcd"]
          ["x", "xyz"]

-- ["","x","ab","xyz","abcd"]

-- How might we test such a function? Ideally, we would like to generate values for all three arguments, including
-- the first argument which is a function.

-- There is a second type class which allows us to create randomly-generated functions. It is called Coarbitrary,
-- and it is defined as follows:

class Coarbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r



instance arbFunction :: (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b)






-- In the case of the sortedness property, we cannot guarantee that the result will be sorted - we do not even
-- necessarily have an Ord instance - but we can expect that the result be sorted with respect to the function f
-- that we pass in as an argument. In addition, we need the two input arrays to be sorted with respect to f, so
-- we use the sortBy function to sort xs and ys based on comparison after the function f has been applied:

quickCheck \xs ys f -> isSorted $ map f $ mergeWith (intToBool f)
                                                    (sortBy (compare `on` f) xs)
                                                    (sortBy (compare `on` f) ys)

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = id



-- In the case of the subarray property, we simply have to change the name of the function to mergeWith - we
-- still expect our input arrays to be subarrays of the result:

quickCheck \xs ys f ->
  xs `isSubarrayOf` mergeWith (numberToBool f) xs ys


-- In addition to being Arbitrary, functions are also Coarbitrary:

instance coarbFunction :: (Arbitrary a, Coarbitrary b) => Coarbitrary (a -> b)

-- This means that we are not limited to just values and functions - we can also randomly generate higher-order
-- functions, or functions whose arguments are higher-order functions, and so on.




-- Just as we can write Arbitrary instances for our data types by using the Monad and Applicative instances
-- of Gen, we can write our own Coarbitrary instances as well. This allows us to use our own data types as the
-- domain of randomly-generated functions.



instance coarbTree :: Coarbitrary a => Coarbitrary (Tree a) where
  coarbitrary Leaf = id
  coarbitrary (Branch l a r) = coarbitrary l <<< coarbitrary a <<< coarbitrary r




anywhere :: forall a. (Tree a -> Boolean) -> Tree a -> Boolean



quickCheck \f g t -> anywhere (\s -> f s || g s) t == anywhere f (treeOfInt t) || anywhere g t

-- Here, the treeOfInt function is used to fix the type of values contained in the tree to the type Int:

treeOfInt :: Tree Int -> Tree Int
treeOfInt = id














import Prelude
import Merge
import Test.QuickCheck
import Test.QuickCheck.LCG (mkSeed)

quickCheckPure (mkSeed 12345) 10 \xs ys zs ->
  ((xs `merge` ys) `merge` zs) == (xs `merge` (ys `merge` zs))










newtype Element = Element
  { name    :: String
  , attribs :: Array Attribute
  , content :: Maybe (Array Content)
  }

data Content = TextContent String | ElementContent Element

newtype Attribute = Attribute
  { key   :: String
  , value :: String
  }



render :: Element -> String



import Prelude
import Data.DOM.Simple
import Data.Maybe
import Control.Monad.Eff.Console

log $ render $ Element
  { name: "p"
  , attribs: [Attribute { key: "class", value: "main" }]
  , content: Just [TextContent "Hello World!"]
  }
-- <p class="main">Hello World!</p>
-- unit



element :: String -> Array Attribute -> Maybe (Array Content) -> Element
element name attribs content = Element
  { name: name
  , attribs: attribs
  , content: content
  }

a :: Array Attribute -> Array Content -> Element
a attribs content = element "a" attribs (Just content)

p :: Array Attribute -> Array Content -> Element
p attribs content = element "p" attribs (Just content)

img :: Array Attribute -> Element
img attribs = element "img" attribs Nothing




module Data.DOM.Smart
  ( Element
  , Attribute(..)
  , Content
  , a
  , p
  , img
  , render
  ) where

-- The module exports list is provided immediately after the module name inside parentheses. Each module
-- export can be one of three types:
--
-- • A value (or function), indicated by the name of the value,
-- • A type class, indicated by the name of the class,
-- • A type constructor and any associated data constructors, indicated by the name of the type followed
--   by a parenthesized list of exported data constructors.



text :: String -> Content
text = TextContent

elem :: Element -> Content
elem = ElementContent





attribute :: String -> String -> Attribute
attribute key value = Attribute
  { key: key
  , value: value
  }

infix 4 attribute as :=

-- This representation suffers from the same problem as the original Element type - it is possible to represent
-- attributes which do not exist or whose names were entered incorrectly. To solve this problem, we can create
-- a newtype which represents attribute names:

newtype AttributeKey = AttributeKey String

-- With that, we can modify our operator as follows:

attribute :: AttributeKey -> String -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: value
  }

-- If we do not export the AttributeKey data constructor, then the user has no way to construct values of type
-- AttributeKey other than by using functions we explicitly export. Here are some examples:

href :: AttributeKey
href = AttributeKey "href"

_class :: AttributeKey
_class = AttributeKey "class"

src :: AttributeKey
src = AttributeKey "src"

width :: AttributeKey
width = AttributeKey "width"

height :: AttributeKey
height = AttributeKey "height"







module Data.DOM.Smart
  ( Element
  , Attribute
  , Content
  , AttributeKey
  , a
  , p
  , img
  , href
  , _class
  , src
  , width
  , height
  , attribute, (:=)
  , text
  , elem
  , render
  ) where


import Prelude
import Data.DOM.Smart
import Control.Monad.Eff.Console

log $ render $ p [ _class := "main" ] [ text "Hello World!" ]
-- <p class="main">Hello World!</p>
-- unit

log $ render $ Element
  { name: "p"
  , attribs: [Attribute { key: "class", value: "main" }]
  , content: Just [TextContent "Hello World!"]
  }







-- To motivate the next technique, consider the following code:

log $ render $ img
  [ src := "cat.jpg"
  , width := "foo"
  , height := "bar"
  ]
-- <img src="cat.jpg" width="foo" height="bar" />
-- unit

-- The problem here is that we have provided string values for the width and height attributes, where we should
-- only be allowed to provide numeric values in units of pixels or percentage points.

-- To solve this problem, we can introduce a so-called phantom type argument to our AttributeKey type:

newtype AttributeKey a = AttributeKey String

-- The type variable a is called a phantom type because there are no values of type a involved in the right-hand
-- side of the definition. The type a only exists to provide more information at compile-time. Any value of type
-- AttributeKey a is simply a string at runtime, but at compile-time, the type of the value tells us the desired
-- type of the values associated with this key.



attribute :: forall a. IsValue a => AttributeKey a -> a -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: toValue value
  }




class IsValue a where
  toValue :: a -> String

instance stringIsValue :: IsValue String where
  toValue = id

instance intIsValue :: IsValue Int where
  toValue = show



href :: AttributeKey String
href = AttributeKey "href"

_class :: AttributeKey String
_class = AttributeKey "class"

src :: AttributeKey String
src = AttributeKey "src"

width :: AttributeKey Int
width = AttributeKey "width"

height :: AttributeKey Int
height = AttributeKey "height"



import Prelude
import Data.DOM.Phantom
import Control.Monad.Eff.Console

log $ render $ img
  [ src := "cat.jpg"
  , width := 100
  , height := 200
  ]

-- <img src="cat.jpg" width="100" height="200" />
-- unit





-- In our final set of modifications to our API, we will use a construction called the free monad to turn our
-- Content type into a monad, enabling do notation. This will allow us to structure our HTML documents in a
-- form in which the nesting of elements becomes clearer - instead of this:

p [ _class := "main" ]
  [ elem $ img
    [ src := "cat.jpg"
    , width := 100
    , height := 200
    ]
    , text "A cat"
  ]

-- we will be able to write this:

p [ _class := "main" ] $ do
  elem $ img
    [ src := "cat.jpg"
    , width := 100
    , height := 200
    ]
  text "A cat"

-- However, do notation is not the only benefit of a free monad. The free monad allows us to separate the
-- representation of our monadic actions from their interpretation, and even support multiple interpretations of
-- the same actions.






import Control.Monad.Free

-- > :kind Free
-- (Type -> Type) -> Type -> Type

-- The kind of Free indicates that it takes a type constructor as an argument, and returns another type
-- constructor. In fact, the Free monad can be used to turn any Functor into a Monad!

-- We begin by defining the representation of our monadic actions. To do this, we need to create a Functor with
-- one data constructor for each monadic action we wish to support. In our case, our two monadic actions will
-- be elem and text. In fact, we can simply modify our Content type as follows:

data ContentF a
  = TextContent String a
  | ElementContent Element a

instance functorContentF :: Functor ContentF where
  map f (TextContent s x) = TextContent s (f x)
  map f (ElementContent e x) = ElementContent e (f x)



type Content = Free ContentF



newtype Element = Element
  { name :: String
  , attribs :: Array Attribute
  , content :: Maybe (Content Unit)
  }



liftF :: forall f a. f a -> Free f a



text :: String -> Content Unit
text s = liftF $ TextContent s unit

elem :: Element -> Content Unit
elem e = liftF $ ElementContent e unit




-- The Control.Monad.Free module provides a number of functions for interpreting a computation in a free
-- monad:

runFree
  :: forall f a
   . Functor f
  => (f (Free f a) -> Free f a)
  -> Free f a
  -> a

runFreeM
  :: forall f m a
   . (Functor f, MonadRec m)
  => (f (Free f a) -> m (Free f a))
  -> Free f a
  -> m a



render :: Element -> String
render = execWriter <<< renderElement
  where
    renderElement :: Element -> Writer String Unit
    renderElement (Element e) = do
      tell "<"
      tell e.name
      for_ e.attribs $ \x -> do
        tell " "
        renderAttribute x
      renderContent e.content

      where

        renderAttribute :: Attribute -> Writer String Unit
        renderAttribute (Attribute x) = do
          tell x.key
          tell "=\""
          tell x.value
          tell "\""

        renderContent :: Maybe (Content Unit) -> Writer String Unit
        renderContent Nothing = tell " />"
        renderContent (Just content) = do
          tell ">"
          runFreeM renderContentItem content
          tell "</"
          tell e.name
          tell ">"

        renderContentItem :: ContentF (Content Unit) -> Writer String (Content Unit)
        renderContentItem (TextContent s rest) = do
          tell s
          pure rest
        renderContentItem (ElementContent e rest) = do
          renderElement e
          pure rest







import Prelude
import Data.DOM.Free
import Control.Monad.Eff.Console

log $ render $ p [] $ do
  elem $ img [ src := "cat.jpg" ]
  text "A cat"

-- <p><img src="cat.jpg" />A cat</p>
-- unit






newtype Name = Name String

instance nameIsValue :: IsValue Name where
  toValue (Name n) = n

data Href
  = URLHref String
  | AnchorHref Name

instance hrefIsValue :: IsValue Href where
  toValue (URLHref url) = url
  toValue (AnchorHref (Name nm)) = "#" <> nm

runName :: Name -> String
runName (Name n) = n

href :: AttributeKey Href
href = AttributeKey "href"

name :: AttributeKey Name
name = AttributeKey "name"




data ContentF a
  = TextContent String a
  | ElementContent Element a
  | NewName (Name -> a)



instance functorContentF :: Functor ContentF where
  map f (TextContent s x) = TextContent s (f x)
  map f (ElementContent e x) = ElementContent e (f x)
  map f (NewName k) = NewName (f <<< k)




newName :: Content Name
newName = liftF $ NewName id



type Interp = WriterT String (State Int)



render :: Element -> String
render e = evalState (execWriterT (renderElement e)) 0





renderContentItem (NewName k) = do
  n <- get
  let fresh = Name $ "name" <> show n
  put $ n + 1
  pure (k fresh)






import Prelude
import Data.DOM.Name
import Control.Monad.Eff.Console

render $ p [ ] $ do
  top <- newName
  elem $ a [name := top] $ text "Top"
  elem $ a [href := AnchorHref top] $ text "Back to top"

-- <p><a name="name0">Top</a><a href="#name0">Back to top</a></p>
-- unit
