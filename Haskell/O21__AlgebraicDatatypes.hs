module O21__AlgebraicDatatypes () where





-- A data declaration relating a type constant with its sole constant value.

data Trivial = Trivial'

x1 :: Trivial
x1 = Trivial'

-- A "phantom" type which is a parameterized type constructor whose type
-- variables are not all used in any of its data constructors.

data Phantom a = Blah
-- The `a` type variable is "phantom" meaning it "has no witness".

x2 :: Phantom Int
x2 = Blah

x2' :: Phantom [[[Char]]]
x2' = Blah

-- A type constructor taking a parametrically polymorphic type variable which is
-- used by its sole data constructor.

data UnaryTypeCon a = UnaryValueCon a

x3 :: UnaryTypeCon Int
x3 = UnaryValueCon 10

-- Type constructors and data constructors can have the same name because they
-- operate on different levels.

-- Type variables follow the same naming rules as term-level variables.

-- Type variables on both sides of a data declaration refer to the same thing.

data Foo bar' = Foo bar'

x4 :: Foo String
x4 = Foo "hi"

{-
x4' :: Foo String
x4' = Foo 10    -- Causes a compile-time error.
-}

{-
data Baz = Baz a    -- Causes a compile-time error.
-}













data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Integer deriving (Eq, Show)

myCar = Car Mini $ Price 14000
urCar = Car Mazda $ Price 20000
clownCar = Car Tata $ Price 7000
doge = Plane PapuAir 10

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x
getManu _         = error "Nothing to do for a plane!"














-- A "kind" is a type of a type.

-- The kind of a concrete type is the primitive type `*`.

{- GHCi ------------------------------------------------------------------------
> :kind Int
Int :: *

> :k Int
Int :: *
-------------------------------------------------------------------------------}

-- The kind signature of `Int` is `*`.

-- The aforementioned can be read as "the Int type has the kind of star".

-- -----------------------------------------------------------------------------

-- The kind of a parameterized type constructor is `* -> *`.

{- GHCi ------------------------------------------------------------------------
> :k Maybe
Maybe :: * -> *

> :k (->)
(->) :: * -> * -> *

> :k (->) *
(->) * :: * -> *

> :k (->) * *
(->) * * :: *
-------------------------------------------------------------------------------}

-- The kind of the primitive type `*` is itself.

{- GHCi ------------------------------------------------------------------------
> :k *
* :: *
-------------------------------------------------------------------------------}

{- GHCi ------------------------------------------------------------------------
> :k * -> *
* -> * :: *

> :k (* -> *)
* -> * :: *

> :k Maybe *
Maybe * :: *

> :k Either
Either :: * -> * -> *

> :k Either *
Either * :: * -> *

> :k Either * *
Either * * :: *

> :k Int *    -- Causes a compile-time error.
-------------------------------------------------------------------------------}





data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

-- `Doggies` needs to be applied to become a concrete type.

{- GHCi ------------------------------------------------------------------------
> :k Doggies
Doggies :: * -> *
-------------------------------------------------------------------------------}

-- `Husky` needs to be applied to become a concrete value.

{- GHCi ------------------------------------------------------------------------
> :t Husky
Husky :: a -> Doggies a
-------------------------------------------------------------------------------}






-- A higher-kinded type is a kind that must be applied before it can be a type.



data Silly a b c d = MkSilly a b c d deriving Show

{- GHCi ------------------------------------------------------------------------
> :kind Silly
Silly :: * -> * -> * -> * -> *

> :kind Silly Int
Silly Int :: * -> * -> * -> *

> :kind Silly Int String
Silly Int String :: * -> * -> *

> :kind Silly Int String Bool
Silly Int String Bool :: * -> *

> :kind Silly Int String Bool String
Silly Int String Bool String :: *

> :kind (,,,)
(,,,) :: * -> * -> * -> * -> *

> :kind (Int, String, Bool, String)
(Int, String, Bool, String) :: *
-------------------------------------------------------------------------------}








-- -----------------------------------------------------------------------------

-- Constructors that don't take arguments behave like constants.

-- Constructors that take arguments behave like functions that don't do anything
-- but get applied.

-- -----------------------------------------------------------------------------





-- Nullary data constructors, such as True and False, are constant values at the
-- term level and, since they have no arguments, they can't construct or
-- represent any data other than themselves.










-- nullary
data Example0 = Example0 deriving (Eq, Show)

-- unary
data Example1 = Example1 Int deriving (Eq, Show)

-- product of Int and String
data Example2 = Example2 Int String deriving (Eq, Show)








-- A tuple is an example of an "anonymous product" in that it is a product value
-- with no name.








-- Datatypes that only contain a unary data constructor always have the same
-- cardinality as the type they contain.  For cardinality, this means unary
-- data constructors are the identity function.











-- Left lesser, right greater is a common convention for arranging binary trees
-- - it could be the opposite and not really change anything, but this matches
-- our usual intuitions of ordering as we do with, say, number lines.




data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)




mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay = if mapTree (+1) testTree' == mapExpected
          then print "yup okay!"
          else error "test failed!"




























-- The `newtype` keyword defines types that can only ever have a single unary
-- data constructor.

-- A type defined with `newtype` instead of `data` has no runtime overhead.




newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42







-- One key contrast between a `newtype` and a `type` alias is that you can
-- define typeclass instances for newtypes that differ from the instances
-- for their underlying type.  You can't do that for type synonyms.



class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 43







-- A product type's cardinality is the product of the cardinalities of its
-- inhabitants.  Arithmetically, products are the result of
-- multiplication.  Where a sum type was expressing or, a
-- product type expresses and.




-- A product is a way to carry multiple values around in a single data
-- constructor.  Any data constructor with two or more type arguments
-- is a product.



-- Tuples are anonymous products.



-- Type aliases create type constructors, not data constructors.



-- The reason it's important to understand cardinality is that the cardinality
-- of a datatype roughly equates to how difficult it is to reason about.



-- Records in Haskell are product types with additional syntax to provide
-- convenient accessors to fields within the record.


-- -----------------------------------------------------------------------------



-- Normal form is a "sum of products".






data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show

type AuthorName = String

-- Not in normal form.
data Author = Author (AuthorName, BookType)

-- In normal form because no further evaluation can be done without calculation.
data Author' = Fiction' AuthorName | Nonfiction' AuthorName deriving (Eq, Show)





-- -----------------------------------------------------------------------------




data Expr = Number Int
          | Add Expr Expr
          | Minus Expr
          | Mult Expr Expr
          | Divide Expr Expr



-- type Number' = Int
-- type Add' = (Expr', Expr')
-- type Minus' = Expr'
-- type Mult' = (Expr', Expr')
-- type Divide' = (Expr', Expr')
-- type Expr' = Either Number' (Either Add' (Either Minus' (Either Mult' Divide')))





-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========

-- Kind: The type of a type.
-- Kind signature: The definition of kind used by a type-level expression.
-- Higher-kinded type: A parameterized type constructor.
-- Arity: The number of arguments a function or constructor takes.
-- Nullary: What you call a function or constructor which takes no arguments.
-- Unary: What you call a function or constructor which takes only one argument.
-- Product: A constructor which takes more than one argument.
-- Anonymous product: A product with no name.
-- Cardinality: The amount of possible values inhabiting a datatype.
-- Accessor: A function that just returns data from a data structure.
-- Record: A product type that comes with accessors to fields within itself.
