module O13__TypeClasses () where

-- New datatypes can be created using the `data` keyword.

data Me = Me

-- The type constructor and data constructor don't have to have the same name.

data Self = Myself

{-
-- Equality comparison with our custom `Me` and `Self` types won't work.
_ = Me == Me            -- Error.
_ = Myself == Myself    -- Error.
-}





-- Type class instances are unique to a given type. Attempting to create
-- multiple instances of the same type class for the same type will
-- result in an error.





-- In Haskell, dispatching is specifying which type class instance to use for
-- a function or value.






-- -----------------------------------------------------------------------------

-- A datatype does not have to have any data constructors.

data Void






-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------


class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

newtype Age = Age Integer deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year = Year Integer deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where integerOfA = toNumber a
        integerOfAPrime = toNumber a'
        summed = integerOfA + integerOfAPrime

_ = sumNumberish (Age 10) (Age 10)    -- `Age 20`

-- -----------------------------------------------------------------------------

-- `Numberish'` is a poorly conceived type class that demonstrates using type
-- declaration for disambiguation.

class Numberish' a where
  fromNumber' :: Integer -> a
  toNumber' :: a -> Integer
  defaultNumber :: a

instance Numberish' Age where
  fromNumber' n = Age n
  toNumber' (Age n) = n
  defaultNumber = Age 65

instance Numberish' Year where
  fromNumber' n = Year n
  toNumber' (Year n) = n
  defaultNumber = Year 1988

_ = defaultNumber :: Age     -- `Age 65`
_ = defaultNumber :: Year    -- `Year 1988`

{-
-- `defaultNumber` on its own is too ambiguous.
_ = defaultNumber    -- Error.
-}

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Concrete types imply all the type classes they have instances of.

-- Since `Int` has instances of `Num`, `Ord`, and `Eq`, the following will
-- all typecheck.

add_ :: Int -> Int -> Int
add_ x y = x + y

addWeird_ :: Int -> Int -> Int
addWeird_ x y =
  if x > 1
  then x + y
  else x

check_ :: Int -> Int -> Bool
check_ a a' = a == a'







-- A type variable is a placeholder for a type.






-- When a type implements a type class it must have a declaration for every
-- function provided in the type class.







-- A type that implements a type class is said to have an instance of the type
-- class. This instance specifies how the type class should work for the type.

-- A type with an instance of a type class can use its values with the functions
-- defined for that type class.






-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Certain type classes can have instances of them conveniently "derived".
-- Derived instances are automatically generated instances based solely
-- on the defintion of a type.

data I = I deriving Eq

_ = I == I    -- `True`
_ = I /= I    -- `False`

-- The common type classes that can be derived are `Eq`, `Ord`, `Enum`,
-- `Bounded`, `Read`, and `Show`.

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Polymorphic types can use constraints in their instance declarations to
-- require their concrete types to have instances as well.

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-- Polymorphic types can use constraints in their instance declarations to
-- require their concrete types to have instances as well.

-- -----------------------------------------------------------------------------

-- It's possible to use a looser constraint to achieve the same thing. In this
-- case, the `Ord` type class can be used because it already requires the use
-- of `Eq`.

data Identity' a = Identity' a

instance Ord a => Eq (Identity' a) where
  (==) (Identity' v) (Identity' v') = compare v v' == EQ

-- However, doing it this way would be unnecessarily less precise.

-- -----------------------------------------------------------------------------

-- Loosened constraints can be used in normal type signatures, too.

check' :: Eq a => a -> a -> Bool
check' x y = x == y

check'' :: Ord a => a -> a -> Bool
check'' x y = x == y

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Some standard Haskell type classes have a default concrete type which gets
-- used if the concrete type can't be determined when needed.

-- The division operator, `/`, is part of the `Fractional` type class and has
-- the type `Fractional a => a -> a -> a`. `Fractional` defaults to the
-- concrete type `Double`. An explicit type declaration can be used
-- to override the default.

_ = 1 / 2                         -- `0.5`
_ = 1 / 2 :: Fractional a => a    -- `0.5`
_ = 1 / 2 :: Float                -- `0.5`
_ = 1 / 2 :: Double               -- `0.5`
_ = 1 / 2 :: Rational             -- `1 % 2`

-- We get an error if we try to use explicit type signatures to more generalize
-- an expression.
--
-- For example, even though `Fractional` is a subclass of `Num`, an expression
-- having a `Fractional` type cannot be generalized into having a `Num` type.
--
--     1 / 2 :: Num a => a
--


-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------





-- Values having concrete type will necessarily concretize polymorphic functions
-- that are applied to them.

-- The addition operator, `+`, from the `Num` type class has the type
-- `Num a => a -> a -> a`

x = 10 :: Integer
y = 5  :: Integer

_ = (+)
-- Has the type `Num a => a -> a -> a`.

_ = (x +)
-- Has the type `Integer -> Integer`.

_ = (x + y)
-- Has the type `Integer`.





-- Type class inheritance is only additive. That is, a subclass cannot override
-- a superclass in any way.




-- A type class instance should be in the same file as the type that has it.





-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

{-

Key Terms
=========

Type class: A set of functions that can be shared across sets of types.
Type class constraint: A type variable for a type implementing a type class.
Superclass: A type class required by another type class.
Subclass: A type class that requires another type class.
Type class arrow (`=>`): Syntax for defining a type class constraint.
Type class instance: An implementation of a type class for a type.
Instance: A synonym for type class instance.

Derived instance: An instance generated based only on a datatype definition.

Type variable: A placeholder for a type.

Type class inheritance: When a type class has a superclass.

Polymorphism: The usage of functions across different sets of types.
Parametric polymorphism: Polymorphism over any type.
Constrained polymorphism: Polymorphism over certain types.
Ad-hoc polymorphism: A synonym for constrained polymorphism.
Bounded polymorphism: A synonym for constrained polymorphism.

Principal type: In Haskell, the most generic type which still typechecks.
Parametricity: Uniform function behavior relative to parametric arguments.

Serialization: The process of converting data into a transmittable format.
Dispatch: In Haskell, to specify what instance to use for an expression.
Effect: An observable action a program takes beyond computing a value.

-}
