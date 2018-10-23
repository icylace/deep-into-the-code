module O13__Typeclasses () where

-- New datatypes can be created using the `data` keyword.

data Me = Me

-- The type constructor and data constructor don't have to have the same name.

data Self = Myself

{-
-- Equality comparison with our custom `Me` and `Self` types won't work.
_ = Me == Me            -- Causes a compile-time error.
_ = Myself == Myself    -- Causes a compile-time error.
-}





-- Typeclass instances are unique to a given type.  Attempting to create
-- multiple instances of the same typeclass for the same type will
-- result in an error.





-- In Haskell, dispatching is specifying which typeclass instance to use for
-- a function or value.




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

-- `Numberish'` is a poorly conceived typeclass that demonstrates using type
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

_ = defaultNumber :: Age    -- `Age 65`
_ = defaultNumber :: Year   -- `Year 1988`

{-
-- `defaultNumber` on its own is too ambiguous.
_ = defaultNumber   -- Causes a compile-time error.
-}

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Concrete types imply all the typeclasses they have instances of.

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






-- When a type implements a typeclass it must have a declaration for every
-- function provided in the typeclass.







-- A type that implements a typeclass is said to have an instance of the
-- typeclass.  This instance specifies how the typeclass should work for
-- the type.

-- A type with an instance of a typeclass can use its values with the functions
-- defined for that typeclass.












-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Certain typeclasses can have instances of them conveniently "derived".
-- Derived instances are automatically generated instances based solely
-- on the defintion of a type.

data I = I deriving Eq

_ = I == I    -- `True`
_ = I /= I    -- `False`

-- The common typeclasses that can be derived are `Eq`, `Ord`, `Enum`,
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

-- It's possible to use a looser constraint to achieve the same thing.  In this
-- case, the `Ord` typeclass can be used because it already requires the use
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

-- Some standard Haskell typeclasses have a default concrete type which gets
-- used if the concrete type can't be determined when needed.

-- The division operator, `/`, is part of the `Fractional` typeclass and has the
-- type `Fractional a => a -> a -> a`.  `Fractional` defaults to the concrete
-- type `Double`.  An explicit type declaration can be used to override
-- the default.

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

-- The addition operator, `+`, from the `Num` typeclass has the type
-- `Num a => a -> a -> a`

x = 10 :: Integer
y = 5  :: Integer

_ = (+)       -- Has the type `Num a => a -> a -> a`.
_ = (x +)     -- Has the type `Integer -> Integer`.
_ = (x + y)   -- Has the type `Integer`.









-- Typeclass inheritance is only additive.  That is, a subclass cannot override
-- a superclass in any way.










-- A typeclass instance should be kept in the same file as the type that has it.





-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

-- Key Terms
-- =========

-- Typeclass: A set of functions that can be shared across sets of types.
-- Typeclass constraint: A type variable for a type implementing a typeclass.
-- Superclass: A typeclass required by another typeclass.
-- Subclass: A typeclass that requires another typeclass.
-- Typeclass arrow (`=>`): Syntax for defining a typeclass constraint.
-- Typeclass instance (instance): An implementation of a typeclass for a type.

-- Derived instance: An instance generated based only on a datatype definition.

-- Type variable: A placeholder for a type.

-- Typeclass inheritance: When a typeclass has a superclass.

-- Polymorphism: Type variables referring to possibly multiple concrete types.
-- Polymorphism: The usage of functions across different sets of types.

-- Parametric polymorphism: Polymorphism over any type.
-- Constrained (ad-hoc, bounded) polymorphism: Polymorphism over certain types.
-- Principal type: In Haskell, the most generic type which still typechecks.
-- Parametricity: Uniform function behavior relative to parametric arguments.

-- Serialization: The process of converting data into a transmittable format.
-- Dispatch: In Haskell, to specify what instance to use for an expression.
-- Effect: An observable action a program takes beyond computing a value.
