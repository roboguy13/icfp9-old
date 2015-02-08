{-# LANGUAGE ExistentialQuantification, KindSignatures #-}
import GHC.TypeLits
class Widget a

data HList (c :: Constraint) = Nil | forall a. Cons a HList

example :: HList Widget
example = undefined
