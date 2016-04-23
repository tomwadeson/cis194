module Party where

import Employee

instance Monoid GuestList where
  mempty                      = GL [] 0
  mappend (GL xs x) (GL ys y) = GL (xs ++ ys) (x + y)

glCons :: Employee -> GuestList -> GuestList
glCons e (GL xs x) = GL (e : xs) (empFun e + x)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if gl1 > gl2 then gl1 else gl2
