module Party where

import Employee
import Data.Tree

instance Monoid GuestList where
  mempty                      = GL [] 0
  mappend (GL xs x) (GL ys y) = GL (xs ++ ys) (x + y)

glCons :: Employee -> GuestList -> GuestList
glCons e (GL xs x) = GL (e : xs) (empFun e + x)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if gl1 > gl2 then gl1 else gl2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node label forest) = f label (map (treeFold f) forest)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results = (withBoss, withoutBoss)
  where
    withBoss    = glCons boss (mconcat $ map snd results)
    withoutBoss = mconcat $ map (\(gl1, gl2) -> moreFun gl1 gl2) results

maxFun :: Tree Employee -> GuestList
maxFun = (\(gl1, gl2) -> moreFun gl1 gl2) . treeFold nextLevel

prettyPrint :: GuestList -> String
prettyPrint (GL list fun) = "Fun: " ++ show fun ++ "\n"
                         ++ "Guest list:\n"
                         ++ (unlines . map empName $ list) 

main :: IO ()
main = do
  contents <- readFile "company.txt"
  let guestlist = maxFun . read $ contents
  putStrLn . prettyPrint $ guestlist

t =
  Node 0
    [ Node 1 []
    , Node 2 []
    , Node 3 [ Node 4 [], Node 5 [] ] ]
