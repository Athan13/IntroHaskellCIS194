module Party where

import Employee
import Data.Tree
import System.Environment


-- EXERCISE 1.1
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empName = name, empFun = fun}) (GL empList funSum) = GL (empList ++ [emp]) (funSum + fun)

-- EXERCISE 1.2
instance Semigroup GuestList where
  (GL empList1 funSum1) <> (GL empList2 funSum2) = GL (empList1 ++ empList2) (funSum1 + funSum2)

instance Monoid GuestList where
  mempty = GL [] 0

-- EXERCISE 1.3
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2) = if fun1 > fun2 then gl1 else gl2

-- EXERCISE 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node label treeList) = label `f` map (treeFold f) treeList

-- EXERCISE 3 --> pairs are (with boss, without boss) for each of the bosses sub-bosses
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss@(Emp _ fun) [] = (GL [boss] fun, mempty)
nextLevel boss guestlists     = (maxWithBoss, maxWithoutBoss)
    where
        -- get max of subtrees without sub-bosses (who would have zero fun if their boss showed up) and add boss to it
        maxWithBoss    = glCons boss $ mconcat $ map snd guestlists
        -- get max of all subtrees with or without sub-bosses
        maxWithoutBoss = mconcat $ map (uncurry moreFun) guestlists

-- EXERCISE 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- EXERCISE 5 --> I have no idea how this part works
parse :: GuestList -> String
parse (GL empl0 fun) = "Fun score : " 
                    ++ show fun ++ "\n"
                    ++ unlines (map empName empl0)

main :: IO ()
main = getArgs 
    >>= readFile . head
    >>= putStrLn . parse . maxFun . read
