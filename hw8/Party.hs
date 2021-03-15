{-#  OpTIONS_GHC -fno-warn-orphans #-}


module Party where

import Data.Monoid
import Employee
import Data.Tree
import Data.Tuple

glCons::Employee -> GuestList ->GuestList
glCons a (GL xs funInteger) = 
        GL (a:xs) (funInteger+(empFun a))


instance Semigroup GuestList where
    (GL xs x) <> (GL ys y) = GL (xs ++ ys) (x+y)

instance Monoid GuestList where
    mempty = GL [] 0


moreFun::GuestList ->GuestList ->GuestList
moreFun fstGl@(GL _ a) sndGl@(GL _ b) 
        | a >= b = fstGl
        | otherwise = sndGl

--ex2
{-
data Tree a = Node {
rootLabel :: a, -- label value
subForest :: [Tree a] -- zero or more child trees
}
-}
--foldl (+) 0 []     --(\ acc n ->) 


treeFold :: (a -> [b] -> b)-> b -> Tree a -> b
treeFold f init (Node x children) = f x (map (treeFold f init) children)

treeFold' :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold' f init (Node {rootLabel = rl, subForest = sf})
  = f rl (map (treeFold' f init) sf)

{-
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

treeFold'' ::(a->b)   --不能是 a->b   因为此函数处理不了[a]  
                    ->Tree a -> b
treeFold''  f  (Node {rootLabel = rl,subForest = treeList})
        | (isEmpty treeList) = f rl                -- treeList == []  -- error
        | otherwise = map (treeFold'' f) treeList
-}

nextLevel ::Employee -> [(GuestList,GuestList)] -> (GuestList,GuestList)  --再颠倒下
nextLevel boss xs = swap $ (foldr  (\ (a,b) (acc1,acc2) -> (a<>acc1,b<>acc2))
                        (GL [] 0,GL [] 0) xs) <> (GL [] 0,GL [boss] (empFun boss))

nextLevel' :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel' boss@(Emp n f) [] = (GL [boss] f, mempty)
nextLevel' boss@(Emp n f) guestLists =
  let nobosses = mconcat $ map snd guestLists
      bosses = mconcat $ map fst guestLists
  in (glCons boss nobosses,
      bosses)  -- 原版 moreFun bosses nobosses


maxFun ::Tree Employee -> GuestList
maxFun x= uncurry moreFun ans
    where ans = treeFold nextLevel' (GL [] 0,GL [] 0) x



main :: IO ()
main = do
  company <- fmap read (readFile "company.txt")
  let (GL es f) = maxFun company
  putStrLn $ "Total fun: " ++ show f
  putStr $ (unlines . map empName) es

main1::IO()
main1 = do
    putStr $ unlines  ["aa","bb"]


-- 