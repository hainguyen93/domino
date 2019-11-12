-----------------------------------------------------------------------
-- 	COM2001: ADVANCED PROGRAMMING TECHNIQUES
-- 	Semester 1: Functional Programming
-- 	Assignment 2014-15
--
--      Assignment 2: Dominoes 
-- 	Registration No. 130197632
-----------------------------------------------------------------------

module MyDomino2 where

import Data.List
import System.Random
import MyDomino --codes from assignment 1

--Domsplayer type
type DomsPlayer = Hand -> Board -> (Domino, End)


{-|
  The 'simplePlayer' function represents a player 
  who always plays the first can-go domino in hand.
  It takes one argument, of type 'DomsPlayer'.
  It returns a domino and an end to play it at, of type '(Domino, End)'.
-}
simplePlayer :: DomsPlayer
--no base case because playDomsRound only calls it when (not knockingP)
simplePlayer (x:xs) ys 
  |goesP x L ys = (x, L)
  |goesP x R ys = (x, R)
  |otherwise = simplePlayer xs ys


{-|
  The 'hsdPlayer' function represents a player
  who plays the highest scoring domino in hand.
  It takes one argument, of type 'DomsPlayer'.
  It returns a domino and an end to play it at, of type '(Domino, End)'.
-}
hsdPlayer :: DomsPlayer

hsdPlayer xs ys = snd max
         where 
         (a, b)  = possPlays xs ys
         goLeft = map (\x -> (scoreBoard (helper (playDom x L ys)), (x, L))) a
         goRight = map (\x -> (scoreBoard (helper (playDom x R ys)), (x, R))) b
         lis = goLeft ++ goRight
         max = foldr my_max (head lis) (tail lis)


{-|
  The 'my_max' function finds the maximum 
  between two 2-tuples by comparing their first components.
  It takes 2 arguments, of type '(a,b)'.
  It returns the max 2-tuple, of type '(a,b)'.
-}    
my_max :: Ord a => (a,b) -> (a,b) -> (a,b)

my_max x@(a,_) y@(b,_) 
  |a >= b = x
  |otherwise = y       
         
      
{-|
  The 'shuffleDoms' function shuffles the set of dominoes.
  It takes a seed, of type 'Int'.
  It returns the shuffled set of dominoes, of type '[Domino]'.
-}
shuffleDoms :: Int -> [Domino]

shuffleDoms n = map snd a
            where
            ranList = take 28 (randoms (mkStdGen n) :: [Int])
            a = my_sort (my_zip ranList table) 
             
       
{-|
  The 'my_zip' function zips a list of Ints with a list of 2-tuples.
  It takes 2 arguments, of type '[Int]' and '[Domino]'.
  It returns a list, of type '[(Int, Domino)]'.
-}
my_zip :: [Int] -> [Domino] -> [(Int, Domino)]

my_zip [] _ = []

my_zip _ [] = []

my_zip (x:xs) (y:ys) = (x,y) : my_zip xs ys


{-|
  The 'my_sort' function sorts a list of (Int, Domino) 
  by comparing the 'Int' components. 
  It uses the algorithm of 'Quick-Sort'
  It takes 1 argument, of type '[(Int, Domino)]'.
  It returns a sorted list, of tyoe '[(Int, Domino)]'.  
-}
my_sort :: [(Int, Domino)] -> [(Int, Domino)]

my_sort [] = []

my_sort ((x,y):xs) =  my_sort left ++ [(x,y)] ++ my_sort right
           where 
           left = [(a,b)|(a,b) <- xs, a <= x]
           right = [(a,b)|(a,b) <- xs, a > x]


{-|
  The 'playDomsRound' function initiates the game and run it,
  return the final scores at the end.
  It takes 3 arguments, of types 'DomsPlayer's, 'Int'.
  It returns the final scores, of type '(Int, Int)'.
-}
playDomsRound :: DomsPlayer -> DomsPlayer -> Int -> (Int, Int)

playDomsRound p1 p2 s = play_Dom [] 0 p1 h1 p2 h2 (0, 0)
                                   where
                                   dom_set = shuffleDoms s  --shuffled set of dominoes
                                   h1 = take 9 dom_set  --P1 get first 9 dominoes
                                   h2 = take 9 (drop 9 dom_set)   --P2 gets next 9 dominoes
          

{-|
  The 'play_Dom' function implements the game, calling player in turn, 
  update new board, new scores, new hand.
  It takes 7 arguments.
  It returns the final scores, of type '(Int, Int)'
-}
play_Dom :: Board->Int->DomsPlayer->Hand->DomsPlayer->Hand->(Int, Int)->(Int, Int)

play_Dom ys t p1 h1 p2 h2 (c1, c2)     --t: turn
  |(knockingP h1 ys) && (knockingP h2 ys) = if (even t) then (c1, c2) 
                                                        else (c2, c1) --both knocking, stop
  |(knockingP h1 ys) = play_Dom ys (t+1) p2 h2 p1 h1 (c2, c1)  --P1 knocking
                      
  |otherwise = play_Dom new_board (t+1) p2 h2 p1 new_h1 (c2, new_c1)
               where 
               (dom,e) = p1 h1 ys --return (Dom, end)
               new_board = helper (playDom dom e ys)
               new_c1 = c1 + scoreBoard new_board 
               new_h1 = filter (/=dom) h1

