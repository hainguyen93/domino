-----------------------------------------------------------------------
-- 	COM2001: ADVANCED PROGRAMMING TECHNIQUES
-- 	Semester 1: Functional Programming
-- 	Assignment 2014-15
--
--      Assignment 1: Dominoes 
-- 	Registration No. 130197632
-----------------------------------------------------------------------

module MyDomino where

import Prelude
import Data.Tuple


--A domino as 2-tuple with two non-negative numbers
type Domino = (Int, Int)

--A hand contains a bunch of Dominoes
type Hand = [Domino]

--A Board is a line of Dominoes
type Board = [Domino]

--A Board has two ends, right-R and left-L
data End = L | R  deriving (Eq, Show)        


--goesP 
--Check a given domino can go at a given end of a given board

goesP :: Domino -> End -> Board -> Bool

goesP _ _ [] = True   --empty board, return True

goesP (a,b) e ys      -- e:left or right end
  |a == t || b == t = True
  |otherwise        = False
   where 
   t = if (e == L) then fst (head ys)
       else snd (last ys)


--knockingP
--Check no dominoes in hand can go

knockingP :: Hand -> Board -> Bool

knockingP [] _  = True    --empty hand, true

knockingP _ []  = False   --empty board, go anyway

knockingP (x:xs) ys 
    = not (goesP x L ys || goesP x R ys) && knockingP xs ys


--playedP
--Check a given domino already played or not

playedP :: Domino -> Board -> Bool

playedP _ [] = False     --empty board, yet played

playedP x ys = elem x ys || elem (swap x) ys


--possPlays
--Find all dominoes that can go
--Return a pair of two lists, one for all dominoes that can
--go on the left and the other right

possPlays :: Hand -> Board -> ([Domino],[Domino])     

possPlays [] _  = ([],[])   --empty hand, no dominoes to go

possPlays xs [] = (xs,xs)   --empty board, any domino can go at any end

possPlays xs ys = (l,r) 
                  where 
                  l = possGoAt xs L ys
                  r = possGoAt xs R ys
   

--possGoAt
--Return a list of all dominoes that can go at a given end

possGoAt :: Hand -> End -> Board -> [Domino]

possGoAt [] _ _ = []    --empty hand, base case 

possGoAt (x:xs) e ys        --e: L or R
  |not (playedP x ys) && goesP x e ys = x : (possGoAt xs e ys)
  |otherwise = possGoAt xs e ys


--playDom
--Play a given domino at a given end
--Return new board, Maybe type, if possible

playDom :: Domino -> End -> Board -> Maybe Board

playDom x _ [] = Just [x]    --empty board, go anyway

playDom x@(a,b) e ys     
  |e == L && goesP x e ys = if (b == l) then Just (x:ys)         --left end
                                        else Just (swap x : ys) 
  |e == R && goesP x e ys = if (a == r) then Just (ys ++ [x])    --right end
                                        else Just (ys ++ [swap x])              
  |otherwise  = Nothing
   where 
   l = fst (head ys)   --left end
   r = snd (last ys)   --right end


--scoreBoard
--Calculate the score according to 5s-and-3s principle

scoreBoard :: Board -> Int

scoreBoard [] = 0   --empty board, 0 point

scoreBoard [(a,b)]    --one-element board
  |mod sum  5 == 0 = div sum 5       --multiply of 5 only
  |mod sum  3 == 0 = div sum 3  
  |otherwise = 0
   where sum = a+b

scoreBoard ys     --two or more tiles board
  |mod sum 15 == 0 = div sum 5 + div sum 3    --multiply of 3 and 5
  |mod sum  5 == 0 = div sum 5       --multiply of 5 only
  |mod sum  3 == 0 = div sum 3       --multiply of 3 only
  |otherwise = 0
   where
   sum = a + b      --sum of two ends
   a = if l1 == l2 then 2*l1 else l1   --double in case of double domino
   b = if r1 == r2 then 2*r2 else r2   
   (l1,l2):_ = ys        --left end
   (r1,r2) = last ys     --right end


--scoreN
--Find all Dominoes, if played, give n scores
--Return a 2-tuple of 2 list, one for left end, the other right
 
scoreN :: Board -> Int -> ([Domino],[Domino])

scoreN [] n = (a,a)     --empty board, same for left and right
             where a = scoreAt table L [] n    
             
scoreN ys n = (a,b)
             where 
             c = possPlays table ys
             a = scoreAt (fst c) L ys n  --go at left end
             b = scoreAt (snd c) R ys n  --go at right end


--table (const)
--Contain all 28 Dominoes

table :: [Domino]

table = [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),
         (0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
         (1,2),(1,3),(1,4),(1,5),(1,6),
         (2,3),(2,4),(2,5),(2,6),
         (3,4),(3,5),(3,6),
         (4,5),(4,6),
         (5,6)]


--scoreAt
--Try all possible go at a given end, calculate the score and compare to n,
--Return a list of dominoes that score n if played at that given end

scoreAt :: [Domino] -> End -> Board -> Int -> [Domino]

scoreAt [] _ _ _ = []    --domino list empty, return empty

scoreAt ((a,b):xs) e [] n      --empty board, e: left or right end
  |divMod sum 3 == (n,0) || divMod sum 5 == (n,0) = (a,b):(scoreAt xs e [] n)
  |mod sum 3 /= 0 && mod sum 5 /= 0 && n==0 = (a,b):(scoreAt xs e [] n)
  |otherwise = scoreAt xs e [] n 
   where sum = a + b
  
scoreAt (x:xs) e ys n       --e: left or right end
  |(goesP x e ys) && scoreBoard (helper (playDom x e ys)) == n = x:(scoreAt xs e ys n)
  |otherwise = scoreAt xs e ys n
  

--Maybe helper 
--Convert Maybe type into Board

helper :: Maybe Board -> Board

helper (Just x) = x

helper Nothing = []























