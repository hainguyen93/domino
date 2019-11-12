-----------------------------------------------------------------------
-- 	COM2001: ADVANCED PROGRAMMING TECHNIQUES
-- 	Semester 1: Functional Programming
-- 	Assignment 2014-15
--
--      Assignment 3: Smart Dominoes Players
-- 	Registration No. 130197632    
-----------------------------------------------------------------------

module MyDomsPlayer where

import DomsMatch   --given codes
import Data.List
import System.Random

--smart doms player
smartPlayer :: DomsPlayer

smartPlayer h b p s = selectMove p h b s


--detemine game situation and calling associated tactic 
selectMove :: Player -> Hand -> DomBoard -> Scores -> Move

selectMove p h InitBoard s = firstDrop h h   --initial board, first drop

selectMove p h b s
  |isCloseWin p h b s  = closeToWin p h b s      --play when close to win
  |isOpCloseWin op s   = opCloseToWin p h h b s  --opponent close to win
  |not (null hsdMoves)   = head hsdMoves           --play hsd dom
  |majorityHand h b    = playMajority p h b s    --play majority spot
  |otherwise           = hsdPlayer h b p s
   where  
   op = if (p==P1) then P2 else P1   --opponent
   hsdMoves = playhsd p h h b


-------------------------------------------------------------------------------------
--SITUATION: first drop

--play move that no reply can score more
firstDrop :: Hand -> [Dom] -> Move

firstDrop h [] = leastScore h (secretDoms h InitBoard) 

firstDrop h (x:xs)     
  |null replyLis = (x,L)     --no doms score more
  |otherwise = firstDrop h xs
   where 
   unknown = secretDoms h InitBoard  --all unknown doms
   replyLis = replyScoreMore x L InitBoard unknown 
  

--replies scoring more when playing a dom into board
replyScoreMore :: Dom -> End -> DomBoard -> [Dom] -> [(Dom, End, Int)]

replyScoreMore d e b ys
            = let (Just tb) = playDom P1 d e b  --temporary board, player doesn't matter
                  s = scoreboard tb  --score for playing dom d
                  l1 = [(y,L,(scoreDom y L tb))| y <- ys, goesLP y tb, (scoreDom y L tb) > s]
                  l2 = [(y,R,(scoreDom y R tb))| y <- ys, goesRP y tb, (scoreDom y R tb) > s]
               in l1 ++ l2


--find the dom when played, reply scores the least possible
leastScore :: Hand -> [Dom] -> Move

leastScore h ys = let lis = [(d,L,(deltaScore d ys))|d <- h]
                      (d,e,_) = last (mySort lis)
                   in (d,e)


--
deltaScore :: Dom -> [Dom] -> Int

deltaScore d ys = let (Just tb) = playDom P1 d L InitBoard  --temporary board, player doesn't matter
                      myScore = scoreboard tb   --score gained when playing this dom
                      replies = replyScoreMore d L InitBoard ys
                      (_,_,s) = head (mySort replies)
                      in (s - myScore)


--sorting 3-tuple using 3rd components, in descending order
mySort :: Ord c => [(a,b,c)] -> [(a,b,c)]

mySort [] = []   --empty list, already sorted

mySort ((x,y,z):xs) = (mySort l) ++ [(x,y,z)] ++ (mySort r)
           where 
           l = [(a,b,c)|(a,b,c) <- xs, c >= z]  --larger than 1st element
           r = [(a,b,c)|(a,b,c) <- xs, c < z]   --smaller than 1st element
      

--find all secret doms (opponent's and sleepings)
secretDoms :: Hand -> DomBoard -> [Dom]

secretDoms h InitBoard = filter (\d -> not (elem d h)) domSet  --empty board

secretDoms h (Board _ _ hist) 
               = let playedDoms = map (\(d,_,_) -> d) hist  --doms on board
                     knownDoms = h ++ playedDoms  --known doms
                  in filter (\d -> not (elem d knownDoms)) domSet   
 
-------------------------------------------------------------------------------------
--SITUATION: player close to win (score >= 53)
--Choose 53 as the max score gained when playing a dom is 8 (i.e. 53 = 61 - 8)

--check if close to win, try to score 61, if not try to get 59
isCloseWin :: Player -> Hand -> DomBoard -> Scores -> Bool

isCloseWin p h b (s1,s2) = let myScore = if (p==P1) then s1 else s2    
                               winMoves = movesToScore h b (61-myScore)  --winning moves (i.e. score 61)
                               movesGet59 = movesToScore h b (59-myScore)   --moves get to 59
                            in (not (null winMoves))||(not (null movesGet59)) 


--tactic associated with closeToWin situation
--only called when can win or score close to 59
closeToWin :: Player -> Hand -> DomBoard -> Scores -> Move

closeToWin p h b (s1,s2)
  |not (null winMoves) =  head winMoves   --have a winner, play to win
  |otherwise = head movesGet59    --if not, get close to 59 
   where 
   myScore = if (p==P1) then s1 else s2    
   winMoves = movesToScore h b (61-myScore)  --winning moves (i.e. score 61)
   movesGet59 = movesToScore h b (59-myScore)  --moves get to 59


--find Moves help score a given point 
movesToScore ::  Hand -> DomBoard -> Int -> [Move]

movesToScore h b s = let l1 = [(d,L,(scoreDom d L b))|d <- h, goesLP d b] --doms can go left
                         l2 = [(d,R,(scoreDom d R b))|d <- h, goesRP d b] --doms can go right
                         lis = filter (\(_,_,t) -> t==s) (l1++l2)   --doms can score s
                      in map (\(d,e,_) -> (d,e)) lis 


-------------------------------------------------------------------------------------
--SITUATION: opponent close to win

--predicate: is opponent close to win ?
isOpCloseWin :: Player -> Scores -> Bool

isOpCloseWin p (s1,s2) = ((p==P1)&&(s1>=53))||((p==P2)&&(s2>=53))


--prevent opponent win next play or reduce chances
opCloseToWin :: Player -> Hand -> [Dom] -> DomBoard -> Scores -> Move

opCloseToWin p h [] b (s1,s2)    --reduce chance next play win
               = let opScore = if (p==P1) then s2 else s1  
                     unknown = secretDoms h b                 --unknown doms
                  in reduceChance p h b unknown (61-opScore)  --reduce chance               

opCloseToWin p h (x:xs) b (s1,s2)   --look for a dom to prevent next play win
  |(goesLP x b) && (null l1) = (x,L)   
  |(goesRP x b) && (null l2) = (x,R) 
  |otherwise = opCloseToWin p h xs b (s1,s2) 
   where 
   opScore = if (p==P1) then s2 else s1  --opponent's score
   unknown = secretDoms h b   --unknown doms
   l1 = replyScoreMoves x L b unknown (61-opScore) --left replies scoring (61-s)
   l2 = replyScoreMoves x R b unknown (61-opScore) --right replies scoring (61-s) 


--moves that reply can score a value.
replyScoreMoves :: Dom -> End -> DomBoard -> [Dom] -> Int -> [Move]

replyScoreMoves d e b ys s 
                 = let (Just tb) = playDom P1 d e b     --temporary board, player does not matter
                       l1 = [(y,L)|y <- ys, goesLP y tb, scoreDom y L tb == s]
                       l2 = [(y,R)|y <- ys, goesRP y tb, scoreDom y R tb == s]
                    in (l1 ++ l2)
 

--reduce chance
reduceChance :: Player -> Hand -> DomBoard -> [Dom] -> Int -> Move

reduceChance p h b ys s = let l1 = [(d,L,(replyScoreMoves d L b ys s))|d <- h, goesLP d b]
                              l2 = [(d,R,(replyScoreMoves d R b ys s))|d <- h, goesRP d b]
                              lis = map (\(d,e,l) -> (d,e,(length l))) (l1++l2)
                              (d,e,_) =  last (mySort lis)
                           in (d,e)                              

-------------------------------------------------------------------------------------
--check majority spot can go into the board
playMajority :: Player -> Hand -> DomBoard -> Scores-> Move 

playMajority p h b@(Board (l,_) (_,r) _) s
  |(length l1 >= 4) && (not (null d1)) = (head d1, L)
  |(length l2 >= 4) && (not (null d2)) = (head d2, R)
  |(length l1 >= 4) = hsdPlayer l1 b p s
  |(length l2 >= 4) = hsdPlayer l2 b p s
  where 
  l1 = majorityDoms h l  --doms having spot l in hand
  d1 = filter (\(v1,v2) -> v1==v2) l1  
  l2 = majorityDoms h r   --doms having spot r in hand
  d2 = filter (\(v1,v2) -> v1==v2) l2           


--predicate: have majority hand ?
majorityHand :: Hand -> DomBoard -> Bool

majorityHand h (Board (l,_) (_,r) _)
      = (length (majorityDoms h l)>=4)||(length (majorityDoms h r)>=4)


--list of majority spots (doms >= 4)
majorityDoms :: Hand -> Int -> [Dom]

majorityDoms h v = filter (\(v1,v2) -> (v1==v)||(v2==v)) h            


-------------------------------------------------------------------------------------
--play hsd doms

--play hsd that reply cannot score more
playhsd :: Player -> Hand -> [Dom] -> DomBoard -> [Move]

playhsd p h ys b
  |(not kp) && (null replyLis) = (d,e) : playhsd p h (filter (/=d) ys) b
  |otherwise = []
   where 
   kp = knocking ys b
   (d,e,s) = hsd ys b
   unknown = secretDoms h b
   (Just tb) = playDom p d e b
   l1 = [(x,L)|x <- unknown, goesLP x tb, scoreDom x L tb > s]
   l2 = [(x,R)|x <- unknown, goesRP x tb, scoreDom x R tb > s]
   replyLis = l1 ++ l2  --reply moves that scores more 
   
























