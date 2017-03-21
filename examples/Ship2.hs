{-# LANGUAGE TupleSections #-}

module Ship2 (

              filteredShips, filteredPositions, shipPositions
			  
              ) where

import Control.Monad.Trans
import Control.Applicative

import Data.List
import Data.Maybe

import Prelude hiding (read)

import Data.PView
import ShipBase

-------------------------------------------------------------------------------
-- generic lenses with push down (change list) semantics

calcChangeList :: Lens ([a], [a]) [a]
calcChangeList = MkLens {get = get, put = put}
   where
     -- ss: list of original records (source) 
	 -- cs: change list	 
     get (ss, cs) = ss
	 -- vs: list of modified records (view)
     put (ss, _) vs = (vs, ss ++ vs)
	
selectLens :: (f -> a -> Bool) -> ParametricLens f [a] ([a], [a])
selectLens pred = MkPLens {getf = get, putf = put}
   where
     -- ss: list of original records (source)   
     get f ss = (filter (pred f) ss, [])
	 
	 -- vs: list of modified records (view)
	 -- cs: change list	 
     put f ss (vs, cs) = (m' ++ vs, \f' -> any (pred f') cs)
      where
	    -- m': list of original records that are not part of the focus 	  
        m' = filter (not . pred f) ss
		
joinLens :: (a -> b -> Bool) -> Lens (([a],[a]),([b],[b])) [(a,b)]
joinLens by = MkLens {get = get, put = put}
   where
     -- lss: original left list (source)
	 -- rss: original right list (source)
	 -- skip change lists, only used in the putback direction
     get ((lss,_),(rss,_)) = mapMaybe f lss 
	    where f ls = (ls,) <$> find (by ls) rss
		
	 -- vs: list of joined modified records (view)
	 -- skip change lists of the original input, we create it to push down to the underlying view
     put ((lss,_),(rss,_)) vs = ((lms' ++ lvs, lms ++ lvs), (rms' ++ rvs, rms ++ rvs))
	    where
		  -- lvs: modified left list (view)
	      -- rvs: modified right list (view)			
          (lvs, rvs) = unzip vs
		  		  		  
		  -- try to to find a pair for the elements of the left list 
          pairLeft  = map (\ls -> (ls, find (by ls) rss)) lss
		  -- and the other way around 
          pairRight = map (\rs -> (rs, find (flip by rs) lss)) rss
		  
          dropPaired as = map fst (filter (isNothing.snd) as)
          keepPaired as = map fst (filter (isJust.snd) as)
		  
		  -- lms:  paired record (and thus modified) of the left list
		  -- lms': unpaired record (and thus unmodified) of the left list
          lms  = keepPaired pairLeft
          lms' = dropPaired pairLeft
		  -- rms': unpaired record (and thus unmodified) of the right list
          rms  = keepPaired pairRight
          rms' = dropPaired pairRight				    
	
mapTuple f as = map (\(a1,a2) -> (f a1, f a2)) as

-------------------------------------------------------------------------------		
-- view definitions				 				 

filteredShips' :: ShipsView (Ships, [ShipFilter]) ([Ship], [Ship])
filteredShips' = ships `Focus` (selectLens shipPredicate)

filteredShips :: ShipsView [ShipFilter] [Ship]
filteredShips = (filteredShips' `Project` calcChangeList) `Translate` (\q -> (Ships,q))

filteredPositions' :: ShipsView (Positions, [PositionFilter]) ([Position],[Position])
filteredPositions' = positions `Focus` (selectLens positionPredicate)

filteredPositions :: ShipsView [PositionFilter] [Position]
filteredPositions = (filteredPositions' `Project` calcChangeList) `Translate` (\q -> (Positions,q))

jointView :: ShipsView ((Ships,[ShipFilter]),(Positions,[PositionFilter])) (([Ship],[Ship]), ([Position],[Position]))
jointView = filteredShips' `Product` filteredPositions'

shipPositions :: ShipsView ((Ships,[ShipFilter]),(Positions,[PositionFilter])) [(Ship, Position)]
shipPositions = jointView `Project` (joinLens joinBy)
