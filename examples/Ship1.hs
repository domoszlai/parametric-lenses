{-# LANGUAGE TupleSections #-}

module Ship1 (

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
-- generic lenses

selectLens :: (f -> a -> Bool) -> ParametricLens f [a] [a]
selectLens pred = MkPLens {getf = get, putf = put}
   where
     get = filter . pred
	 
     -- ss: list of original records (source)
	 -- vs: list of modified records (view)	 
     put f ss vs = (m' ++ vs, \f' -> any (pred f') (m ++ vs))
      where
        -- m:  list of original records that are part of the focus 
	    -- m': list of original records that are not part of the focus 	  
        (m, m') = partition (pred f) ss

joinLens :: (a -> b -> Bool) -> Lens ([a],[b]) [(a,b)]
joinLens by = MkLens {get = get, put = put}
   where
     -- lss: original left list (source)
	 -- rss: original right list (source)
     get (lss, rss) = mapMaybe f lss 
	    where f ls = (ls,) <$> find (by ls) rss
		
	 -- vs: list of joined modified records (view)		
     put (lss, rss) vs = (lms' ++ lvs, rms' ++ rvs)
	    where
		  -- lvs: modified left list (view)
	      -- rvs: modified right list (view)		
          (lvs, rvs) = unzip vs
		  
		  -- try to to find a pair for the elements of the left list 
          pairLeft  = map (\ls -> (ls, find (by ls) rss)) lss
		  -- and the other way around 
          pairRight = map (\rs -> (rs, find (flip by rs) lss)) rss
		  
          dropPaired as = map fst (filter (isNothing.snd) as)
		  
		  -- lms': unpaired record (and thus unmodified) of the left list
          lms' = dropPaired pairLeft
		  -- rms': unpaired record (and thus unmodified) of the right list		  
          rms' = dropPaired pairRight
		  
-------------------------------------------------------------------------------		
-- view definitions				 				 

filteredShips' :: ShipsView (Ships, [ShipFilter]) [Ship]
filteredShips' = ships `Focus` (selectLens shipPredicate)
	
filteredShips :: ShipsView [ShipFilter] [Ship]
filteredShips = Translate filteredShips' (\q -> (Ships,q))

filteredPositions' :: ShipsView (Positions, [PositionFilter]) [Position]
filteredPositions' = positions `Focus` (selectLens positionPredicate)

filteredPositions :: ShipsView [PositionFilter] [Position]
filteredPositions = Translate filteredPositions' (\q -> (Positions,q))

jointView :: ShipsView ((Ships,[ShipFilter]),(Positions,[PositionFilter])) ([Ship], [Position])
jointView = filteredShips' `Product` filteredPositions'

shipPositions :: ShipsView ((Ships,[ShipFilter]),(Positions,[PositionFilter])) [(Ship, Position)]
shipPositions = jointView `Project` (joinLens joinBy)


	
