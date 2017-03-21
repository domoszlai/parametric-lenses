{-# LANGUAGE DeriveDataTypeable #-}

module ShipBase (

              ShipsView,
			  
              Ships (..), Positions (..),
              ships, positions,

              Ship (..), Position (..), Coord,

			  ShipFilter (..),
			  PositionFilter (..),
			  
              shipPredicate, positionPredicate, joinBy,
			  
              runShipT) where

import Control.Monad.State (StateT, execStateT, evalStateT, runStateT)
import qualified Control.Monad.State as ST (get, gets, modify)

import Data.Typeable

import Data.List
import Data.Maybe

import Prelude hiding (read)

import Data.PView

-------------------------------------------------------------------------------
-- Types and base data for testing

type Coord = (Double, Double)

data Ship = Ship
  {  s_ship_name    :: String
  ,  s_capacity     :: Int
  } deriving Show

data Position = Position
  {  p_ship_name    :: String
  ,  p_position     :: Coord
  } deriving Show
  
baseData = (
   [Ship {s_ship_name = "Queen", s_capacity = 100},
    Ship {s_ship_name = "Aberford", s_capacity = 200},
    Ship {s_ship_name = "Ross", s_capacity = 80}], 
   [Position {p_ship_name = "Ross", p_position = (10,25)},
    Position {p_ship_name = "Queen", p_position = (100,200)},
	Position {p_ship_name = "Aberford", p_position = (50,50)}])
   
-------------------------------------------------------------------------------
-- Source views for the ships and positions

type ShipsT     = StateT ShipsState IO
type ShipsState = ([Ship], [Position])
type ShipsView q a = PView q ShipsT a

-- focus domains
data Ships = Ships deriving (Typeable, Eq)
data Positions = Positions deriving (Typeable, Eq)

ships :: ShipsView Ships [Ship]
ships = Source (MkSource (const $ ST.gets fst)
                 (const $ \ss -> ST.modify (\(_, ps) -> (ss, ps)) >> return (const True)))

positions :: ShipsView Positions [Position]
positions = Source (MkSource (const $ ST.gets snd)
                 (const $ \ps -> ST.modify (\(ss, _) -> (ss, ps)) >> return (const True)))
		  
-------------------------------------------------------------------------------		
-- predicate functions		 				 

allpred :: (f -> a -> Bool) -> [f] -> a -> Bool
allpred p [] a = True
allpred p fs a = foldl1 (&&) (map (\f -> p f a) fs)

data ShipFilter = SNameIN [String] | CapacityGT Int deriving Typeable
data PositionFilter = PNameIN [String] | AreaIN (Coord, Coord) deriving Typeable

shipPredicate :: [ShipFilter] -> Ship -> Bool
shipPredicate = allpred pred
  where  
    pred (CapacityGT c) s = s_capacity s > c
    pred (SNameIN ns) s = elem (s_ship_name s) ns

positionPredicate :: [PositionFilter] -> Position -> Bool
positionPredicate = allpred pred
  where  
    pred (AreaIN ((x,y),(x',y'))) p = inside (p_position p)
      where
        inside (a,b) = a >= x && a <= x' && b >= y && b <= y'

    pred (PNameIN ns) s = elem (p_ship_name s) ns	
	
joinBy :: (Ship -> Position -> Bool)	
joinBy s = (s_ship_name s ==) . p_ship_name

-------------------------------------------------------------------------------	

runShipT main = evalStateT (evalStateT main []) baseData


