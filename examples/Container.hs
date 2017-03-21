{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

import Control.Monad.Trans
import Control.Monad.State (StateT, execStateT, evalStateT, runStateT)
import qualified Control.Monad.State as ST (get, put)

import Data.Typeable

import Data.List
import Data.Maybe

import Prelude hiding (read)

import Data.PView

-------------------------------------------------------------------------------
-- Data types and base data for testing

data Container = Container ContainerType [Property] [Item] deriving (Show, Eq)
data ContainerType = CTRoot | CTBuilding String | CTRoom String | CTBox String deriving (Show, Eq)

data Property = Owner String deriving (Show, Eq)
data Item = Item 
  {  i_name     :: String
  ,  i_quantity :: Int
  } deriving (Show, Eq)

data RoseTree a = RoseTree a [RoseTree a] deriving Show
type Store = RoseTree Container
  
buildingA = Container (CTBuilding "A") [] []
buildingB = Container (CTBuilding "B") [] []

roomA11 = Container (CTRoom "1.1") [] []
roomA12 = Container (CTRoom "1.2") [] []
roomB11 = Container (CTRoom "1.1") [] []

box1 = Container (CTBox "first to the left") [Owner "Albus Dumbledore"] [Item "Sorcerer's Stone" 1]
box2 = Container (CTBox "second to the left") [] []
box3 = Container (CTBox "the only one") [Owner "Queen Elizabeth II"] [Item "Andamooka Opal" 1]

baseData = RoseTree (Container CTRoot [] [])
	[RoseTree buildingA 
		[RoseTree roomA11 [RoseTree box1 [], RoseTree box2 []]
		,RoseTree roomA12 []
		]
	,RoseTree buildingB 
		[RoseTree roomB11 [RoseTree box3 []]
		]
	]
		
-------------------------------------------------------------------------------
-- The source view for accessing shared data

type StoreT      = StateT StoreState IO
type StoreState  = Store
type StoreView q = PView q StoreT Store

data StoreSource = StoreSource deriving (Typeable, Eq)

store :: StoreView StoreSource
store = Source (MkSource (const $ ST.get)
                 (const $ \s -> ST.put s >> return (const True)))

-------------------------------------------------------------------------------
-- The used lens and related types, functions

data Selector = S {unS :: Int} deriving (Typeable, Eq)

selectLens :: ParametricLens Selector Store Store
selectLens = MkPLens {getf = get, putf = put}
   where
     get (S i) (RoseTree _ ss) = ss !! i
     put (S i) (RoseTree r ss) s = (RoseTree r (take i ss ++ [s] ++ drop (i + 1) ss), \(S j)-> j == i)	 				 
							 
select :: (Typeable q, Eq q) => StoreView q -> StoreView (q, Selector)
select v = v `Focus` selectLens

-------------------------------------------------------------------------------
	 
data StoreResult where 
	StoreResult :: (Typeable q, Eq q) => StoreView q -> q -> StoreResult

findFirst :: (Typeable q, Eq q) => StoreView q -> q -> (Container -> Bool) -> PViewT StoreT (Maybe StoreResult)
findFirst v q pred = do
   (RoseTree container children) <- read v q
   if pred container 
     then return $ Just $ StoreResult v q
     else case children of
               [] -> return Nothing
               _  -> searchChildren (length children)
   where
     searchChildren 0 = return Nothing
     searchChildren i = do
       res <- findFirst (select v) (q, S (i-1)) pred
       case res of
          Nothing -> searchChildren (i-1)	   
          _       -> return res
		
findAll :: (Typeable q, Eq q) => StoreView q -> q -> (Container -> Bool) -> PViewT StoreT [StoreResult]
findAll v q f = findAll' v q f []

findAll' :: (Typeable q, Eq q) => StoreView q -> q -> (Container -> Bool) -> [StoreResult] -> PViewT StoreT [StoreResult]
findAll' v q f rs = do
   (RoseTree room children) <- read v q
   if f room
      then searchChildren (length children - 1) (StoreResult v q : rs)
      else searchChildren (length children - 1) rs
   where
     searchChildren 0 rs = return rs
     searchChildren i rs = do
       rs <- findAll' (select v) (q, S i) f rs
       searchChildren (i-1) rs
		
--------------------------------------------------------

findProperty prop (Container _ props _) = elem prop props
findContainer ty (Container aty _ _) = ty == aty

main = evalStateT (evalStateT mainContainer []) baseData >>= print
mainContainer = do
  (Just (StoreResult v q)) <- findFirst store StoreSource (findProperty (Owner "Albus Dumbledore"))
  observe v q "observealbus" (lift $ print "Oh, no!")
  (Just (StoreResult v q)) <- findFirst store StoreSource (findContainer (CTBox "first to the left"))
  (RoseTree (Container ty props items) children) <- read v q
  update v q (RoseTree (Container ty [Owner "Voldemort"] items) children)
  read store StoreSource
  
--mainContainer = read store StoreSource  
--mainContainer = read (select store) (StoreSource, S 1)
