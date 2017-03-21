module ShipTest where

import Control.Monad.Trans

import Prelude hiding (read)

import Data.PView
import ShipBase

-- Choose one of the implementations
-- import Ship1
import Ship2
   
----------------------------------------------------------------------
	  
main = runShipT mainShip >>= print

----------------------------------------------------------------------
-- Read tests

-- Expected: ["Queen"]
--mainShip = read shipPositions ((Ships, [CapacityGT 80]), (Positions, [PNameIN ["Ross","Queen"]]))

-- Expected: []
--mainShip = read shipPositions ((Ships, [CapacityGT 0]), (Positions, [PNameIN ["Ross"],PNameIN ["Queen"]]))

-- Expected: ["Queen"]
--mainShip = read shipPositions ((Ships, [CapacityGT 0]), (Positions, [PNameIN ["Queen"]]))

-- Expected: ["Aberford"]
--mainShip = read shipPositions ((Ships, [CapacityGT 90]), (Positions, [AreaIN ((0,0),(90,90))]))

-- Expected: ["Ross"]
--mainShip = read shipPositions ((Ships, [SNameIN ["Ross"]]), (Positions, [AreaIN ((0,0),(90,90))]))

-- Expected: ["Ross", "Queen", "Aberford"]
--mainShip = read shipPositions ((Ships, []), (Positions, []))

----------------------------------------------------------------------
-- Update tests

-- Expected: ["Ross, 80", "Queen, 33", "Aberford, 44"]
{-
mainShip = do
	update filteredShips [CapacityGT 80] [Ship {s_ship_name = "Queen", s_capacity = 33}, Ship {s_ship_name = "Aberford", s_capacity = 44}]
	read ships Ships
-}

-- Expected: ["Ross, 80", "Queen, 33"]
{-
mainShip = do
	update filteredShips [CapacityGT 80] [Ship {s_ship_name = "Queen", s_capacity = 33}]
	read ships Ships
-}

-- Expected: ["Ross, 80", "Admiral, 300"]
{-
mainShip = do
	update filteredShips [CapacityGT 80] [Ship {s_ship_name = "Admiral", s_capacity = 300}]
	read ships Ships
-}

-- Expected: ["Ross, 80"]
{-
mainShip = do
	update filteredShips [CapacityGT 80] []
	read ships Ships
-}

-- Expected: ["Ross, 33, (33,33)", "Queen, 100, (100,200)"]
{-
mainShip = do
	update shipPositions ((Ships, [CapacityGT 0]), (Positions, [AreaIN ((0,0),(90,90))])) [(Ship {s_ship_name = "Ross", s_capacity = 33}, Position {p_ship_name = "Ross", p_position = (33,33)})]
	read shipPositions ((Ships, []), (Positions, []))
-}

-- Expected: ["Ross, 33", "Queen, 100"]
{-
mainShip = do
	update shipPositions ((Ships, [CapacityGT 0]), (Positions, [AreaIN ((0,0),(90,90))])) [(Ship {s_ship_name = "Ross", s_capacity = 33}, Position {p_ship_name = "Ross", p_position = (33,33)})]
	read ships Ships
-}	
	
-- Expected: ["Ross, (33,33)", "Queen, (100,200)"]
{-
mainShip = do
	update shipPositions ((Ships, [CapacityGT 0]), (Positions, [AreaIN ((0,0),(90,90))])) [(Ship {s_ship_name = "Ross", s_capacity = 33}, Position {p_ship_name = "Ross", p_position = (33,33)})]
	read positions Positions
-}

----------------------------------------------------------------------
-- Notification tests

-- Expected: "542"
{-
mainShip = do
	observe positions Positions "1" (lift $ print "1")
	observe ships Ships "2" (lift $ print "2")
	observe filteredShips [CapacityGT 200] "3" (lift $ print "3")
	observe filteredShips [CapacityGT 100] "4" (lift $ print "4")
	observe filteredShips [SNameIN ["Queen", "Ross"]] "5" (lift $ print "5")
	observe filteredShips [SNameIN ["Ross"]] "6" (lift $ print "6")
	
	update filteredShips [CapacityGT 80] [Ship {s_ship_name = "Queen", s_capacity = 15}, Ship {s_ship_name = "Aberford", s_capacity = 150}]
-}

-- Expected (1. version): "8615432"
-- Expected (2. version): "861532"

mainShip = do
	observe positions Positions "1" (lift $ print "1")
	observe ships Ships "2" (lift $ print "2")
	observe filteredShips [SNameIN ["Ross"]] "3" (lift $ print "3")
	observe filteredShips [SNameIN ["Queen"]] "4" (lift $ print "4")
	observe filteredShips [SNameIN ["Aberford"]] "5" (lift $ print "5")
	observe filteredPositions [PNameIN ["Ross"]] "6" (lift $ print "6")
	observe filteredPositions [PNameIN ["Queen"]] "7" (lift $ print "7")
	observe filteredPositions [PNameIN ["Aberford"]] "8" (lift $ print "8")
	
	update shipPositions ((Ships, [CapacityGT 0]), (Positions, [AreaIN ((0,0),(90,90))])) [(Ship {s_ship_name = "Ross", s_capacity = 33}, Position {p_ship_name = "Ross", p_position = (33,33)})]

	
