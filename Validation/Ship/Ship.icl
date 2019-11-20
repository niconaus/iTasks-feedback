implementation module Ship

import ideas
import MiniShip



// --- Program translated to DSL

shipSimulation :: TTree SimulationState
shipSimulation = Choice [ Seq [ Condition hasInventory (Leaf pickup)
		                      , shipSimulation]
		                , Seq [ Condition canExtinguish (Leaf extinguish)
		                      , shipSimulation]
		                ]

pickup :: Rule SimulationState
pickup = Rule "Pickup" applyPickup

extinguish :: Rule SimulationState
extinguish = Rule "extinguish" applyExtinguish		                


// --- tests

testAF x = allFirsts shipSimulation x
testBF x = bruteForceHint shipOnFire x shipSimulation

// ----- example problems

ship1   = [[back0]
           , [room01, room02, room03]
           , [corridor0]
           , [room04, room05, room06]
           , [room07, room08]
          ]

back0     = {number = 1,  exits = [South 2, South 3, South 4], has = NoItem, state = Normal}
room01    = {number = 2,  exits = [North 1, South 5], has = NoItem, state = Normal}
room02    = {number = 3,  exits = [North 1, South 5], has = NoItem, state = Normal}
room03    = {number = 4,  exits = [North 1, South 5], has = Extinguisher, state = Normal}
corridor0 = {number = 5,  exits = [ North 2, North 3, North 4, South 6, South 7, South 8], has = NoItem, state = Normal}
room04    = {number = 6,  exits = [North 5, South 9], has = NoItem, state = Normal}
room05    = {number = 7,  exits = [North 5, South 9], has = NoItem, state = Normal}
room06    = {number = 8,  exits = [North 5, South 10], has = NoItem, state = Normal}
room07    = {number = 9,  exits = [North 6, North 7], has = NoItem, state = Normal}
room08    = {number = 10, exits = [North 8], has = NoItem, state = Fire}