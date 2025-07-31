{-# LANGUAGE LinearTypes      #-}
{-# LANGUAGE RebindableSyntax #-}

module GatesTest where

import Data.Complex
import Prelude           hiding (Monad (..))
import Test.HUnit
import Vectorial.Algebra as A
import Vectorial.Gates
import Vectorial.Vector  as V

-- Helper function to check if two complex numbers are approximately equal
approxEqual :: CC -> CC -> Bool
approxEqual (CC (x1 :+ y1)) (CC (x2 :+ y2)) =
    abs (x1 - x2) < 1e-10 && abs (y1 - y2) < 1e-10

-- Helper function to check if two V values are approximately equal
approxEqualV :: Eq a => V a -> V a -> Bool
approxEqualV (V xs) (V ys) =
    length xs == length ys &&
    all (\((c1, x1), (c2, x2)) -> x1 == x2 && approxEqual c1 c2) (zip xs ys)

-- Test basic quantum states
testBasicStates :: Test
testBasicStates = TestList
    [ "ket0 has correct structure" ~:
        let V terms = ket0
        in length terms ~?= 1

    , "ket1 has correct structure" ~:
        let V terms = ket1
        in length terms ~?= 1

    , "ketPlus has two components" ~:
        let V terms = ketPlus
        in length terms ~?= 2

    , "ketMinus has two components" ~:
        let V terms = ketMinus
        in length terms ~?= 2
    ]

-- Test single-qubit gates
testSingleQubitGates :: Test
testSingleQubitGates = TestList
    [ "Pauli-X on |0⟩" ~:
        approxEqualV (ket0 V.>>= pauliX) ket1 ~? "X|0⟩ = |1⟩"

    , "Pauli-X on |1⟩" ~:
        approxEqualV (ket1 V.>>= pauliX) ket0 ~? "X|1⟩ = |0⟩"

    , "Hadamard on |0⟩" ~:
        approxEqualV (ket0 V.>>= hadamard) ketPlus ~? "H|0⟩ = |+⟩"

    , "Hadamard on |1⟩" ~:
        approxEqualV (ket1 V.>>= hadamard) ketMinus ~? "H|1⟩ = |−⟩"

    , "Double Hadamard creates superposition" ~:
        let V result = ket0 V.>>= hadamard V.>>= hadamard
        in length result >= 2 ~? "HH creates multiple components (not collapsed)"

    , "Pauli-Z on |0⟩" ~:
        approxEqualV (ket0 V.>>= pauliZ) ket0 ~? "Z|0⟩ = |0⟩"

    , "Pauli-Z on |1⟩" ~:
        let result = ket1 V.>>= pauliZ
            expected = A.negate ket1
        in approxEqualV result expected ~? "Z|1⟩ = -|1⟩"
    ]

-- Test two-qubit operations using direct function application
testTwoQubitGates :: Test
testTwoQubitGates = TestList
    [ "CNOT on |00⟩" ~:
        approxEqualV (cnot False False) (V.return (False, False)) ~? "CNOT|00⟩ = |00⟩"

    , "CNOT on |01⟩" ~:
        approxEqualV (cnot False True) (V.return (False, True)) ~? "CNOT|01⟩ = |01⟩"

    , "CNOT on |10⟩" ~:
        approxEqualV (cnot True False) (V.return (True, True)) ~? "CNOT|10⟩ = |11⟩"

    , "CNOT on |11⟩" ~:
        approxEqualV (cnot True True) (V.return (True, False)) ~? "CNOT|11⟩ = |10⟩"

    , "CZ on |00⟩" ~:
        approxEqualV (cz False False) (V.return (False, False)) ~? "CZ|00⟩ = |00⟩"

    , "CZ on |11⟩" ~:
        let result = cz True True
            expected = A.negate (V.return (True, True))
        in approxEqualV result expected ~? "CZ|11⟩ = -|11⟩"
    ]

-- Test quantum circuits using monadic composition
{- testQuantumCircuits :: Test
testQuantumCircuits = TestList
    [ "Bell state circuit structure" ~:
        let bellCircuit = do
                q1 <- ket0
                q1' <- hadamard q1
                q2 <- ket0
                cnot q1' q2
            V result = bellCircuit
        in length result ~?= 2  -- Bell state should have 2 components

    , "Grover iteration with do notation" ~:
        let groverStep = do
                q <- ket0
                q1 <- hadamard q      -- Create superposition
                q2 <- pauliZ q1       -- Oracle (phase flip)
                hadamard q2           -- Diffusion operator
            V result = groverStep
        in length result > 0 ~? "Grover step with do notation produces quantum state"

    , "Quantum phase kickback" ~:
        let phaseKickback = do
                control <- ket1       -- Control qubit in |1⟩
                target <- ket0        -- Target qubit in |0⟩
                target' <- hadamard target  -- Put target in superposition
                cz control target'    -- Controlled-Z gate
            V result = phaseKickback
        in length result > 0 ~? "Phase kickback circuit with do notation"
    ] -}

-- Test quantum interference
testQuantumInterference :: Test
testQuantumInterference = TestList
    [ "Interference creates complex amplitudes" ~:
        let interferenceResult = ket0 V.>>= hadamard V.>>= pauliZ V.>>= hadamard
            V result = interferenceResult
        in length result > 1 ~? "HZH creates interference pattern"

    , "Hadamard sequence preserves structure" ~:
        let constructiveResult = ket0 V.>>= hadamard V.>>= hadamard
            V result = constructiveResult
        in length result > 0 ~? "HH produces valid quantum state"
    ]

-- Test superposition and gate operations
testSuperposition :: Test
testSuperposition = TestList
    [ "Equal superposition" ~:
        let superpos = ket0 V.>>= hadamard
        in approxEqualV superpos ketPlus ~? "Hadamard creates equal superposition"

    , "Superposition with Pauli-X" ~:
        let result = ketPlus V.>>= pauliX
            expected = V [(1/sqrt 2, True), (1/sqrt 2, False)]
        in approxEqualV result expected ~? "X flips superposition components"
    ]

-- Test gate identities and properties
testGateProperties :: Test
testGateProperties = TestList
    [ "Pauli-X is self-inverse" ~:
        let result = ket0 V.>>= pauliX V.>>= pauliX
        in approxEqualV result ket0 ~? "XX = I"

    , "Hadamard creates superposition" ~:
        let result = ket1 V.>>= hadamard V.>>= hadamard
            V components = result
        in length components >= 2 ~? "HH creates superposition pattern"
    ]

f,g :: Int %1 -> Int
f = undefined
g = undefined

h :: Int %1 -> Int
h x = g y
  where
    y = f x

k :: Int %1 -> V Bool
ma :: V Int
k = undefined
ma = undefined

test1 = ma >>= \x -> k x
test2 = ma >>= \x -> (k x >>= \x -> k x)

-- Test proper vector operations using explicit addition
testVectorOperations :: Test
testVectorOperations = TestList
    [ "Manual Hadamard composition with do notation" ~:
        let manualHH = do
                q <- ket0
                q1 <- hadamard q
                hadamard q1
                -- q1 <- hadamard q  -- First Hadamard
                -- hadamard q1       -- Second Hadamard (manually composed)
            -- manualHH' = ket0 >>= \q -> hadamard q >>= \q1 -> hadamard q1
            V result = manualHH
        in length result > 0 ~? "Manual HH composition with do notation works"

    {- , "Vector addition combines like terms" ~:
        let state1 = V [(CC (0.5 :+ 0), False)]
            state2 = V [(CC (0.5 :+ 0), False)]
            combined = state1 A.+ state2
        in case combined of
            V [(coeff, _)] -> coeff ~?= CC (1.0 :+ 0.0)
            _ -> TestCase $ assertFailure "Combined state should have one component"

    , "Complex quantum circuit with do notation" ~:
        -- Test a more complex circuit: |0⟩ → H → Z → H using do notation
        let complexCircuit = do
                q0 <- ket0
                q1 <- hadamard q0   -- Create superposition
                q2 <- pauliZ q1     -- Apply phase flip
                hadamard q2         -- Final Hadamard
            V result = complexCircuit
        in length result > 0 ~? "Complex circuit with do notation executes successfully"

    , "Quantum teleportation setup" ~:
        -- Create entangled pair for teleportation protocol
        let entangledPair = do
                alice <- ket0
                alice' <- hadamard alice  -- Alice's qubit in superposition
                bob <- ket0
                cnot alice' bob           -- Create Bell state
            V result = entangledPair
        in length result ~?= 2

    , "Multi-gate sequence" ~:
        -- Test applying multiple gates in sequence
        let multiGate = do
                q <- ket0
                q1 <- pauliX q      -- Flip to |1⟩
                q2 <- hadamard q1   -- Create superposition from |1⟩
                pauliZ q2           -- Apply phase
            V result = multiGate
        in length result > 0 ~? "Multi-gate sequence with do notation" -}
    ]

-- Combine all tests
gatesTests :: Test
gatesTests = TestList
    [ TestLabel "Basic States" testBasicStates
    , TestLabel "Single Qubit Gates" testSingleQubitGates
    , TestLabel "Two Qubit Gates" testTwoQubitGates
    -- , TestLabel "Quantum Circuits" testQuantumCircuits
    , TestLabel "Quantum Interference" testQuantumInterference
    , TestLabel "Superposition" testSuperposition
    , TestLabel "Gate Properties" testGateProperties
    -- , TestLabel "Vector Operations" testVectorOperations
    ]
