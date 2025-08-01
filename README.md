# Vectorial

A Haskell library for quantum circuit simulation using Linear Types and a vector-based approach.

## Features

- **Linear Types**: Safe resource management using Haskell's Linear Types
- **Quantum Gates**: Pauli-X, Pauli-Z, Hadamard, Phase S/T, CNOT, CZ gates
- **Vector-based Quantum States**: Complex amplitude representation with automatic simplification
- **Monadic Quantum Circuits**: Compose quantum circuits using do-notation
- **Floating Point Precision**: Robust handling of numerical precision in quantum computations

## Example Usage

```haskell
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
import Vectorial.Vector as V

-- Basic quantum states
ket0, ket1 :: V Bool
ket0 = V.return False  -- |0⟩
ket1 = V.return True   -- |1⟩

-- Basic quantum gates
hadamard :: Bool %1 -> V Bool
hadamard False = V.V [(1 / sqrt 2, False), (1 / sqrt 2, True)]
hadamard True  = V.V [(1 / sqrt 2, False), (- 1 / sqrt 2, True)]

pauliX :: Bool %1 -> V Bool
pauliX False = V.return True
pauliX True  = V.return False

cnot :: Bool %1 -> Bool %1 -> V (Bool, Bool)
cnot False False = V.return (False, False)
cnot False True  = V.return (False, True)
cnot True False  = V.return (True, True)
cnot True True   = V.return (True, False)

-- Bell State Creation (Quantum Entanglement)
bellState :: V (Bool, Bool)
bellState = V.do
    q1 <- ket0          -- First qubit in |0⟩
    q1' <- hadamard q1  -- Apply Hadamard: |0⟩ → |+⟩
    q2 <- ket0          -- Second qubit in |0⟩
    cnot q1' q2         -- Apply CNOT: |+⟩⊗|0⟩ → (|00⟩ + |11⟩)/√2
-- Result: V [(0.7071, (False,False)), (0.7071, (True,True))]

```
