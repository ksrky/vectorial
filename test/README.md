# Quantum Computing Test Examples

This directory contains comprehensive test examples demonstrating how to use the `Gates.hs` API to execute quantum computations within the V monad.

## Test Categories

### Basic States (`testBasicStates`)
Tests the fundamental quantum states:
- `ket0`: |0⟩ state  
- `ket1`: |1⟩ state
- `ketPlus`: |+⟩ = (|0⟩ + |1⟩)/√2 state
- `ketMinus`: |−⟩ = (|0⟩ - |1⟩)/√2 state

### Single Qubit Gates (`testSingleQubitGates`)
Tests individual quantum gates:
- **Pauli-X**: Bit flip gate (X|0⟩ = |1⟩, X|1⟩ = |0⟩)
- **Hadamard**: Creates superposition (H|0⟩ = |+⟩, H|1⟩ = |−⟩)  
- **Pauli-Z**: Phase flip gate (Z|0⟩ = |0⟩, Z|1⟩ = -|1⟩)

### Two Qubit Gates (`testTwoQubitGates`)
Tests two-qubit operations:
- **CNOT**: Controlled-NOT gate (flips target if control is |1⟩)
- **CZ**: Controlled-Z gate (applies phase if both qubits are |1⟩)

### Quantum Circuits (`testQuantumCircuits`)
Tests combinations of gates:
- **Bell State Preparation**: Creating entangled states using H⊗I followed by CNOT
- **Grover Iteration**: H→Z→H sequence demonstrating quantum interference

### Quantum Interference (`testQuantumInterference`)
Tests interference effects:
- **Interference Patterns**: How quantum amplitudes combine
- **Phase Relationships**: Effects of phase gates on superposition states

### Superposition (`testSuperposition`)
Tests superposition states and operations:
- **Equal Superposition**: Testing |+⟩ state properties
- **Gate Operations on Superposition**: How gates transform superposed states

### Gate Properties (`testGateProperties`)
Tests mathematical properties:
- **Self-Inverse Gates**: XX = I, verifying gate inverses
- **Unitary Properties**: Preservation of quantum state structure

### Vector Operations (`testVectorOperations`)
Tests the underlying vector algebra:
- **Manual Composition**: Explicitly combining quantum operations using vector addition
- **Like Term Combination**: How coefficients combine for identical states
- **Complex Circuits**: Multi-step quantum algorithms

## Usage Examples

### Basic Gate Application
```haskell
-- Apply Pauli-X to |0⟩
result = ket0 V.>>= pauliX
-- Result: |1⟩

-- Create superposition with Hadamard
superposition = ket0 V.>>= hadamard  
-- Result: |+⟩ = (|0⟩ + |1⟩)/√2
```

### Circuit Composition
```haskell
-- Bell state preparation
bellState = ket0 V.>>= hadamard V.>>= \q1 -> 
           ket0 V.>>= \q2 -> 
           cnot q1 q2
-- Result: (|00⟩ + |11⟩)/√2
```

### Manual Vector Operations
```haskell
-- Proper amplitude combination using explicit addition
let V [(c1, v1), (c2, v2)] = ketPlus
    result1 = c1 A.*> hadamard v1  
    result2 = c2 A.*> hadamard v2
    combined = result1 A.+ result2
-- Result: Properly combined quantum state
```

## Understanding the V Monad

The V monad represents quantum states as linear combinations of basis states:
- `V [(coefficient, basis_state)]`
- Monadic bind (`>>=`) applies quantum operations
- Vector addition (`A.+`) combines quantum amplitudes
- Scalar multiplication (`A.*>`) scales amplitudes

## Running the Tests

```bash
cabal test
```

This will run all 28 quantum computing tests, verifying the correct behavior of quantum gates and circuits within the V monad framework.
