# Vectorial

A Haskell library for quantum circuit simulation using a vector-based approach.

## Features

- Quantum gate operations (Pauli-X, Pauli-Z, Hadamard, Phase S, CNOT)
- Vector-based quantum state representation
- Let binding with pattern matching
- Evaluation of quantum circuits

## Building and Testing

To build the project:
```bash
cabal build
```

To run tests:
```bash
cabal test
```

## Test Coverage

The test suite is organized into three modules:

### EvalTests Module
**Basic Evaluation Tests**
- Bit evaluation
- Variable evaluation from context

**Gate Evaluation Tests**
- Pauli-X gate (bit flip)
- Hadamard gate (superposition creation with coefficient verification)
- Phase S gate (complex phase rotation with i coefficient verification)

**CNOT Gate Tests**
- All four computational basis states (|00⟩, |01⟩, |10⟩, |11⟩)

**Let Binding Tests**
- Variable pattern matching
- Bit pattern matching
- Pair pattern matching

**Complex Circuit Tests**
- Multiple gate applications
- Various gate combinations

**Superposition Tests**
- **Bell state preparation**: Creates |Φ+⟩ = (|00⟩ + |11⟩)/√2 and verifies coefficients
- **Superposition normalization**: Verifies that quantum states remain normalized
- **Gates on superposition states**: Tests X gate application to |+⟩ state

### ParserTests Module
**Basic Syntax Parsing**
- Bit literals, variables, let expressions, pairs
- Complex program structures
- Nested expressions and pattern matching
- Error handling

**Parser Syntax Tests**
- Parentheses handling
- Complex variable names
- Multiple coefficient terms

**Pattern Parsing Tests**
- Simple and nested pattern structures
- Mixed bit and variable patterns
- Complex pair patterns

**Quantum Algorithm Examples**
- Quantum Teleportation structure parsing
- Deutsch-Jozsa algorithm components
- Grover's algorithm iteration patterns
- Quantum error correction syntax

### ComplexEvalTests Module
**Advanced Quantum Circuit Evaluation**
- Superposition creation with coefficient verification
- Manual gate applications with complex circuits
- Bell state preparation with complete evaluation

**Quantum Protocol Simulations**
- **Quantum Teleportation Protocol**: Simplified protocol with multi-qubit operations
- **Quantum Error Correction**: 3-qubit bit flip code encoding
- **Quantum Phase Kickback**: Controlled operations with phase effects
- **Quantum Fourier Transform**: 2-qubit QFT with normalization verification

All tests use the Hspec testing framework and verify:
- Correct evaluation of quantum circuits using `eval` and `evalV` functions
- Proper coefficient values in superposition states (1/√2 for Hadamard gates)
- Complex phase factors (i for Phase S gate)
- Quantum state normalization
- Bell state entanglement preparation
- **Parser correctness**: Syntax tree generation from text input
- **Complex program evaluation**: Multi-step quantum algorithms

## Example Usage

```haskell
import Vectorial.Eval
import Vectorial.Gates
import Vectorial.Syntax
import Vectorial.Vector

-- Apply Pauli-X gate to |0⟩
let input = V [(1, Bit False)]
let result = evalV [] (V [(1, IsoApp pauliX input)])
-- Result: V [(1, PBit True)]

-- Apply Hadamard gate to |0⟩
let input = V [(1, Bit False)]
let result = evalV [] (V [(1, IsoApp hadamard input)])
-- Result: ketPlus = V [(1/√2, PBit False), (1/√2, PBit True)]
```
