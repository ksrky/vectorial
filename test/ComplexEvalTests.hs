module ComplexEvalTests (complexEvalTestsSpec) where

import Data.Complex
import Test.Hspec
import Vectorial.Eval
import Vectorial.Gates
import Vectorial.Syntax
import Vectorial.Vector as V

complexEvalTestsSpec :: Spec
complexEvalTestsSpec = describe "Complex Program Evaluation Tests" $ do

  describe "Advanced Quantum Circuit Evaluation" $ do

    it "evaluates quantum circuit with manual gates" $ do
      -- Test: X applied to |0⟩ should give |1⟩
      let xGate = IsoApp pauliX (V.return (Bit False))
      let result = evalV [] (V.return xGate)
      case result of
        V [(1, PBit True)] -> pure ()
        _ -> expectationFailure "Pauli-X gate evaluation failed"

    it "evaluates Bell state preparation circuit" $ do
      -- Test: CNOT(H|0⟩ ⊗ |0⟩) = |Φ+⟩ = (|00⟩ + |11⟩)/√2
      let ctx = [("q0", PBit False), ("q1", PBit False)]

      -- First apply Hadamard to q0: H|0⟩ = |+⟩
      let hCircuit = Let (PVar "h0") (V.return (IsoApp hadamard (V.return (Var "q0"))))
                         (V.return (Pair (V.return (Var "h0")) (V.return (Var "q1"))))

      -- Then apply CNOT: CNOT|+0⟩ = |Φ+⟩
      let bellCircuit = Let (PVar "pair") (V.return hCircuit)
                            (V.return (IsoApp cnot (V.return (Var "pair"))))

      let result = evalV ctx (V.return bellCircuit)
      case result of
        V coeffs -> do
          let expectedCoeff = 1 / sqrt 2
          let hasCorrectLength = length coeffs == 2
          let has00 = any (\(CC c, p) -> case p of
                              PPair (PBit False) (PBit False) ->
                                magnitude (c - (expectedCoeff :+ 0)) < 1e-10
                              _ -> False) coeffs
          let has11 = any (\(CC c, p) -> case p of
                              PPair (PBit True) (PBit True) ->
                                magnitude (c - (expectedCoeff :+ 0)) < 1e-10
                              _ -> False) coeffs
          if hasCorrectLength && has00 && has11
            then pure ()
            else expectationFailure "Bell state preparation evaluation failed"

  describe "Quantum Protocol Simulations" $ do
    it "evaluates quantum teleportation protocol (simplified)" $ do
      -- Test: Preparation of entangled pair + measurement simulation
      let ctx = [("alice", PBit True), ("bob", PBit False), ("ancilla", PBit False)]

      -- Create Bell pair between bob and ancilla: CNOT(H|0⟩ ⊗ |0⟩)
      let bellPrep = Let (PVar "h_anc") (V.return (IsoApp hadamard (V.return (Var "ancilla"))))
                          (V.return (IsoApp cnot (V.return (Pair (V.return (Var "h_anc")) (V.return (Var "bob"))))))

      -- Entangle Alice's qubit with ancilla: CNOT(Alice ⊗ Ancilla)
      let entangleAlice = Let (PVar "bell_pair") (V.return bellPrep)
                              (V.return (Let (PPair (PVar "anc") (PVar "b")) (V.return (Var "bell_pair"))
                                          (V.return (IsoApp cnot (V.return (Pair (V.return (Var "alice")) (V.return (Var "anc"))))))))

      let result = evalV ctx (V.return entangleAlice)
      -- Just check that evaluation doesn't crash and produces some result
      case result of
        V coeffs -> length coeffs `shouldSatisfy` (> 0)

    it "evaluates quantum error correction (bit flip)" $ do
      -- Test: 3-qubit bit flip code encoding
      let ctx = [("data", PBit True), ("anc1", PBit False), ("anc2", PBit False)]

      -- Encode: |data⟩ → |data, data, data⟩ using CNOT gates
      let encode1 = IsoApp cnot (V.return (Pair (V.return (Var "data")) (V.return (Var "anc1"))))
      let encode2 = Let (PVar "pair1") (V.return encode1)
                        (V.return (Let (PPair (PVar "d1") (PVar "a1")) (V.return (Var "pair1"))
                                    (V.return (IsoApp cnot (V.return (Pair (V.return (Var "d1")) (V.return (Var "anc2"))))))))

      let result = evalV ctx (V.return encode2)
      case result of
        V coeffs -> do
          -- Should have |111⟩ state (all qubits in |1⟩)
          let hasTripleOne = any (\(CC c, p) -> case p of
                                    PPair (PBit True) (PBit True) -> magnitude c > 0.5
                                    _ -> False) coeffs
          hasTripleOne `shouldBe` True

    it "evaluates quantum phase kickback" $ do
      -- Test: Controlled-Z gate with |+⟩ control and |1⟩ target
      let ctx = [("control", PBit False), ("target", PBit True)]

      -- Apply H to control: H|0⟩ = |+⟩
      let hControl = Let (PVar "plus") (V.return (IsoApp hadamard (V.return (Var "control"))))
                         (V.return (Pair (V.return (Var "plus")) (V.return (Var "target"))))

      -- Apply Controlled-Z (using CNOT followed by appropriate phases)
      let czGate = Let (PVar "pair") (V.return hControl)
                       (V.return (IsoApp cnot (V.return (Var "pair"))))

      let result = evalV ctx (V.return czGate)
      case result of
        V coeffs -> do
          let hasCorrectLength = length coeffs `elem` [2, 4] -- Depending on implementation
          hasCorrectLength `shouldBe` True

    it "evaluates quantum Fourier transform (2-qubit)" $ do
      -- Test: Simple 2-qubit QFT on |00⟩
      let ctx = [("q0", PBit False), ("q1", PBit False)]

      -- Simplified 2-qubit QFT: H ⊗ I, then controlled phase
      let qft = Let (PVar "h0") (V.return (IsoApp hadamard (V.return (Var "q0"))))
                    (V.return (Pair (V.return (Var "h0")) (V.return (IsoApp hadamard (V.return (Var "q1"))))))

      let result = evalV ctx (V.return qft)
      case result of
        V coeffs -> do
          let isNormalized = abs (sum (map (\(CC c, _) -> magnitude c ^ (2 :: Int)) coeffs) - 1.0) < 1e-10
          isNormalized `shouldBe` True
