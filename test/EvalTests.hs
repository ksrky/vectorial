{-# LANGUAGE OverloadedStrings #-}

module EvalTests (evalTestsSpec) where

import Data.Complex
import Test.Hspec
import Vectorial.Eval
import Vectorial.Gates
import Vectorial.Syntax
import Vectorial.Vector as V

evalTestsSpec :: Spec
evalTestsSpec = describe "Eval Tests" $ do

  describe "Basic evaluation" $ do
    it "evaluates a bit correctly" $ do
      let result1 = eval [] (Bit True)
      let result2 = eval [] (Bit False)
      -- Simple structural check since we can't easily compare V Pattern
      case (result1, result2) of
        (V [(1, PBit True)], V [(1, PBit False)]) -> pure ()
        _ -> expectationFailure "Bit evaluation failed"

    it "evaluates a variable from context" $ do
      let ctx = [("x", PBit True)]
      let result = eval ctx (Var "x")
      case result of
        V [(1, PBit True)] -> pure ()
        _                  -> expectationFailure "Variable evaluation failed"

  describe "Gate evaluation tests" $ do
    it "applies Pauli-X gate correctly" $ do
      -- Pauli-X flips |0⟩ to |1⟩
      let input0 = V.return (Bit False)
      let result0 = evalV [] (V.return (IsoApp pauliX input0))
      case result0 of
        V [(1, PBit True)] -> pure ()
        _                  -> expectationFailure "Pauli-X on |0⟩ failed"

      -- Pauli-X flips |1⟩ to |0⟩
      let input1 = V.return (Bit True)
      let result1 = evalV [] (V.return (IsoApp pauliX input1))
      case result1 of
        V [(1, PBit False)] -> pure ()
        _                   -> expectationFailure "Pauli-X on |1⟩ failed"

    it "applies Hadamard gate correctly" $ do
      -- H|0⟩ = |+⟩ = (|0⟩ + |1⟩)/√2
      let input0 = V.return (Bit False)
      let result0 = evalV [] (V.return (IsoApp hadamard input0))
      case result0 of
        V coeffs -> do
          let expectedCoeff = 1 / sqrt 2
          let hasCorrectLength = length coeffs == 2
          let checkCoeff (CC c) expected = abs (realPart c - expected) < 1e-10
          let hasFalseCoeff = any (\(cc, p) -> case p of
                                      PBit False -> checkCoeff cc expectedCoeff
                                      _          -> False) coeffs
          let hasTrueCoeff = any (\(cc, p) -> case p of
                                     PBit True -> checkCoeff cc expectedCoeff
                                     _         -> False) coeffs
          if hasCorrectLength && hasFalseCoeff && hasTrueCoeff
            then pure ()
            else expectationFailure "Hadamard on |0⟩ failed: wrong coefficients"

      -- H|1⟩ = |-⟩ = (|0⟩ - |1⟩)/√2
      let input1 = V.return (Bit True)
      let result1 = evalV [] (V.return (IsoApp hadamard input1))
      case result1 of
        V coeffs -> do
          let expectedCoeff = 1 / sqrt 2
          let hasCorrectLength = length coeffs == 2
          let checkCoeff (CC c) expected = abs (realPart c - expected) < 1e-10
          let hasFalseCoeff = any (\(cc, p) -> case p of
                                      PBit False -> checkCoeff cc expectedCoeff
                                      _          -> False) coeffs
          let hasTrueCoeff = any (\(cc, p) -> case p of
                                     PBit True -> checkCoeff cc (-expectedCoeff)
                                     _         -> False) coeffs
          if hasCorrectLength && hasFalseCoeff && hasTrueCoeff
            then pure ()
            else expectationFailure "Hadamard on |1⟩ failed: wrong coefficients"

    it "applies Phase S gate correctly" $ do
      -- S|0⟩ = |0⟩
      let input0 = V.return (Bit False)
      let result0 = evalV [] (V.return (IsoApp phaseS input0))
      case result0 of
        V [(1, PBit False)] -> pure ()
        _                   -> expectationFailure "Phase S on |0⟩ failed"

      -- S|1⟩ = i|1⟩ (complex phase)
      let input1 = V.return (Bit True)
      let result1 = evalV [] (V.return (IsoApp phaseS input1))
      case result1 of
        V coeffs -> do
          let expectedPhase = exp (0 :+ pi / 2) -- i = e^(iπ/2)
          let hasCorrectLength = length coeffs == 1
          let hasCorrectPhase = any (\(CC c, p) -> case p of
                                        PBit True -> magnitude (c - expectedPhase) < 1e-10
                                        _         -> False) coeffs
          if hasCorrectLength && hasCorrectPhase
            then pure ()
            else expectationFailure "Phase S on |1⟩ failed: wrong phase"

  describe "CNOT gate tests" $ do
    it "applies CNOT gate correctly to |00⟩" $ do
      let input = V.return (Pair (V.return (Bit False)) (V.return (Bit False)))
      let result = evalV [] (V.return (IsoApp cnot input))
      case result of
        V [(1, PPair (PBit False) (PBit False))] -> pure ()
        _ -> expectationFailure "CNOT on |00⟩ failed"

    it "applies CNOT gate correctly to |11⟩" $ do
      let input = V.return (Pair (V.return (Bit True)) (V.return (Bit True)))
      let result = evalV [] (V.return (IsoApp cnot input))
      case result of
        V [(1, PPair (PBit True) (PBit False))] -> pure ()
        _ -> expectationFailure "CNOT on |11⟩ failed"

  describe "Let binding tests" $ do
    it "evaluates let binding with variable pattern" $ do
      -- let x = |0⟩ in x
      let input = V.return (Bit False)
      let letExpr = Let (PVar "x") input (V.return (Var "x"))
      let result = evalV [] (V.return letExpr)
      case result of
        V [(1, PBit False)] -> pure ()
        _ -> expectationFailure "Let binding with variable pattern failed"

    it "evaluates let binding with bit pattern" $ do
      -- let False = |0⟩ in |1⟩
      let input = V.return (Bit False)
      let output = V.return (Bit True)
      let letExpr = Let (PBit False) input output
      let result = evalV [] (V.return letExpr)
      case result of
        V [(1, PBit True)] -> pure ()
        _ -> expectationFailure "Let binding with bit pattern failed"

    it "evaluates let binding with pair pattern" $ do
      -- let (x, y) = (|0⟩, |1⟩) in x
      let pairInput = Pair (V.return (Bit False)) (V.return (Bit True))
      let letExpr = Let (PPair (PVar "x") (PVar "y")) (V.return pairInput) (V.return (Var "x"))
      let result = evalV [] (V.return letExpr)
      case result of
        V [(1, PBit False)] -> pure ()
        _ -> expectationFailure "Let binding with pair pattern failed"

  describe "Complex circuit tests" $ do
    it "applies multiple Pauli-X gates" $ do
      -- X(X|0⟩) = |0⟩
      let input = V.return (Bit False)
      let firstX = IsoApp pauliX input
      let secondX = IsoApp pauliX (V.return firstX)
      let result = evalV [] (V.return secondX)
      case result of
        V [(1, PBit False)] -> pure ()
        _ -> expectationFailure "Double Pauli-X application failed"

    it "applies Pauli-Z to different states" $ do
      -- Z|0⟩ = |0⟩
      let input0 = V.return (Bit False)
      let result0 = evalV [] (V.return (IsoApp pauliZ input0))
      case result0 of
        V [(1, PBit False)] -> pure ()
        _                   -> expectationFailure "Pauli-Z on |0⟩ failed"

      -- Z|1⟩ = |1⟩ (in computational basis)
      let input1 = V.return (Bit True)
      let result1 = evalV [] (V.return (IsoApp pauliZ input1))
      case result1 of
        V [(1, PBit True)] -> pure ()
        _                  -> expectationFailure "Pauli-Z on |1⟩ failed"

    it "applies CNOT to |01⟩ and |10⟩" $ do
      -- CNOT|01⟩ = |01⟩
      let input01 = V.return (Pair (V.return (Bit False)) (V.return (Bit True)))
      let result01 = evalV [] (V.return (IsoApp cnot input01))
      case result01 of
        V [(1, PPair (PBit False) (PBit True))] -> pure ()
        _ -> expectationFailure "CNOT on |01⟩ failed"

      -- CNOT|10⟩ = |11⟩
      let input10 = V.return (Pair (V.return (Bit True)) (V.return (Bit False)))
      let result10 = evalV [] (V.return (IsoApp cnot input10))
      case result10 of
        V [(1, PPair (PBit True) (PBit True))] -> pure ()
        _ -> expectationFailure "CNOT on |10⟩ failed"

  describe "Superposition tests" $ do
    it "evaluates Bell state preparation correctly" $ do
      -- Create |Φ+⟩ = (|00⟩ + |11⟩)/√2 using H⊗I followed by CNOT
      -- First apply H to first qubit: H|0⟩ = |+⟩ = (|0⟩ + |1⟩)/√2
      let ctx = [("q0", PBit False), ("q1", PBit False)]
      let hOnFirst = IsoApp hadamard (V.return (Var "q0"))
      let afterH = Let (PVar "h0") (V.return hOnFirst)
                       (V.return (Pair (V.return (Var "h0")) (V.return (Var "q1"))))
      let bellCircuit = Let (PVar "pair") (V.return afterH)
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
            else expectationFailure "Bell state preparation failed"

    it "verifies superposition normalization" $ do
      -- Check that |+⟩ state has normalized coefficients
      let input = V.return (Bit False)
      let result = evalV [] (V.return (IsoApp hadamard input))
      case result of
        V coeffs -> do
          let totalProbability = sum $ map (\(CC c, _) -> magnitude c ^ (2 :: Int)) coeffs
          let isNormalized = abs (totalProbability - 1.0) < 1e-10
          if isNormalized
            then pure ()
            else expectationFailure $ "Superposition not normalized: " ++ show totalProbability

    it "applies X gate to superposition state" $ do
      -- X|+⟩ = X((|0⟩ + |1⟩)/√2) = (|1⟩ + |0⟩)/√2 = |+⟩
      let ctx = [("plus", PBit False)]
      let hGate = IsoApp hadamard (V.return (Var "plus"))
      let circuit = Let (PVar "superpos") (V.return hGate)
                        (V.return (IsoApp pauliX (V.return (Var "superpos"))))
      let result = evalV ctx (V.return circuit)

      case result of
        V coeffs -> do
          let expectedCoeff = 1 / sqrt 2
          let hasCorrectLength = length coeffs == 2
          let hasBothBits =
                any (\(CC c, p) -> case p of
                        PBit False -> magnitude (c - (expectedCoeff :+ 0)) < 1e-10
                        _          -> False) coeffs &&
                any (\(CC c, p) -> case p of
                        PBit True -> magnitude (c - (expectedCoeff :+ 0)) < 1e-10
                        _         -> False) coeffs
          if hasCorrectLength && hasBothBits
            then pure ()
            else expectationFailure "X gate on superposition failed"
