(* SchubertFactorization.wl
   Utilities for computing Schubert polynomials, checking the pattern-avoidance
   criterion from the accompanying writeup, and constructing the predicted
   factorization into elementary symmetric polynomials when it applies. *)

ClearAll[
  xVar, xVars, LehmerCode, PermutationInversionNumber,
  InversePermutationList, SimpleTransposition,
  BubbleReducedWord, DividedDifference, SchubertSeed,
  SchubertPolynomial, PatternOfList, AvoidsPatternQ,
  AvoidsPatternsQ, LehmerSlopeConditionQ, RequiredPatterns,
  AvoidsFactorizationPatternsQ, BottomPipeDreamColumns,
  ColumnFactors, FactorizationPolynomial, FactorizationCertificate,
  RectangularObstructionQ, ElementarySymmetric
];

(* Symbol for x_i *)
xVar[i_Integer] := Symbol["x" <> ToString[i]];
xVars[n_Integer] := Table[xVar[i], {i, 1, n}];

ElementarySymmetric[k_Integer, vars_List] /; k >= 0 := If[
  k == 0, 1,
  Total[Times @@@ Subsets[vars, {k}]]
];

(* Lehmer code and inversion count *)
LehmerCode[perm_List] /; VectorQ[perm, IntegerQ] := Table[
  Count[perm[[i + 1 ;;]], _?(# < perm[[i]] &)],
  {i, 1, Length[perm]}
];

PermutationInversionNumber[perm_List] := Total[LehmerCode[perm]];

(* Basic permutation utilities *)
InversePermutationList[perm_List] := Module[{n = Length[perm], inv},
  inv = ConstantArray[0, n];
  Do[
    inv[[perm[[i]]]] = i,
    {i, 1, n}
  ];
  inv
];

SimpleTransposition[perm_List, i_Integer] := Module[{p = perm},
  p[[{i, i + 1}]] = p[[{i + 1, i}]];
  p
];

(* Reduced word via bubble sort (each swap drops inversion count by 1) *)
BubbleReducedWord[perm_List] := Module[{p = perm, swaps = {}, n = Length[perm]},
  Do[
    Do[
      If[p[[j]] > p[[j + 1]],
        AppendTo[swaps, j];
        p = SimpleTransposition[p, j];
      ],
      {j, 1, n - i}
    ],
    {i, 1, n - 1}
  ];
  swaps
];

(* Divided-difference operator acting on polynomials *)
DividedDifference[poly_, i_Integer] := Module[
  {xi = xVar[i], xip1 = xVar[i + 1]},
  Expand[Cancel[(poly - (poly /. {xi -> xip1, xip1 -> xi}))/(xi - xip1)]]
];

(* Schubert polynomial using the standard definition
   S_w = ∂_{w^{-1} w0}(x1^(n-1) x2^(n-2) ... x_{n-1}) *)
SchubertSeed[n_Integer] := Times @@ Table[xVar[i]^(n - i), {i, 1, n}];

SchubertPolynomial[perm_List] /; VectorQ[perm, IntegerQ] := Module[
  {n = Length[perm], w0, inv, u, word, poly},
  w0 = Reverse[Range[n]];
  inv = InversePermutationList[perm];
  u = inv[[w0]]; (* u = w^{-1} w0 as a list *)
  word = BubbleReducedWord[u];
  poly = SchubertSeed[n];
  Do[
    poly = DividedDifference[poly, i],
    {i, word}
  ];
  Expand[poly]
];

(* Pattern-avoidance utilities *)
PatternOfList[list_List] := Ordering[Ordering[list]];

AvoidsPatternQ[perm_List, pattern_List] := Module[{k = Length[pattern], positions},
  positions = Subsets[Range[Length[perm]], {k}];
  NoneTrue[positions, PatternOfList[perm[[#]]] === pattern &]
];

AvoidsPatternsQ[perm_List, patterns_List] :=
  AllTrue[patterns, AvoidsPatternQ[perm, #] &];

RequiredPatterns = {{1, 4, 3, 2}, {1, 4, 2, 3}, {4, 1, 3, 2}, {3, 1, 4, 2}};

AvoidsFactorizationPatternsQ[perm_List] := AvoidsPatternsQ[perm, RequiredPatterns];

(* Lehmer code slope condition from Lemma 2.1 *)
LehmerSlopeConditionQ[perm_List] := Module[{code = LehmerCode[perm]},
  Max[Differences[code]] <= 1
];

(* Column information extracted from the bottom pipe dream (left-justified) *)
BottomPipeDreamColumns[perm_List] := Module[
  {code = LehmerCode[perm], maxCol, cols},
  maxCol = Max[code];
  cols = Table[
    Module[{rows = Flatten[Position[code, x_ /; x >= c]]},
      <|
        "Column" -> c,
        "Rows" -> rows,
        "Height" -> Length[rows],
        "TopRow" -> If[rows === {}, Missing["Empty"], Min[rows]],
        "BottomRow" -> If[rows === {}, Missing["Empty"], Max[rows]]
      |>
    ],
    {c, 1, maxCol}
  ];
  cols
];

ColumnFactors[perm_List] := Module[
  {cols = BottomPipeDreamColumns[perm]},
  DeleteCases[
    cols /. assoc_Association :> Append[
      assoc,
      "Factor" -> ElementarySymmetric[
        assoc["Height"],
        xVars[assoc["BottomRow"]]
      ]
    ],
    assoc_Association /; assoc["Height"] == 0
  ]
];

FactorizationPolynomial[perm_List] := Module[
  {factors = ColumnFactors[perm]},
  If[factors === {}, 1, Expand[Times @@ (factors[[All, "Factor"]])]]
];

(* Summarize conditions and compare predicted factorization against
   a direct Schubert polynomial computation. *)
FactorizationCertificate[perm_List] := Module[
  {
    patternOK = AvoidsFactorizationPatternsQ[perm],
    slopeOK = LehmerSlopeConditionQ[perm],
    schubert, predicted, matches
  },
  schubert = SchubertPolynomial[perm];
  predicted = If[patternOK && slopeOK, FactorizationPolynomial[perm], Missing["NotApplicable"]];
  matches = If[MissingQ[predicted], False, Simplify[Expand[schubert - predicted] === 0]];
  <|
    "Permutation" -> perm,
    "PatternAvoidance" -> patternOK,
    "LehmerSlopeCondition" -> slopeOK,
    "PredictedFactorization" -> predicted,
    "SchubertPolynomial" -> schubert,
    "MatchesPrediction" -> matches
  |>
];

(* Obstruction described in Section 3: a rectangular block in the Lehmer code. *)
RectangularObstructionQ[perm_List] := Module[
  {code = LehmerCode[perm], pos, firstPos, lastPos, block, nVal,
   prefixZeroQ, suffixZeroQ, blockSameQ},
  pos = Flatten@Position[code, _?(# > 0 &)];
  If[pos === {}, Return[False]];
  firstPos = Min[pos]; lastPos = Max[pos];
  block = code[[firstPos ;; lastPos]];
  nVal = First[Select[block, # > 0 &]];
  blockSameQ = AllTrue[block, # == nVal &];
  prefixZeroQ = If[firstPos == 1, True, AllTrue[code[[;; firstPos - 1]], # == 0 &]];
  suffixZeroQ = If[lastPos == Length[code], True, AllTrue[code[[lastPos + 1 ;;]], # == 0 &]];
  blockSameQ && prefixZeroQ && suffixZeroQ
];
