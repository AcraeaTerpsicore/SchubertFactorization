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
  RectangularObstructionQ, ElementarySymmetric,
  DiagonalSeparationPropertyQ, ConjectureSweep,
  BottomPipeDreamCrosses, NormalizePipeDream,
  SimpleLadderMoves, AllPipeDreams, PipeDreamColumns,
  ColumnIndependenceQ, PipeDreamPermutation
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
BottomPipeDreamCrosses[perm_List] := Module[
  {code = LehmerCode[perm]},
  Flatten[
    Table[
      Table[{row, col}, {col, 1, code[[row]]}],
      {row, Length[code]}
    ],
    1
  ]
];

BottomPipeDreamColumns[perm_List] := Module[
  {code = LehmerCode[perm], maxCol, cross, cols = {}, runs},
  maxCol = Max[code];
  cross = Table[j <= code[[i]], {i, Length[code]}, {j, maxCol}];
  Do[
    runs = SplitBy[Range[Length[code]], cross[[#, c]] &];
    Do[
      If[TrueQ[cross[[run[[1]], c]]],
        AppendTo[cols, <|
          "Column" -> c,
          "Rows" -> run,
          "Height" -> Length[run],
          "TopRow" -> First[run],
          "BottomRow" -> Last[run]
        |>]
      ],
      {run, runs}
    ],
    {c, 1, maxCol}
  ];
  SortBy[cols, {#["Column"] &, #["TopRow"] &}]
];

(* Pipe dream normalization and ladder moves *)
NormalizePipeDream[pd_List] := SortBy[pd, {First, Last}];

SimpleLadderMoves[pd_List] := Module[
  {crosses = AssociationThread[NormalizePipeDream[pd] -> True], moves = {}},
  Do[
    With[{r = rc[[1]], c = rc[[2]], target = {rc[[1]] - 1, rc[[2]] + 1}},
      If[r > 1 && !KeyExistsQ[crosses, target],
        AppendTo[moves, NormalizePipeDream[Append[DeleteCases[pd, rc], target]]]
      ]
    ],
    {rc, NormalizePipeDream[pd]}
  ];
  DeleteDuplicates[moves]
];

PipeDreamColumns[pd_List] := Module[
  {cols = GroupBy[pd, Last -> First], runs},
  KeyValueMap[
    Function[{col, rows},
      runs = Split[Sort[rows], #2 == #1 + 1 &];
      Table[
        <|
          "Column" -> col,
          "Rows" -> run,
          "Height" -> Length[run],
          "TopRow" -> First[run],
          "BottomRow" -> Last[run]
        |>,
        {run, runs}
      ]
    ],
    cols
  ] // Flatten // SortBy[{#["Column"] &, #["TopRow"] &}]
];

ColumnIndependenceQ[perm_List] := Module[
  {baselineHeights, dreams},
  dreams = AllPipeDreams[perm];
  baselineHeights = PipeDreamColumns[BottomPipeDreamCrosses[perm]][[All, "Height"]] // Sort;
  AllTrue[dreams,
    Sort[PipeDreamColumns[#][[All, "Height"]]] === baselineHeights &
  ]
];

AllPipeDreams[perm_List] := Module[
  {n = Length[perm], l = PermutationInversionNumber[perm], start,
   seen, queue, ladder, potentialCells, brute = {}, threshold = 200000},
  start = NormalizePipeDream[BottomPipeDreamCrosses[perm]];
  seen = <|ToString[start, InputForm] -> start|>;
  queue = {start};
  While[queue =!= {},
    With[{current = First[queue]},
      queue = Rest[queue];
      Do[
        If[! KeyExistsQ[seen, ToString[npd, InputForm]],
          seen[ToString[npd, InputForm]] = npd;
          queue = Append[queue, npd];
        ],
        {npd, SimpleLadderMoves[current]}
      ];
    ];
  ];
  ladder = Select[Values[seen], PipeDreamPermutation[#, n] === perm &];
  potentialCells = Flatten[
    Table[{r, c}, {r, 1, n}, {c, 1, n - 1}],
    1
  ];
  If[Binomial[Length[potentialCells], l] <= threshold,
    brute = Select[
      Subsets[potentialCells, {l}],
      PipeDreamPermutation[#, n] === perm &
    ];
  ];
  DeleteDuplicatesBy[Join[ladder, brute], ToString[#, InputForm] &]
];

PipeDreamPermutation[pd_List, n_Integer?Positive] := Module[
  {maxCol, crossSet, east, north, exitCols},
  maxCol = Max[Last /@ pd, n] + n;
  crossSet = AssociationThread[NormalizePipeDream[pd] -> True];
  east = ConstantArray[Null, {n, maxCol}];
  north = ConstantArray[Null, {n, maxCol}];
  Do[
    Do[
      Module[{west, south, crossQ},
        west = If[c == 1, r, east[[r, c - 1]]];
        south = If[r == n, Null, north[[r + 1, c]]];
        crossQ = KeyExistsQ[crossSet, {r, c}];
        If[crossQ,
          east[[r, c]] = west;
          north[[r, c]] = south,
          east[[r, c]] = south;
          north[[r, c]] = west
        ];
      ],
      {r, n, 1, -1}
    ],
    {c, 1, maxCol}
  ];
  exitCols = Table[
    With[{pos = FirstPosition[north[[1, All]], r, Missing["NotFound"]]},
      If[MissingQ[pos], Null, pos[[1]]]
    ],
    {r, 1, n}
  ];
  exitCols
];

(* Diagonal separation property from Lemma 2.3 *)
DiagonalSeparationPropertyQ[perm_List] := Module[
  {cols = Select[BottomPipeDreamColumns[perm], #["Height"] > 0 &]},
  AllTrue[
    Subsets[cols, {2}],
    Module[{c1 = #[[1]], c2 = #[[2]]},
      If[c1["Column"] == c2["Column"], True,
        If[c1["Column"] < c2["Column"],
          c1["TopRow"] + c1["Column"] > c2["BottomRow"] + c2["Column"],
          c2["TopRow"] + c2["Column"] > c1["BottomRow"] + c1["Column"]
        ]
      ]
    ] &
  ]
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
    diagOK = DiagonalSeparationPropertyQ[perm],
    schubert, predicted, matches
  },
  schubert = SchubertPolynomial[perm];
  predicted = If[patternOK && slopeOK && diagOK, FactorizationPolynomial[perm], Missing["NotApplicable"]];
  matches = If[MissingQ[predicted], False, Simplify[Expand[schubert - predicted] === 0]];
  <|
    "Permutation" -> perm,
    "PatternAvoidance" -> patternOK,
    "LehmerSlopeCondition" -> slopeOK,
    "DiagonalSeparationProperty" -> diagOK,
    "PredictedFactorization" -> predicted,
    "SchubertPolynomial" -> schubert,
    "MatchesPrediction" -> matches
  |>
];

(* Brute-force sweep over S_n to collect evidence for/against the conjecture. *)
ConjectureSweep[n_Integer?Positive] := Module[
  {perms = Permutations[Range[n]], certs, avoid, factorable, violations},
  certs = FactorizationCertificate /@ perms;
  avoid = Select[certs, #["PatternAvoidance"] &];
  factorable = Select[avoid, #["LehmerSlopeCondition"] && #["DiagonalSeparationProperty"] &];
  violations = Select[factorable, Not[#["MatchesPrediction"]] &];
  <|
    "n" -> n,
    "TotalPermutations" -> Length[perms],
    "PatternAvoiding" -> Length[avoid],
    "FactorableCandidates" -> Length[factorable],
    "MatchingFactorizations" -> Length[factorable] - Length[violations],
    "Violations" -> violations
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
