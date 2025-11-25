#!/usr/bin/env wolframscript

Get["src/SchubertFactorization.wl"];

Clear[assert];
assert[label_, cond_] := Module[{ok = TrueQ[cond]},
  Print[label <> ": " <> If[ok, "PASS", "FAIL"]];
  ok
];

results = {
  Module[{expected, actual},
    expected = xVar[2]^2*xVar[3] + xVar[1]*xVar[2]*xVar[3] + xVar[1]^2*xVar[3] +
      xVar[1]^2*xVar[2] + xVar[1]*xVar[2]^2;
    actual = SchubertPolynomial[{1, 4, 3, 2}];
    assert["SchubertPolynomial[{1,4,3,2}]", Expand[expected - actual] === 0]
  ],
  Module[{cert = FactorizationCertificate[{1, 2, 4, 3}]},
    assert["FactorizationCertificate[{1,2,4,3}] agrees",
      cert["PatternAvoidance"] && cert["LehmerSlopeCondition"] && cert["MatchesPrediction"]
    ]
  ],
  Module[{cert = FactorizationCertificate[{2, 1, 3, 4}]},
    assert["Simple transposition factors (s1)", cert["MatchesPrediction"]]
  ],
  Module[{obstruct = RectangularObstructionQ[{3, 4, 5, 1, 2}]},
    assert["Rectangular obstruction detected for {3,4,5,1,2}", obstruct]
  ],
  assert["Pattern avoidance fails on 1432", Not@AvoidsFactorizationPatternsQ[{1, 4, 3, 2}]],
  assert["Diagonal separation holds for {1,2,4,3}", DiagonalSeparationPropertyQ[{1, 2, 4, 3}]],
  assert["Diagonal separation fails for {1,4,3,2}", Not@DiagonalSeparationPropertyQ[{1, 4, 3, 2}]],
  Module[{sweep = ConjectureSweep[4]},
    assert["Conjecture sweep n=4 has no violations", sweep["Violations"] === {}]
  ],
  Module[{dreams = AllPipeDreams[{1, 4, 3, 2}]},
    assert["AllPipeDreams cardinality for 1432", Length[dreams] == 5]
  ],
  Module[{dreams = AllPipeDreams[{1, 2, 4, 3}], heights},
    heights = PipeDreamColumns[BottomPipeDreamCrosses[{1, 2, 4, 3}]][[All, "Height"]] // Sort;
    assert["ColumnIndependenceQ holds on pattern-avoiding example", ColumnIndependenceQ[{1, 2, 4, 3}] && And @@ (Sort[PipeDreamColumns[#][[All, "Height"]]] == heights & /@ dreams)]
  ]
};

If[AllTrue[results, TrueQ],
  Print["All tests passed."];
  Exit[0],
  Print["At least one test failed."];
  Exit[1]
];
