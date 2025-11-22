# Schubert Polynomial Factorization (WL)

- Wolfram Language utilities to compute Schubert polynomials, check the pattern-avoidance criterion ($1432$, $1423$, $4132$, $3142$), and build the predicted elementary-symmetric factorization described in the reference writeup.
- Core code: `src/SchubertFactorization.wl` exposing `SchubertPolynomial`, `LehmerCode`, `AvoidsFactorizationPatternsQ`, `FactorizationCertificate`, `RectangularObstructionQ`, and helper utilities.
- Tests: `tests/run_tests.wls` (uses the Windows install of wolframscript) plus the recorded results in `TEST_SUMMARY.md`.

- Reference: [SCHUBERT POLYNOMIALS AND ELEMENTARY SYMMETRIC PRODUCTS](https://arxiv.org/abs/2511.15920)


## Usage
- Run tests from this folder:  
  `/mnt/d/Software/Wolfram\\ Research/Mathematica/14.0/wolframscript.exe -script tests/run_tests.wls`
- Load the library in your own script:  
  ```
  Get["/mnt/d/ExplorerDownload/arXiv-2511.15920v1/src/SchubertFactorization.wl"];
  SchubertPolynomial[{1, 4, 3, 2}]
  FactorizationCertificate[{1, 2, 4, 3}]
  ```
- The factorization certificate reports the pattern-avoidance check, the Lehmer slope condition, the predicted product of $e_k(x_1,\dots,x_m)$ factors, and whether it matches a direct Schubert polynomial computation.

## Files
- `src/SchubertFactorization.wl` – Wolfram Language implementation.
- `tests/run_tests.wls` – basic regression checks.
- `FORMULAS.md` – formulas used by the code in LaTeX-friendly form.
- `TEST_SUMMARY.md` – commands run and results.
- `.gitignore` – ignores `reference_paper/` as requested.
