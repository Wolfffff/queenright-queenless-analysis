---
name: verify-code-manuscript-correspondence
description: Verify that all statistics in the manuscript and R2R match the output of the R analysis scripts
user-invocable: true
---

# Verify Code-Manuscript Correspondence

Run each stats-generating R script, compare output to the manuscript and R2R, and report any mismatches.

## Steps

### 1. Extract manuscript statistics

Read `manuscript/01_Article_MainText.tex` and extract all inline statistics: chi-squared values, p-values, z-values, t-values, beta coefficients, slopes, sample sizes, means, SDs.

### 2. Run all stats-generating scripts

Run each script from the repo root and capture output. Timeout: 2 minutes each. Run independent scripts in parallel where possible.

| Script | What it tests |
|--------|--------------|
| `scripts/manuscript/lmms_nwp.R` | Colony-level network params: transitivity, efficiency, assortativity, modularity, sum |
| `scripts/manuscript/lmms_qlw_v_inf.R` | Hub bee vs non-hub QL worker: degree, move_perc, N90, betweenness, clustering, ovary index |
| `scripts/manuscript/lmms_queen_v_nestmates.R` | Queen vs worker: degree, N90, initiation freq, move_perc, betweenness |
| `scripts/manuscript/lmms_queen_v_infl.R` | Queen vs hub bee comparisons |
| `scripts/manuscript/variance_analysis.R` | glmmTMB dispersion contrasts: degree and clustering variance |
| `scripts/manuscript/ovary_interactivity_interaction_model.R` | Ovary x condition interaction: beta, z, p, emtrends slopes |
| `scripts/manuscript/stats_modality.R` | Head-to-head vs head-to-body paired t-tests |

### 3. Cross-reference script output against manuscript

Known mappings (line numbers are approximate — search for the actual text):

**lmms_nwp.R** (lme4 fixed effects, t-stats reported as chi-squared in paper):
- Transitivity: `$\chi^2 = 19.7, p = 1.68*10^{-80}$`
- GlobalEfficiency: `$\chi^2 = 39.6, p=6.77*10^{-271}$`
- Assortativity: `$\chi^2 = 11.0, p = 1.22*10^{-27}$`
- Sum: `$\chi^2 = 46.4, p = \sim 0$`

**lmms_qlw_v_inf.R** (lme4 fixed effects):
- Degree: `$\chi^2=42.1, p= \sim 0$`
- move_perc: `$\chi^2=19.5, p=1.4*10^{-82}$`
- N90: `$\chi^2=-17.0, p=4.70*10^{-64}$`
- Betweenness: `$\chi^2=33.4, p=1.17*10^{-228}$`
- OvaryIndex: `$\chi^2=25.4, p=2.88*10^{-133}$`

**lmms_queen_v_nestmates.R** (lme4 fixed effects):
- Queen Degree: `$\chi^2=26.4, p=2.09*10^{-147}$`
- Queen N90: `$\chi^2=-12.8, p=2.39*10^{-37}$`
- Queen Initiation: `$\chi^2=-16.9, p=7.97*10^{-63}$`

**variance_analysis.R** (glmmTMB dispersion contrasts):
- Degree variance: dispersion contrast `t = -17.92`
- Clustering variance: dispersion contrast `t = -15.83`
- These appear in Fig 3 caption and body text

**ovary_interactivity_interaction_model.R** (glmmTMB):
- Interaction beta: `$\beta=10541$`, `$z=6.42$`, `$p=1.37 \times 10^{-10}$`
- Queenless slope: `8843`, SE `1072`
- Queenright slope: `-1698`, SE `1281`
- Pairwise: `$t=-6.42$`, `$p < 0.0001$`
- n=264

**stats_modality.R** (paired t-tests):
- QR workers: `t=61.21`
- QL workers: `t=81.67`
- Queens: `t=19.39`

### 4. Check R2R consistency

Read `manuscript/response_to_reviewers_body.tex` and verify that statistics in `\changed{}` blocks and `\response{}` blocks match both the script output and the manuscript text. Key stats to check:
- Dispersion contrast values (Reviewer 2, Comment 1)
- Ovary interaction model stats (Reviewer 1, Comment 4 / Reviewer 2, Comment 2)
- Hub bee Degree stat = 42.1 (Reviewer 1, Comment 3)
- Hub bee initiation rates: 48.4% vs 49.7% (Reviewer 1, Comments 3 & 6)
- All `\autocite{}` references resolve to valid bib keys

### 5. Report

Output a summary table:

```
| Script | Metric | Script Value | Manuscript Value | Line | Match |
|--------|--------|-------------|-----------------|------|-------|
```

Then list any mismatches with details. If everything matches, confirm full alignment.

## Important Notes
- The manuscript reports lme4 Wald test statistics as `$\chi^2$` — these correspond to the `statistic` (t-value) column in lmerTest tidy output
- glmmTMB reports z-values for fixed effects and t-values for emmeans contrasts
- Run all scripts from the repo root directory
- Compare values to 3 significant figures (rounding differences are OK)
- If a script errors, report the error rather than skipping
