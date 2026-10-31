# Reproducibility and review notes

## Preparation status

The seven supplied R scripts and report have been copied without changing their contents. All seven R files pass `parse()` checks. No full analysis run or independent verification of reported results has been completed because the processed input data is absent. No package lockfile was supplied, so exact historical package versions are unknown.

## Required input

`_targets.R` reads `cmcdataset.csv` from the working directory. The code references these case-sensitive columns:

| Column | Expected role |
| --- | --- |
| `cmc` | Numeric CMC score, with a modeled maximum of 64 |
| `MatchorNot` | Known class, using exactly `match` or `non-match` |
| `refBarrel` | Reference barrel identifier |
| `targBarrel` | Target barrel identifier |

These requirements are inferred from the scripts, not checked against a supplied CSV. Validate missing values, score ranges, labels, and barrel identifiers before running. Do not invent or relabel synthetic data as the original research data.

The source dataset cited in the report is not necessarily the processed CSV. Confirm the processing procedure and redistribution terms before adding data to the repository.

## Code details to review

- The SLR and Bayes factor validation targets classify a comparison as a match when `log(SLR) > 1` or `log(BF) > 1`. Their plots draw a reference line at 0. A cutoff of 1 on the log scale corresponds to a ratio above `exp(1)`; a ratio above 1 corresponds to a log cutoff of 0. Confirm the intended cutoff before revising or interpreting the results. These original decisions have been preserved.
- `results.R` hard-codes 1,497 rows when appending method labels, writes and reads `twostagest.csv`, and later expects a `decision.criteria` column not visibly created in that script's two-stage export. It needs review before it can be treated as a standalone post-processing workflow.
- `cmcn1.R` and `others.R` rely on pre-existing session objects, including `cmcnew`. The performance scripts use `cmcdata2`. They should not be sourced as a single clean-session workflow.
- Match/non-match estimates in parts of the pipeline are selected by aggregate row position. Confirm class ordering when inspecting or modifying the code.
- Fold test sets can share pairwise comparisons across held-out barrels. The combined test-set row count should not be interpreted as a count of independent observations.
- The input CSV is read within a target rather than declared as a file target. After replacing the CSV, use `targets::tar_invalidate(cmcdata)` before rebuilding to ensure its contents are reread.
- The report's first page contains unfilled journal-template date and DOI fields. It is presented here as the supplied creative-component report; no journal publication status or DOI is claimed.

## Before describing the project as fully reproducible

Obtain the processed dataset and its provenance; confirm the intended thresholds; reconcile post-processing with the pipeline outputs; run the pipeline in a clean environment; compare the resulting metrics with the report; then record package versions. Any changes to the statistical analysis should be documented separately from the original work.
