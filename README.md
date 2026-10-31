# Statistical Methods for Assessing Strength of Cartridge Case Evidence

**Fredrick Osei · Master's Creative Component · Statistics · Iowa State University**

How can statistical methods quantify the evidence that two cartridge cases were fired from the same firearm?

This project analyzes Congruent Matching Cells (CMC) scores using a two-stage approach, score-based likelihood ratios, and Bayesian methods. It includes the research report and original R analysis scripts.

📄 **[Read the full report](CreativeComponentFRED.pdf)**

## Research overview

The CMC algorithm compares regions on cartridge case scans and produces a similarity score. This analysis uses those scores to compare common-source and different-source propositions.

- **Two-stage approach:** compare the score against a threshold of 6, then assess coincidental association among known non-matches.
- **Score-based likelihood ratio:** compare binomial likelihoods fitted to known matches and non-matches.
- **Bayesian approach:** use beta priors, beta-binomial posterior predictive distributions, posterior model probabilities, and Bayes factors.
- **Evaluation:** leave-one-barrel-out cross-validation with sensitivity, specificity, and accuracy summaries.

The report describes 780 pairwise comparisons of 40 cartridge cases from 10 barrels, including 63 matches and 717 non-matches. It reports average sensitivity, specificity, and accuracy of at least 98% for the evaluated methods. These are results reported in the supplied document, not independently reproduced in this repository preparation. They describe this dataset and evaluation, rather than general performance on new forensic populations.

## Repository contents

| File | Purpose |
| --- | --- |
| `CreativeComponentFRED.pdf` | Full research report, methods, figures, results, and references |
| `_targets.R` | Main analysis pipeline, models, validation, and figures |
| `functions.R` | Helper functions used by the pipeline |
| `results.R` | Interactive inspection and post-processing of stored targets |
| `cmcn1.R` | Exploratory modeling using objects from an existing R session |
| `perfmce.R`, `perfmce2.R` | Exploratory validation and visualization scripts |
| `others.R` | Additional exploratory snippets |
| `REPRODUCIBILITY.md` | Data requirements, known limitations, and review notes |

## Run the main pipeline

**Current status:** the supplied R files pass syntax parsing. The processed input dataset `cmcdataset.csv` is not included, so an end-to-end reproduction has not been performed. The exploratory scripts are not standalone entry points.

Use R 4.1 or later (the pipeline uses the native `|>` operator). From the repository root, install the packages used by the main pipeline:

```r
install.packages(c(
  "targets", "tibble", "ggplot2", "extraDistr",
  "dplyr", "gridExtra", "sqldf"
))
```

Place the author's processed `cmcdataset.csv` in the repository root. See [the data requirements](REPRODUCIBILITY.md) before substituting any other dataset. Then run:

```r
targets::tar_make()

# Inspect a plot and performance summaries
targets::tar_read(cmcsummaryhist)
targets::tar_read(metricvalues)
targets::tar_read(metrics_plot)
```

For additional exploratory/post-processing scripts, `GGally` and `ggh4x` are also used. `results.R` expects an existing targets store and performs file writes; review the notes before sourcing the whole script.

## Data provenance

The report cites the [Zemmels et al. (2022) Cartridge Case Scans dataset](https://iastate.figshare.com/articles/dataset/Zemmels_et_al_2022_Cartridge_Case_Scans/19686297/1). This is the source cited in the report; it has not been verified here as a direct download for the processed CSV used by the scripts. The supplied files do not include the processing steps needed to reconstruct that CSV from source scans.

## Author and acknowledgments

Fredrick Osei · [GitHub](https://github.com/foseistats) · [Academic website](https://foseistats.github.io/) · [LinkedIn](https://www.linkedin.com/in/fredrick-osei-ms-813837224)

The report acknowledges Dr. Danica Ommen, Dr. Jarad Niemi, and Dr. Lynna Chu for their guidance and support. See the report for the complete bibliography and figure attribution.
