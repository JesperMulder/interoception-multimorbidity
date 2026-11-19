# Multimorbidity and Interoception

This repository contains the complete R analysis script accompanying the manuscript:

> Title: Interoceptive accuracy and attention across multimorbidity classes: A latent class analysis

> Authors: Mulder, J., Böker, M., Smit, A.K., Kiefte-de Jong, J.C.

> Journal: [TBD]

> DOI: [TBD]

## Overview

The script (`LCA_Analyses.R`) implements a full latent class analysis (LCA)
pipeline examining health profiles and their association with interoceptive
accuracy (IAS) and interoceptive attention (IATS) across two independent
Dutch community samples (DA and L).

### Analysis sections

|Section|Content|
|-|-|
|1|Project setup & data loading|
|2|Variable recoding, total score calculation & data quality|
|3|Socioeconomic position (SEP) calculation|
|4|Normality & multicollinearity checks|
|5|Outlier detection|
|6|Descriptive statistics & internal consistency|
|7|Latent Class Analysis (K-selection, BLRT, profile plots)|
|8|Regression of interoceptive outcomes on LCA class (BCH and modal-assignment OLS)|
|9|Sensitivity analyses (covariate-adjusted regressions)|
|10|Direct comparison by disease status|
|11|Joint analysis of interoceptive accuracy and attention|
|12|Sensitivity: LCA with alternative multimorbidity definition|
|13|Covariate adjustment strategies and sex differences|

---

## Requirements

* Tested on R version 4.6.1
* All required packages are listed at the top of `LCA_Analyses.R`.
Install them in one step:

```r
install.packages(c(
  "haven", "dplyr", "tidyr", "stringr", "naniar",
  "psych", "corrplot", "ggplot2", "patchwork", "writexl",
  "openxlsx", "poLCA", "effectsize", "broom", "car"
))
```

---

## Repository structure

```
.
├── LCA_Analyses.R      # Main analysis script (all sections)
├── data/               # Raw data — NOT included (see Data Availability)
│   └── raw/            # Place the two .sav files here before running
├── results/            # All outputs written here by the script
│   ├── *.xlsx          # Descriptive tables, fit indices, regression results
│   ├── *.png           # Profile plots, elbow plots, coefficient plots
│   └── sessionInfo.txt # R session info from the analysis run
├── .gitignore
├── LICENSE
└── README.md
```

---

## Usage

1. Clone or download this repository.
2. Obtain the raw data files (see **Data Availability** below) and place them in `data/raw/`.
3. Set the repository folder as your R working directory (in RStudio: open the folder as a project or use `setwd()`). The script uses paths relative to this folder, so no path edits are needed as long as the files in `data/raw/` have these names (set in **Section 1** of `LCA_Analyses.R`):

```r
path_da   <- file.path("data", "raw", "Part1_19082024_clean_JM_version2.sav")
path_l    <- file.path("data", "raw", "Part2_19082024_clean_JM_version2.sav")
base_path <- file.path("results")
```

4. Run the script from top to bottom (e.g., `source("LCA_Analyses.R")` or execute section-by-section in RStudio).

---

## Data Availability

The raw participant data are not included in this repository. The data contain
sensitive health information and cannot be shared openly. Access may be
requested from the corresponding author ([insert contact email]) subject to
institutional data-sharing agreements and ethics approval conditions. See the
Data Availability Statement in the published manuscript for full details.

---

## License

This code is released under the [MIT License](LICENSE). You are free to use,
adapt, and redistribute it with attribution.

---

## Contact

**Corresponding author:** Jesper Mulder — j.mulder6@lumc.nl
**Issues / questions about the code:** please open a [GitHub Issue](../../issues).

