# Multimorbidity and Interoception

This repository contains the R analysis script for the manuscript:

- **Title:** Interoceptive accuracy and attention across multimorbidity classes: A latent class analysis
- **Authors:** Mulder, J., Böker, M., Smit, A.K., Kiefte-de Jong, J.C.
- **Journal:** [TBD]
- **DOI:** [TBD]

## Overview

The script (`LCA_Analyses.R`) runs a latent class analysis (LCA) of health
profiles in two independent Dutch community samples (DA and L). It then tests
how the classes differ in interoceptive accuracy (IAS) and interoceptive
attention (IATS).

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

- R 4.6.1 (the version used for the analyses)
- The 15 packages listed at the top of `LCA_Analyses.R`

The package versions are locked with [renv](https://rstudio.github.io/renv/).
To install them, open the repository folder as an R project and run:

```r
install.packages("renv")  # only if renv is not installed yet
renv::restore()
```

Without renv, install the packages directly:

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
├── LCA_Analyses.R      # Analysis script (all sections)
├── data/               # Raw data (not included, see Data Availability)
│   └── raw/            # Put the two .sav files here before running
├── results/            # The script writes all outputs here
│   ├── *.xlsx          # Descriptive tables, fit indices, regression results
│   ├── *.png           # Profile plots, elbow plots, coefficient plots
│   └── sessionInfo.txt # R session info from the analysis run
├── renv.lock           # Locked package versions
├── renv/               # renv setup files
├── .Rprofile           # Activates renv when the project opens
├── .gitignore
├── LICENSE
└── README.md
```

---

## Usage

1. Clone or download this repository.
2. Request the raw data files (see **Data Availability** below) and put them in `data/raw/`.
3. Open the repository folder as your R working directory. In RStudio, open it as a project; otherwise use `setwd()`. The script uses paths relative to this folder. You need no path edits if the files in `data/raw/` have the names set in **Section 1** of `LCA_Analyses.R`:

```r
path_da   <- file.path("data", "raw", "Part1_19082024_clean_JM_version2.sav")
path_l    <- file.path("data", "raw", "Part2_19082024_clean_JM_version2.sav")
base_path <- file.path("results")
```

4. Run the script from top to bottom, with `source("LCA_Analyses.R")` or section by section in RStudio.

---

## Data Availability

The raw participant data are not in this repository. They contain sensitive
health information and cannot be shared openly. You can request access from the
corresponding author (j.mulder6@lumc.nl), subject to institutional
data-sharing agreements and ethics approval conditions. The Data Availability
Statement in the published manuscript has the full details.

---

## License

This code is released under the [MIT License](LICENSE). You may use, adapt and
redistribute it with attribution.

---

## Contact

**Corresponding author:** Jesper Mulder, j.mulder6@lumc.nl
**Questions about the code:** open a [GitHub Issue](../../issues).
