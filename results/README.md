# results/

`LCA_Analyses.R` writes all its outputs to this folder. Git excludes them
(see `.gitignore`) because running the script reproduces them.

---

## Generated files

A complete run of `LCA_Analyses.R` produces these files:

### Excel tables

|File|Section|Description|
|-|-|-|
|`removal_log.xlsx`|5|Participant exclusion log with reasons|
|`descriptive_statistics.xlsx`|6|Sample characteristics table|
|`internal_consistency.xlsx`|6|Internal consistency of the scales|
|`LCA_fitindices_DA.xlsx`, `LCA_fitindices_L.xlsx`|7|LCA fit indices (all K), DA and L samples|
|`LCA_diagnostics_DA.xlsx`, `LCA_diagnostics_L.xlsx`|7|Classification diagnostics and candidate solutions, DA and L samples|
|`LCA_decisionlog_DA.xlsx`, `LCA_decisionlog_L.xlsx`|7|Class enumeration decision log (also as `.txt`), DA and L samples|
|`LCA_class_profiles.xlsx`|7|Composite class profile scoring|
|`LCA_classtable_DA.xlsx`, `LCA_classtable_L.xlsx`|7|Conditional probabilities by class, DA and L samples|
|`LCA_class_demographics.xlsx`|7|Class demographics (modal assignment)|
|`BCH_diagnostics.xlsx`|8|BCH diagnostics|
|`primary_regression_results_BCH.xlsx`|8|Regression results, BCH approach (unadjusted)|
|`primary_regression_results_modalOLS.xlsx`|8|Regression results, modal-assignment OLS (unadjusted)|
|`sensitivity_regression_results_BCH.xlsx`|9|Regression results, BCH approach (covariate-adjusted)|
|`sensitivity_regression_results_modalOLS.xlsx`|9|Regression results, modal-assignment OLS (covariate-adjusted; includes a VIF diagnostics sheet)|
|`disease_status_regression_results.xlsx`|10|Comparison by disease status (primary definition)|
|`disease_status_alt_regression_results.xlsx`|10|Comparison by disease status (alternative definition)|
|`joint_accuracy_attention.xlsx`|11|Joint analysis of interoceptive accuracy and attention|
|`LCA_classtable_L_alt.xlsx`|12|Conditional probabilities by class, L sample, alternative definition|
|`BCH_diagnostics_L_alt.xlsx`|12|BCH diagnostics, L sample, alternative definition|
|`alt_LCA_regression_results_BCH.xlsx`, `alt_LCA_regression_results_modalOLS.xlsx`|12|Regression results, alternative LCA (BCH and modal OLS)|
|`M4_covariate_adjustment.xlsx`|13|Stepwise and separate covariate adjustment, sex differences, interaction tests|
|`regression_diagnostics.xlsx`|13|Residual diagnostics (skewness, kurtosis, Shapiro–Wilk, Breusch–Pagan) across regression models|

For the alternative L-sample enumeration, the `LCA_fitindices_`, `LCA_diagnostics_`
and `LCA_decisionlog_` files are also written with the suffix `L_alt`.

### Figures (PNG, 300 dpi)

|File|Section|Description|
|-|-|-|
|`LCA_elbowplot_DA.png`, `LCA_elbowplot_L.png`|7|Fit indices by number of classes, DA and L samples|
|`LCA_profileplot_DA.png`, `LCA_profileplot_L.png`|7|Class profile plots, DA and L samples|
|`primary_regression_coef_DA.png`, `primary_regression_coef_L.png`|8|Coefficient plots (unadjusted), DA and L samples|
|`sensitivity_regression_coef_DA.png`, `sensitivity_regression_coef_L.png`|9|Coefficient plots (adjusted), DA and L samples|
|`sensitivity_regression_coef_DA_BDI_suppl.png`|9|Supplementary coefficient plot including BDI, DA sample|
|`LCA_elbowplot_L_alt.png`, `LCA_profileplot_L_alt.png`|12|Elbow and profile plots, L sample, alternative definition|

### Reproducibility

|File|Description|
|-|-|
|`sessionInfo.txt`|Full R session information (R version, OS, package versions), written at the end of the script|

---

## Reproducing the outputs

```r
# From the project root directory:
source("LCA_Analyses.R")
```

See the main `README.md` for requirements.
