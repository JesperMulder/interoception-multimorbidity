# ══════════════════════════════════════════════════════════════════════════════
# Title:       Latent Class Analysis — Health Profiles and Interoceptive
#              Awareness
# Authors:     [Author Name(s)]
# Affiliation: [Institution]
# Date:        [YYYY-MM-DD]
# R version:   [e.g., 4.4.0 — see sessionInfo() output at end of script]
#
# Description:
#   Full analysis pipeline for latent class analysis (LCA) of health profiles
#   and their association with interoceptive accuracy (IAS) and attention
#   (IATS) across two independent samples (DA and L). The pipeline covers
#   data cleaning and quality control, normality and multicollinearity
#   checks, outlier detection, descriptive statistics and internal
#   consistency, LCA with K-selection and bootstrap LRT, OLS regression of
#   class membership on interoceptive outcomes, and covariate-adjusted
#   sensitivity analyses.
#
# Sections:
#   1 — Project setup & data loading
#   2 — Variable recoding, total score calculation & data quality
#   3 — SEP calculation from education and income
#   4 — Normality & multicollinearity checks
#   5 — Outlier detection
#   6 — Descriptive statistics & internal consistency
#   7 — Latent class analysis
#   8 — Regression analyses
#   9 — Sensitivity analyses (covariate-adjusted regressions)
#
# Citation:
#   [Author et al. (Year). Title. Journal, Vol(Issue), pp–pp.
#    https://doi.org/xxxxx]
#
# License:     MIT (see LICENSE file)
#
# Data availability:
#   Raw data are not included in this repository. The data files referenced
#   in Section 1 (path_da, path_l) contain participant-level health data
#   and cannot be shared openly. Access may be requested from the
#   corresponding author subject to institutional data-sharing agreements.
#   See the data availability statement in the published manuscript.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# LCA ANALYSES
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 1 — Project Setup & Data Loading
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Load packages ──────────────────────────────────────────────────────────
#
# To install all required packages in one step, run the following line once
# in a fresh R session before sourcing this script:
#   install.packages(required_packages)
required_packages <- c(
  "conflicted", # Resolve function-name conflicts explicitly
  "haven",      # Read .sav files (SPSS)
  "dplyr",      # Data wrangling
  "tidyr",      # Reshaping data
  "stringr",    # String operations
  "naniar",     # Missing data summaries
  "psych",      # Descriptive statistics (alpha, omega, skewness, kurtosis)
  "corrplot",   # Correlation matrix visualisation
  "ggplot2",    # Data visualisation
  "patchwork",  # Combining ggplot2 plots
  "writexl",    # Write Excel files (no Java)
  "openxlsx",   # Advanced Excel writing (formatting, styles)
  "poLCA",      # Latent Class Analysis
  "effectsize", # Standardised parameters
  "broom"       # Tidy model output
)

load_packages <- function(packages) {
  missing_pkgs <- packages[!vapply(packages, requireNamespace,
                                   logical(1L), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop(
      "The following required packages are not installed:\n  ",
      paste(missing_pkgs, collapse = ", "), "\n\n",
      "Install them by running:\n  install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "), "))\n",
      "Then re-run this script."
    )
  }
  invisible(lapply(packages, library, character.only = TRUE))
}

load_packages(required_packages)

# ── Resolve function-name conflicts ───────────────────────────────────────────
# Several packages (e.g. MASS, stats) export functions with the same name as
# dplyr verbs. The conflicted package turns ambiguous calls into hard errors,
# and conflict_prefer() declares the winner once for the whole session.
conflict_prefer("select", "dplyr", quiet = TRUE)
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("mutate", "dplyr", quiet = TRUE)
conflict_prefer("rename", "dplyr", quiet = TRUE)

# ── 1. File paths ─────────────────────────────────────────────────────────────
# ── USER CONFIGURATION ────────────────────────────────────────────────────────
# Edit the three paths below before running the script.
# Relative paths are used by default (relative to the project root directory,
# i.e. the folder containing this script). Absolute paths are also accepted.
#
# Recommended project layout:
#   <project_root>/
#   ├── LCA_Analyses.R          ← this script
#   ├── data/
#   │   └── raw/                ← place the two .sav files here
#   └── results/                ← all outputs (Excel, PNG) will be written here
#
# DATA AVAILABILITY NOTE: The raw .sav files are not included in this
# repository. They contain participant-level health data that cannot be
# shared openly. Access may be requested from the corresponding author
# subject to institutional data-sharing agreements. See the data
# availability statement in the published manuscript for details.

path_da   <- file.path("data", "raw", "[Part 1 dataset/DA-dataset file name].sav")
path_l    <- file.path("data", "raw", "[Part 2 dataset/L-dataset file name].sav")
base_path <- file.path("results")
# Output directory — created here so all later sections can write to it directly
if (!dir.exists(base_path)) dir.create(base_path, recursive = TRUE)
cat("Output directory:", base_path, "\n")

# ── 2. Load raw datasets ──────────────────────────────────────────────────────
da_raw <- read_sav(path_da)
l_raw  <- read_sav(path_l)

# ── 3. Add sample prefix to Persoon_ID ───────────────────────────────────────
da_raw <- da_raw |> mutate(Persoon_ID = paste0("DA_", Persoon_ID))
l_raw  <- l_raw  |> mutate(Persoon_ID = paste0("L_",  Persoon_ID))

# ── 4. Add sample identifier ──────────────────────────────────────────────────
da_raw <- da_raw |> mutate(sample = "DA")
l_raw  <- l_raw  |> mutate(sample = "L")

# ── 5. Strip haven labels ─────────────────────────────────────────────────────
da_raw <- da_raw |> zap_labels() |> zap_label()
l_raw  <- l_raw  |> zap_labels() |> zap_label()

# ── 6. Integrity checks ───────────────────────────────────────────────────────
stopifnot(
  "Duplicate Persoon_ID in DA" = !any(duplicated(da_raw$Persoon_ID)),
  "Duplicate Persoon_ID in L"  = !any(duplicated(l_raw$Persoon_ID))
)

cat("DA — participants loaded:", nrow(da_raw), "\n")
cat("L  — participants loaded:", nrow(l_raw),  "\n")

# ── 7. Initial inspection ─────────────────────────────────────────────────────
cat("\n── DA dataset ──\n"); glimpse(da_raw)
cat("\n── L dataset ──\n");  glimpse(l_raw)

# ── 8. Missing data overview ──────────────────────────────────────────────────
cat("\n── Missing data: DA ──\n")
miss_var_summary(da_raw) |> filter(pct_miss > 0) |> print(n = Inf)
cat("\n── Missing data: L ──\n")
miss_var_summary(l_raw)  |> filter(pct_miss > 0) |> print(n = Inf)

cat("\nSection 1 complete.\n")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 2 — Variable Recoding, Total Score Calculation & Data Quality
# ══════════════════════════════════════════════════════════════════════════════

# ── Helper functions ──────────────────────────────────────────────────────────

col_range <- function(data, items) {
  present <- intersect(items, names(data))
  if (length(present) == 0) return(list(min = NA, max = NA))
  vals <- data |> select(all_of(present)) |> unlist(use.names = FALSE)
  list(min = min(vals, na.rm = TRUE), max = max(vals, na.rm = TRUE))
}

flag_and_remove <- function(df, log, bad_rows, reason) {
  bad_rows[is.na(bad_rows)] <- FALSE
  n_bad <- sum(bad_rows)
  if (n_bad == 0) {
    cat("  No participants flagged:", reason, "\n")
    return(list(df = df, log = log))
  }
  new_entries <- df[bad_rows, c("Persoon_ID", "sample")] |>
    mutate(reason = reason)
  log <- bind_rows(log, new_entries)
  df  <- df[!bad_rows, ]
  cat("  Removed", n_bad, "participant(s):", reason, "\n")
  list(df = df, log = log)
}

items_out_of_range <- function(df, items, lo, hi) {
  present <- intersect(items, names(df))
  if (length(present) == 0) return(rep(FALSE, nrow(df)))
  rowSums(
    df |> select(all_of(present)) |>
      mutate(across(everything(), ~ . < lo | . > hi)) |>
      mutate(across(everything(), ~ replace_na(., FALSE))),
    na.rm = TRUE
  ) > 0
}

# ── Initialise removal logs ───────────────────────────────────────────────────
empty_log <- function() {
  data.frame(Persoon_ID = character(), sample = character(),
             reason = character(), stringsAsFactors = FALSE)
}
removal_log_da <- empty_log()
removal_log_l  <- empty_log()

# ── 2.1 Drop unneeded variables ───────────────────────────────────────────────
# Opleiding_Cat and Inkomen_Cat are RETAINED — used to calculate SEP_3Cat in
# Section 3. The original binary SEP variable is dropped.

drop_vars <- function(df) {
  ipaq_drop <- c(
    "Actief_Nonactief", "IPAQ_Sport", "OpenClosedSkill", "SportCat",
    "TeamIndividueel", "OpenSkill_dummy", "ClosedSkill_dummy",
    "OpenClosed_Cat1_dummy", "OpenClosed_Cat2_dummy",
    "OpenClosed_Cat3_dummy", "OpenClosed_Cat4_dummy",
    "Team_dummy", "Individueel_dummy", "IPAQ_SportErvaring",
    "IPAQ_Wandel_TempoMET", "Wandel_UrenWeek", "Moderate_UrenWeek",
    "Vigorous_UrenWeek", "Wandel_MET", "Vigorous_MET", "Moderate_MET",
    "IPAQ_Uren_Totaal", "IPAQ_MET_Totaal"
  )
  # SEP: original imputed binary variable — replaced by calculated SEP_3Cat
  # Opleiding_Cat and Inkomen_Cat: KEPT for SEP calculation
  other_drop <- c(
    "BPQ_Awareness", "BPQ_ANS", "Leeftijd_Cat",
    "SEP",           # dropped: replaced by calculated 3-category SEP
    "Opleiding",     # raw education variable — keep only _Cat
    "Provincie",
    "Geen_ChronischeAandoening", "AndersOpen_ChronischeAandoening",
    "Wel_ChronischeAandoening"
  )
  bpq_icq_drop <- names(df)[str_detect(names(df), "^(BPQ_|ICQ)")]
  roken_drop   <- names(df)[str_detect(names(df), "^Roken_") &
                              names(df) != "Roken_Frequentie"]
  
  all_drop <- unique(c(ipaq_drop, other_drop, bpq_icq_drop, roken_drop))
  df |> select(-any_of(all_drop))
}

df_da <- drop_vars(da_raw)
df_l  <- drop_vars(l_raw)
cat("DA — variables remaining after dropping:", ncol(df_da), "\n")
cat("L  — variables remaining after dropping:", ncol(df_l),  "\n")

# ── 2.2 Education and income range checks ────────────────────────────────────
# Run on raw integer values BEFORE factor conversion in 2.3 so that
# out-of-range codes (e.g. 0 or 4) are caught rather than silently becoming NA
# during factor creation.  Valid levels: 1–3.

check_cat_range <- function(df, log, varname, lo, hi, label_sample) {
  cat("\n── Range check:", varname, "—", label_sample, "──\n")
  x   <- as.integer(df[[varname]])
  bad <- !is.na(x) & (x < lo | x > hi)
  result <- flag_and_remove(df, log, bad,
                            paste0(varname, " outside ", lo, "–", hi))
  cat("  Valid n:", sum(!bad & !is.na(x)),
      "| Missing:", sum(is.na(x)), "\n")
  result
}

result <- check_cat_range(df_da, removal_log_da, "Opleiding_Cat", 1, 3, "DA")
df_da  <- result$df; removal_log_da <- result$log
result <- check_cat_range(df_da, removal_log_da, "Inkomen_Cat",   1, 3, "DA")
df_da  <- result$df; removal_log_da <- result$log

result <- check_cat_range(df_l, removal_log_l, "Opleiding_Cat", 1, 3, "L")
df_l   <- result$df; removal_log_l  <- result$log
result <- check_cat_range(df_l, removal_log_l, "Inkomen_Cat",   1, 3, "L")
df_l   <- result$df; removal_log_l  <- result$log

# ── 2.3 Demographics ──────────────────────────────────────────────────────────
# Geslacht:      1 = Male, 2 = Female
# Opleiding_Cat: 1 = Low, 2 = Middle, 3 = High   (ordered factor)
# Inkomen_Cat:   1 = Below average, 2 = Average, 3 = Above average (ordered factor)
# SEP_3Cat: calculated in Section 3 from the sum of the above two

recode_demographics <- function(df) {
  df |> mutate(
    Geslacht = factor(Geslacht, levels = c(1, 2),
                      labels = c("Male", "Female")),
    
    # Education: ordered factor (1 = Low → 3 = High)
    Opleiding_Cat = factor(Opleiding_Cat,
                           levels  = c(1, 2, 3),
                           labels  = c("Low", "Middle", "High"),
                           ordered = TRUE),
    
    # Income: ordered factor (1 = Below average → 3 = Above average)
    Inkomen_Cat   = factor(Inkomen_Cat,
                           levels  = c(1, 2, 3),
                           labels  = c("Below average", "Average", "Above average"),
                           ordered = TRUE),
    
    Leeftijd_Cat  = factor(
      if_else(Leeftijd < 45, "Below 45", "45 or older"),
      levels = c("Below 45", "45 or older")
    )
  )
}

df_da <- recode_demographics(df_da)
df_l  <- recode_demographics(df_l)

# ── 2.4 Age sanity check ──────────────────────────────────────────────────────
for (lbl in c("DA", "L")) {
  df_cur  <- if (lbl == "DA") df_da else df_l
  log_cur <- if (lbl == "DA") removal_log_da else removal_log_l
  cat("\n── Age quality check —", lbl, "──\n")
  age_bad <- df_cur$Leeftijd < 18 | df_cur$Leeftijd > 100
  result  <- flag_and_remove(df_cur, log_cur, age_bad,
                             "Age out of realistic range (18–100)")
  if (lbl == "DA") { df_da <- result$df; removal_log_da <- result$log }
  else             { df_l  <- result$df; removal_log_l  <- result$log }
}

# ── 2.5a Disease morbidity (WHO chronic conditions) ───────────────────────────
# Only true chronic conditions are counted toward multimorbidity (WHO criteria).
# Overgewicht_Obesitas, Hoge_Bloeddruk, and Verhoogd_Cholesterol are modifiable
# risk factors and are handled separately in section 2.5b.
# Disease indicators: 0 = Absent, 1 = Present.
# Categories: No Disease (0 conditions), One Disease (1), Multimorbid (>1)

disease_vars <- c(
  "Type2_Diabetes", "Astma_COPD", "Hart_Vaatziekten", "Kanker",
  "Spier_Gewrichtsproblemen", "Leververvetting", "Dementie",
  "Anders_ChronischeAandoening"
)

process_disease <- function(df, log, label) {
  cat("\n── Disease variable quality check —", label, "──\n")
  disease_bad <- items_out_of_range(df, disease_vars, 0, 1)
  result      <- flag_and_remove(df, log, disease_bad,
                                 "Disease indicator(s) outside 0–1 range")
  df  <- result$df
  log <- result$log
  df <- df |>
    mutate(
      Disease_Count = rowSums(across(any_of(disease_vars)), na.rm = TRUE),
      Disease_Cat   = factor(
        case_when(
          Disease_Count == 0 ~ "No Disease",
          Disease_Count == 1 ~ "One Disease",
          Disease_Count  > 1 ~ "Multimorbid"
        ),
        levels = c("No Disease", "One Disease", "Multimorbid")
      )
    )
  list(df = df, log = log)
}

result <- process_disease(df_da, removal_log_da, "DA")
df_da  <- result$df; removal_log_da <- result$log

result <- process_disease(df_l, removal_log_l, "L")
df_l   <- result$df; removal_log_l  <- result$log

# ── 2.5b Modifiable risk factors → single composite LCA indicator ─────────────
# Overgewicht_Obesitas, Hoge_Bloeddruk, and Verhoogd_Cholesterol are NOT counted
# toward multimorbidity but are entered into the LCA as a single binary variable.
# RiskFactor_Cat = "Present" if the participant has at least one of the three
# risk factors; "Absent" otherwise.
# Raw coding: 0 = Absent, 1 = Present per risk factor.

risk_factor_vars <- c("Overgewicht_Obesitas", "Hoge_Bloeddruk", "Verhoogd_Cholesterol")

process_risk_factors <- function(df, log, label) {
  cat("\n── Modifiable risk factor quality check —", label, "──\n")
  rf_bad <- items_out_of_range(df, risk_factor_vars, 0, 1)
  result <- flag_and_remove(df, log, rf_bad,
                            "Risk factor indicator(s) outside 0–1 range")
  df  <- result$df
  log <- result$log
  
  # Composite: at least one risk factor present = "Present"
  df <- df |>
    mutate(
      RiskFactor_Cat = factor(
        if_else(rowSums(across(any_of(risk_factor_vars)), na.rm = TRUE) >= 1,
                "Present", "Absent"),
        levels = c("Absent", "Present")
      )
    )
  cat("  RiskFactor_Cat created.\n")
  print(table(df$RiskFactor_Cat, useNA = "ifany"))
  list(df = df, log = log)
}

result <- process_risk_factors(df_da, removal_log_da, "DA")
df_da  <- result$df; removal_log_da <- result$log

result <- process_risk_factors(df_l, removal_log_l, "L")
df_l   <- result$df; removal_log_l  <- result$log

# ── 2.6 IAS ───────────────────────────────────────────────────────────────────
# Items: IAS_1–IAS_21 | Item scale: 1–5 | Total range: 21–105

process_ias <- function(df, log, label) {
  cat("\n── IAS quality check —", label, "──\n")
  items <- paste0("IAS_", 1:21)
  r     <- col_range(df, items)
  cat("Global item range — min:", r$min, "| max:", r$max, "\n")
  bad    <- items_out_of_range(df, items, 1, 5)
  result <- flag_and_remove(df, log, bad, "IAS item(s) outside 1–5 range")
  df <- result$df; log <- result$log
  df <- df |> mutate(IAS_Totaal = rowSums(across(any_of(items)), na.rm = FALSE))
  total_bad <- !between(df$IAS_Totaal, 21, 105) & !is.na(df$IAS_Totaal)
  result    <- flag_and_remove(df, log, total_bad, "IAS_Totaal outside 21–105")
  cat("IAS checks passed.\n")
  list(df = result$df, log = result$log)
}

result <- process_ias(df_da, removal_log_da, "DA")
df_da  <- result$df; removal_log_da <- result$log
result <- process_ias(df_l, removal_log_l, "L")
df_l   <- result$df; removal_log_l  <- result$log

# ── 2.7 IATS ──────────────────────────────────────────────────────────────────
# Items: IATS_1–IATS_21 | Item scale: 1–5 | Total range: 21–105

process_iats <- function(df, log, label) {
  cat("\n── IATS quality check —", label, "──\n")
  items <- paste0("IATS_", 1:21)
  r     <- col_range(df, items)
  cat("Global item range — min:", r$min, "| max:", r$max, "\n")
  bad    <- items_out_of_range(df, items, 1, 5)
  result <- flag_and_remove(df, log, bad, "IATS item(s) outside 1–5 range")
  df <- result$df; log <- result$log
  df <- df |> mutate(IATS_Totaal = rowSums(across(any_of(items)), na.rm = FALSE))
  total_bad <- !between(df$IATS_Totaal, 21, 105) & !is.na(df$IATS_Totaal)
  result    <- flag_and_remove(df, log, total_bad, "IATS_Totaal outside 21–105")
  cat("IATS checks passed.\n")
  list(df = result$df, log = result$log)
}

result <- process_iats(df_da, removal_log_da, "DA")
df_da  <- result$df; removal_log_da <- result$log
result <- process_iats(df_l, removal_log_l, "L")
df_l   <- result$df; removal_log_l  <- result$log

# ── 2.8 BDI — DA sample only ──────────────────────────────────────────────────
# Items: BDI_1–BDI_21 | Item scale: 0–3 (may be stored as 1–4) | Total: 0–63
# Categories: Low depressive symptoms (0–13),
#             Clinically relevant depressive symptoms (14–63)

cat("\n── BDI quality check — DA ──\n")
bdi_items <- paste0("BDI_", 1:21)
bdi_r     <- col_range(df_da, bdi_items)
cat("Global item range — min:", bdi_r$min, "| max:", bdi_r$max, "\n")

if (bdi_r$max > 3) {
  cat("BDI on 1–4 scale. Recoding to 0–3.\n")
  df_da <- df_da |> mutate(across(any_of(bdi_items), ~ . - 1))
} else {
  cat("BDI already on 0–3 scale.\n")
}
bdi_bad <- items_out_of_range(df_da, bdi_items, 0, 3)
result  <- flag_and_remove(df_da, removal_log_da, bdi_bad,
                           "BDI item(s) outside 0–3 range after correction")
df_da <- result$df; removal_log_da <- result$log

df_da <- df_da |>
  mutate(BDI_Totaal = rowSums(across(any_of(bdi_items)), na.rm = FALSE))
bdi_total_bad <- !between(df_da$BDI_Totaal, 0, 63) & !is.na(df_da$BDI_Totaal)
result        <- flag_and_remove(df_da, removal_log_da, bdi_total_bad,
                                 "BDI_Totaal outside 0–63")
df_da <- result$df; removal_log_da <- result$log

df_da <- df_da |>
  mutate(BDI_Cat = factor(
    if_else(BDI_Totaal <= 13,
            "Low depressive symptoms",
            "Clinically relevant depressive symptoms"),
    levels = c("Low depressive symptoms",
               "Clinically relevant depressive symptoms")
  ))
cat("BDI checks passed.\n")
print(table(df_da$BDI_Cat, useNA = "ifany"))

# ── 2.9 TAS — DA sample only ──────────────────────────────────────────────────
# Items: TAS_1–TAS_20 | Item scale: 1–5 | Total range: 20–100
# Categories: No Alexithymia (≤60), Alexithymia Present (61–100)

cat("\n── TAS quality check — DA ──\n")
tas_items <- paste0("TAS_", 1:20)
tas_r     <- col_range(df_da, tas_items)
cat("Global item range — min:", tas_r$min, "| max:", tas_r$max, "\n")

tas_bad <- items_out_of_range(df_da, tas_items, 1, 5)
result  <- flag_and_remove(df_da, removal_log_da, tas_bad,
                           "TAS item(s) outside 1–5 range")
df_da <- result$df; removal_log_da <- result$log

df_da <- df_da |>
  mutate(TAS_Totaal = rowSums(across(any_of(tas_items)), na.rm = FALSE))
tas_total_bad <- !between(df_da$TAS_Totaal, 20, 100) & !is.na(df_da$TAS_Totaal)
result        <- flag_and_remove(df_da, removal_log_da, tas_total_bad,
                                 "TAS_Totaal outside 20–100")
df_da <- result$df; removal_log_da <- result$log

df_da <- df_da |>
  mutate(TAS_Cat = factor(
    case_when(
      TAS_Totaal <= 60  ~ "No Alexithymia",
      TAS_Totaal <= 100 ~ "Alexithymia Present"
    ),
    levels = c("No Alexithymia", "Alexithymia Present")
  ))
cat("TAS checks passed.\n")

# ── 2.10 IPAQ — L sample only ─────────────────────────────────────────────────
# Binary variables: 1 = Yes (activity performed), 2 = No
# Active: ≥75 min vigorous/week OR ≥150 min effective moderate/week
# Walking counts as moderate only at high (1) or moderate (2) tempo

ipaq_vars_present <- all(c(
  "IPAQ_Wandel_UrenDag",   "IPAQ_Wandel_MinDag",   "IPAQ_Wandel_Dagen",
  "IPAQ_Wandel_Tempo",     "IPAQ_Wandel_Binary",
  "IPAQ_Moderate_UrenDag", "IPAQ_Moderate_MinDag", "IPAQ_Moderate_Dagen",
  "IPAQ_Moderate_Binary",
  "IPAQ_Vigorous_UrenDag", "IPAQ_Vigorous_MinDag", "IPAQ_Vigorous_Dagen",
  "IPAQ_Vigorous_Binary"
) %in% names(df_l))

if (!ipaq_vars_present) {
  warning("One or more IPAQ variables missing from L dataset. Skipping IPAQ processing.")
} else {
  cat("\n── IPAQ quality check — L ──\n")
  
  ipaq_days_bad <- replace_na(
    !between(df_l$IPAQ_Wandel_Dagen,   0, 7) |
      !between(df_l$IPAQ_Moderate_Dagen, 0, 7) |
      !between(df_l$IPAQ_Vigorous_Dagen, 0, 7), FALSE)
  result <- flag_and_remove(df_l, removal_log_l, ipaq_days_bad,
                            "IPAQ days/week outside 0–7")
  df_l <- result$df; removal_log_l <- result$log
  
  ipaq_hours_bad <- replace_na(
    !between(df_l$IPAQ_Wandel_UrenDag,   0, 16) |
      !between(df_l$IPAQ_Moderate_UrenDag, 0, 16) |
      !between(df_l$IPAQ_Vigorous_UrenDag, 0, 16), FALSE)
  result <- flag_and_remove(df_l, removal_log_l, ipaq_hours_bad,
                            "IPAQ hours/day outside 0–16")
  df_l <- result$df; removal_log_l <- result$log
  
  ipaq_mins_bad <- replace_na(
    !between(df_l$IPAQ_Wandel_MinDag,   0, 59) |
      !between(df_l$IPAQ_Moderate_MinDag, 0, 59) |
      !between(df_l$IPAQ_Vigorous_MinDag, 0, 59), FALSE)
  result <- flag_and_remove(df_l, removal_log_l, ipaq_mins_bad,
                            "IPAQ minutes component/day outside 0–59")
  df_l <- result$df; removal_log_l <- result$log
  
  cat("IPAQ sanity checks passed.\n")
  
  df_l <- df_l |>
    mutate(
      Wandel_MinDag_Total        = if_else(
        IPAQ_Wandel_Binary == 2, 0,
        (IPAQ_Wandel_UrenDag * 60) + IPAQ_Wandel_MinDag),
      Wandel_MinWeek             = Wandel_MinDag_Total * IPAQ_Wandel_Dagen,
      
      Moderate_MinDag_Total      = if_else(
        IPAQ_Moderate_Binary == 2, 0,
        (IPAQ_Moderate_UrenDag * 60) + IPAQ_Moderate_MinDag),
      Moderate_MinWeek           = Moderate_MinDag_Total * IPAQ_Moderate_Dagen,
      
      Vigorous_MinDag_Total      = if_else(
        IPAQ_Vigorous_Binary == 2, 0,
        (IPAQ_Vigorous_UrenDag * 60) + IPAQ_Vigorous_MinDag),
      Vigorous_MinWeek           = Vigorous_MinDag_Total * IPAQ_Vigorous_Dagen,
      
      Wandel_AsModerateMins      = if_else(
        IPAQ_Wandel_Tempo %in% c(1, 2), Wandel_MinWeek, 0),
      
      Moderate_Effective_MinWeek = Moderate_MinWeek + Wandel_AsModerateMins,
      
      IPAQ_Cat = factor(
        case_when(
          Vigorous_MinWeek           >= 75  ~ "Active",
          Moderate_Effective_MinWeek >= 150 ~ "Active",
          TRUE                              ~ "Insufficiently Active"
        ),
        levels = c("Insufficiently Active", "Active")
      )
    )
  cat("IPAQ_Cat created.\n")
  print(table(df_l$IPAQ_Cat, useNA = "ifany"))
}

# ── 2.11 SSRQ — L sample only ─────────────────────────────────────────────────
# Items: SSRQ_1–SSRQ_31 | Item scale: 1–5 | Total range: 31–155
# Categories: split at L-sample median (distribution-based)

cat("\n── SSRQ quality check — L ──\n")
ssrq_items <- paste0("SSRQ_", 1:31)
ssrq_r     <- col_range(df_l, ssrq_items)
cat("Global item range — min:", ssrq_r$min, "| max:", ssrq_r$max, "\n")

ssrq_bad <- items_out_of_range(df_l, ssrq_items, 1, 5)
result   <- flag_and_remove(df_l, removal_log_l, ssrq_bad,
                            "SSRQ item(s) outside 1–5 range")
df_l <- result$df; removal_log_l <- result$log

df_l <- df_l |>
  mutate(SSRQ_Totaal = rowSums(across(any_of(ssrq_items)), na.rm = FALSE))
ssrq_total_bad <- !between(df_l$SSRQ_Totaal, 31, 155) & !is.na(df_l$SSRQ_Totaal)
result         <- flag_and_remove(df_l, removal_log_l, ssrq_total_bad,
                                  "SSRQ_Totaal outside 31–155")
df_l <- result$df; removal_log_l <- result$log

ssrq_median <- median(df_l$SSRQ_Totaal, na.rm = TRUE)
cat("SSRQ median (cutpoint, L sample):", ssrq_median, "\n")

df_l <- df_l |>
  mutate(SSRQ_Cat = factor(
    if_else(SSRQ_Totaal <= ssrq_median,
            "Below Average Self-Regulation",
            "Above Average Self-Regulation"),
    levels = c("Below Average Self-Regulation",
               "Above Average Self-Regulation")
  ))
cat("SSRQ checks passed.\n")

# ── 2.12 EEQ — L sample only ──────────────────────────────────────────────────
# Items: EEQ_1–EEQ_10 | Item scale: 0–3 (may be stored as 1–4) | Total: 0–30
# Note: Very Emotional Eater collapsed with Emotional Eater (<10% prevalence)

cat("\n── EEQ quality check — L ──\n")
eeq_items <- paste0("EEQ_", 1:10)
eeq_r     <- col_range(df_l, eeq_items)
cat("Global item range — min:", eeq_r$min, "| max:", eeq_r$max, "\n")

if (eeq_r$max > 3) {
  cat("EEQ on 1–4 scale. Recoding to 0–3.\n")
  df_l <- df_l |> mutate(across(any_of(eeq_items), ~ . - 1))
} else {
  cat("EEQ already on 0–3 scale.\n")
}
eeq_bad <- items_out_of_range(df_l, eeq_items, 0, 3)
result  <- flag_and_remove(df_l, removal_log_l, eeq_bad,
                           "EEQ item(s) outside 0–3 range after correction")
df_l <- result$df; removal_log_l <- result$log

df_l <- df_l |>
  mutate(EEQ_Totaal = rowSums(across(any_of(eeq_items)), na.rm = FALSE))
eeq_total_bad <- !between(df_l$EEQ_Totaal, 0, 30) & !is.na(df_l$EEQ_Totaal)
result        <- flag_and_remove(df_l, removal_log_l, eeq_total_bad,
                                 "EEQ_Totaal outside 0–30")
df_l <- result$df; removal_log_l <- result$log

df_l <- df_l |>
  mutate(EEQ_Cat = factor(
    case_when(
      EEQ_Totaal <= 5  ~ "Non-Emotional Eater",
      EEQ_Totaal <= 10 ~ "Low Emotional Eater",
      EEQ_Totaal <= 30 ~ "Emotional to Very Emotional Eater"
    ),
    levels = c("Non-Emotional Eater", "Low Emotional Eater",
               "Emotional to Very Emotional Eater")
  ))
cat("EEQ checks passed.\n")

# ── 2.13 JSS — L sample only ──────────────────────────────────────────────────
# Items: JSS_1–JSS_4 | Item scale: 0–5 (may be stored as 1–6) | Total: 0–20

cat("\n── JSS quality check — L ──\n")
jss_items <- paste0("JSS_", 1:4)
jss_r     <- col_range(df_l, jss_items)
cat("Global item range — min:", jss_r$min, "| max:", jss_r$max, "\n")

if (jss_r$min >= 1 & jss_r$max <= 6) {
  cat("JSS on 1–6 scale. Recoding to 0–5.\n")
  df_l <- df_l |> mutate(across(any_of(jss_items), ~ . - 1))
} else if (jss_r$min < 0 | jss_r$max > 5) {
  warning("JSS out of unexpected range — inspect manually.")
} else {
  cat("JSS already on 0–5 scale.\n")
}
jss_bad <- items_out_of_range(df_l, jss_items, 0, 5)
result  <- flag_and_remove(df_l, removal_log_l, jss_bad,
                           "JSS item(s) outside 0–5 range after correction")
df_l <- result$df; removal_log_l <- result$log

df_l <- df_l |>
  mutate(JSS_Totaal = rowSums(across(any_of(jss_items)), na.rm = FALSE))
jss_total_bad <- !between(df_l$JSS_Totaal, 0, 20) & !is.na(df_l$JSS_Totaal)
result        <- flag_and_remove(df_l, removal_log_l, jss_total_bad,
                                 "JSS_Totaal outside 0–20")
df_l <- result$df; removal_log_l <- result$log

df_l <- df_l |>
  mutate(JSS_Cat = factor(
    case_when(
      JSS_Totaal <= 11 ~ "No Disturbed Sleep",
      JSS_Totaal <= 20 ~ "Disturbed Sleep"
    ),
    levels = c("No Disturbed Sleep", "Disturbed Sleep")
  ))
cat("JSS checks passed.\n")

# ── 2.14 Alcohol — L sample only ──────────────────────────────────────────────
# Items: 0–4 (may be stored as 1–5) | Total: 0–12
# Item 2 recoding (after scale correction): 0 stays 0, all other values − 1
# Cutoffs: No Likelihood (<5 males, <4 females), Likelihood Present (≥cutoff)
#
# Non-drinker rule: if Alcohol_Frequentie == 0 (never drinks), items 2 and 3
# are not presented to the participant and will be NA in the data. These NAs are
# set to 0 before scoring because a non-drinker by definition scores 0 on
# quantity and binge frequency, and their total score is unambiguously 0.

cat("\n── Alcohol quality check — L ──\n")
alcohol_items <- c("Alcohol_Frequentie", "Alcohol_AantalDag", "Alcohol_MeerDan6")
alcohol_r     <- col_range(df_l, alcohol_items)
cat("Global item range — min:", alcohol_r$min, "| max:", alcohol_r$max, "\n")

if (alcohol_r$min >= 1 & alcohol_r$max <= 5) {
  cat("Alcohol on 1–5 scale. Recoding to 0–4.\n")
  df_l <- df_l |> mutate(across(any_of(alcohol_items), ~ . - 1))
} else if (alcohol_r$min < 0 | alcohol_r$max > 4) {
  warning("Alcohol out of unexpected range — inspect manually.")
} else {
  cat("Alcohol already on 0–4 scale.\n")
}

# Set items 2 and 3 to 0 for non-drinkers (Alcohol_Frequentie == 0)
n_nondrinker <- sum(df_l$Alcohol_Frequentie == 0, na.rm = TRUE)
cat("  Non-drinkers (Alcohol_Frequentie == 0):", n_nondrinker,
    "— items 2 and 3 set to 0.\n")
df_l <- df_l |>
  mutate(
    Alcohol_AantalDag  = if_else(Alcohol_Frequentie == 0, 0L, Alcohol_AantalDag),
    Alcohol_MeerDan6   = if_else(Alcohol_Frequentie == 0, 0L, Alcohol_MeerDan6)
  )

alcohol_bad <- items_out_of_range(df_l, alcohol_items, 0, 4)
result      <- flag_and_remove(df_l, removal_log_l, alcohol_bad,
                               "Alcohol item(s) outside 0–4 range after correction")
df_l <- result$df; removal_log_l <- result$log

df_l <- df_l |>
  mutate(
    Alcohol_AantalDag_R = if_else(Alcohol_AantalDag == 0, 0, Alcohol_AantalDag - 1),
    Alcohol_Totaal      = Alcohol_Frequentie + Alcohol_AantalDag_R + Alcohol_MeerDan6
  )
alcohol_total_bad <- !between(df_l$Alcohol_Totaal, 0, 12) & !is.na(df_l$Alcohol_Totaal)
result <- flag_and_remove(df_l, removal_log_l, alcohol_total_bad,
                          "Alcohol_Totaal outside 0–12")
df_l <- result$df; removal_log_l <- result$log

df_l <- df_l |>
  mutate(Alcohol_Cat = factor(
    case_when(
      Geslacht == "Male"   & Alcohol_Totaal < 5 ~ "No Likelihood",
      Geslacht == "Female" & Alcohol_Totaal < 4 ~ "No Likelihood",
      TRUE                                       ~ "Likelihood Present"
    ),
    levels = c("No Likelihood", "Likelihood Present")
  ))
cat("Alcohol checks passed.\n")

# ── 2.15 Smoking — L sample only ──────────────────────────────────────────────
# Roken_Frequentie | Item scale: 0–4 (may be stored as 1–5)
# Categories: Non-Smoker (0), Smoker (>0)

cat("\n── Smoking quality check — L ──\n")
roken_range <- range(df_l$Roken_Frequentie, na.rm = TRUE)
cat("Roken_Frequentie range — min:", roken_range[1], "| max:", roken_range[2], "\n")

if (roken_range[1] >= 1 & roken_range[2] <= 5) {
  cat("Roken_Frequentie on 1–5 scale. Recoding to 0–4.\n")
  df_l <- df_l |> mutate(Roken_Frequentie = Roken_Frequentie - 1)
} else if (roken_range[1] < 0 | roken_range[2] > 4) {
  warning("Roken_Frequentie out of range — inspect manually.")
} else {
  cat("Roken_Frequentie already on 0–4 scale.\n")
}
roken_bad <- !between(df_l$Roken_Frequentie, 0, 4) & !is.na(df_l$Roken_Frequentie)
result    <- flag_and_remove(df_l, removal_log_l, roken_bad,
                             "Roken_Frequentie outside 0–4 range after correction")
df_l <- result$df; removal_log_l <- result$log

df_l <- df_l |>
  mutate(Roken_Cat = factor(
    if_else(Roken_Frequentie == 0, "Non-Smoker", "Smoker"),
    levels = c("Non-Smoker", "Smoker")
  ))
cat("Smoking checks passed.\n")

# ── 2.16 Final variable overview ──────────────────────────────────────────────
print_overview <- function(df, log, label) {
  cat("\n══ Recoded variables summary —", label, "══\n")
  df |>
    select(any_of(c(
      "Geslacht", "Leeftijd_Cat", "Opleiding_Cat", "Inkomen_Cat",
      "Disease_Cat", "RiskFactor_Cat",
      "IAS_Totaal", "IATS_Totaal",
      "BDI_Totaal", "BDI_Cat", "TAS_Totaal", "TAS_Cat",
      "IPAQ_Cat", "SSRQ_Totaal", "SSRQ_Cat", "EEQ_Totaal", "EEQ_Cat",
      "JSS_Totaal", "JSS_Cat", "Alcohol_Totaal", "Alcohol_Cat", "Roken_Cat"
    ))) |>
    summary() |> print()
  cat("\n── Removal log —", label, "──\n")
  if (nrow(log) == 0) {
    cat("No participants removed.\n")
  } else {
    cat("Total removed:", nrow(log), "\n")
    print(log |> group_by(reason) |> summarise(n = n(), .groups = "drop"), n = Inf)
  }
  cat("Final n:", nrow(df), "\n")
}

print_overview(df_da, removal_log_da, "DA")
print_overview(df_l,  removal_log_l,  "L")

df_da_recoded <- df_da
df_l_recoded  <- df_l
cat("\nSection 2 complete.\n")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 3 — SEP Calculation from Education and Income
# ══════════════════════════════════════════════════════════════════════════════
# No MICE imputation. SEP is derived directly from Opleiding_Cat and Inkomen_Cat
# using a rule-based sum score.
#
# SEP_Score = Opleiding_Num + Inkomen_Num   (range 2–6)
#   Opleiding_Cat: Low = 1, Middle = 2, High = 3
#   Inkomen_Cat:   Below average = 1, Average = 2, Above average = 3
#
# SEP_3Cat (the variable entered into the LCA):
#   ≤ 3  → Below Average SEP  (low+low, low+average, average+low)
#   = 4  → Average SEP        (low+high, middle+middle, high+low)
#   ≥ 5  → Above Average SEP  (middle+high, high+middle, high+high)
#
# Participants missing Opleiding_Cat or Inkomen_Cat receive NA on SEP_3Cat and
# are excluded via complete-case deletion (after MCAR/MAR diagnostics in 3.1).
# ══════════════════════════════════════════════════════════════════════════════

calculate_sep <- function(df, label) {
  
  cat("\n══ SEP calculation —", label, "══\n")
  
  # Report missingness on the two components
  n_miss_opl <- sum(is.na(df$Opleiding_Cat))
  n_miss_ink <- sum(is.na(df$Inkomen_Cat))
  cat("Missing Opleiding_Cat:", n_miss_opl,
      "(", round(100 * n_miss_opl / nrow(df), 1), "%)\n")
  cat("Missing Inkomen_Cat:  ", n_miss_ink,
      "(", round(100 * n_miss_ink / nrow(df), 1), "%)\n")
  
  df <- df |>
    mutate(
      # Convert ordered factors to numeric scores (Low/Below=1, Middle/Avg=2, High/Above=3)
      Opleiding_Num = as.integer(Opleiding_Cat),
      Inkomen_Num   = as.integer(Inkomen_Cat),
      SEP_Score     = Opleiding_Num + Inkomen_Num,
      
      # Three-category SEP entered into the LCA
      SEP_3Cat = factor(
        case_when(
          is.na(SEP_Score) ~ NA_character_,
          SEP_Score <= 3   ~ "Below Average SEP",
          SEP_Score == 4   ~ "Average SEP",
          SEP_Score >= 5   ~ "Above Average SEP"
        ),
        levels = c("Below Average SEP", "Average SEP", "Above Average SEP")
      )
    )
  
  # Report composite score and cross-tabulation
  cat("\nSEP_Score distribution:\n")
  print(table(df$SEP_Score, useNA = "ifany"))
  
  cat("\nEducation × Income cross-tabulation (N):\n")
  print(table(df$Opleiding_Cat, df$Inkomen_Cat, useNA = "ifany",
              dnn = c("Education", "Income")))
  
  cat("\nSEP_3Cat distribution:\n")
  print(table(df$SEP_3Cat, useNA = "ifany"))
  cat("SEP_3Cat missing:", sum(is.na(df$SEP_3Cat)), "\n")
  
  df
}

df_da_recoded <- calculate_sep(df_da_recoded, "DA")
df_l_recoded  <- calculate_sep(df_l_recoded,  "L")

# ── 3.1 MCAR/MAR diagnostics for Education and Income ────────────────────────
# Two complementary checks are run on Opleiding_Cat and Inkomen_Cat BEFORE
# complete-case exclusion, to characterise the missing-data mechanism:
#
#   (a) Little's MCAR test (naniar::mcar_test) on the SEP components together
#       with observed auxiliary variables (age, sex, disease status, risk
#       factors). Factors are coerced to integer codes for the test. Little's
#       test assumes multivariate normality, so with categorical data the
#       result is interpreted as a heuristic rather than a formal test.
#
#   (b) Logistic regression of each missingness indicator (1 = missing,
#       0 = observed) on the same auxiliary variables, with a likelihood-ratio
#       test vs. the null model. A significant joint LR test indicates that
#       missingness is systematically related to observed data — inconsistent
#       with MCAR and consistent with MAR.
#
# Neither procedure can distinguish MAR from MNAR; conclusions are therefore
# qualified as "no evidence against MCAR" or "consistent with MAR".

check_sep_missingness <- function(df, label) {
  
  cat("\n══ MCAR/MAR diagnostics for SEP components —", label, "══\n")
  
  # Auxiliary variables assumed to be observed for (nearly) all participants
  aux_vars <- intersect(
    c("Leeftijd", "Geslacht", "Disease_Cat", "RiskFactor_Cat"),
    names(df)
  )
  cat("Auxiliary variables:", paste(aux_vars, collapse = ", "), "\n")
  
  n_miss_opl <- sum(is.na(df$Opleiding_Cat))
  n_miss_ink <- sum(is.na(df$Inkomen_Cat))
  cat("Missing Opleiding_Cat:", n_miss_opl,
      "| Missing Inkomen_Cat:", n_miss_ink, "\n")
  
  if (n_miss_opl == 0 && n_miss_ink == 0) {
    cat("No missingness on SEP components — diagnostics skipped.\n")
    return(invisible(NULL))
  }
  
  # (a) Little's MCAR test --------------------------------------------------
  mcar_data <- df |>
    select(Opleiding_Cat, Inkomen_Cat, any_of(aux_vars)) |>
    mutate(across(where(is.factor), as.integer))
  
  mcar_res <- tryCatch(
    mcar_test(mcar_data),
    error = function(e) {
      cat("\nLittle's MCAR test failed:", e$message, "\n"); NULL
    }
  )
  
  if (!is.null(mcar_res)) {
    cat("\n(a) Little's MCAR test (heuristic for mixed/categorical data):\n")
    print(mcar_res)
    cat(if (mcar_res$p.value < 0.05)
      "  → Reject MCAR (p < .05): missingness associated with observed data (consistent with MAR).\n"
      else
        "  → Fail to reject MCAR (p ≥ .05): no evidence against MCAR.\n")
  }
  
  # (b) Logistic regression of missingness on auxiliaries -------------------
  run_miss_logit <- function(var_name) {
    miss_ind <- as.integer(is.na(df[[var_name]]))
    
    fit_df <- df |>
      select(any_of(aux_vars)) |>
      mutate(.miss = miss_ind) |>
      drop_na(any_of(aux_vars))  # require complete auxiliaries for the logit
    
    # Guard against unstable fits
    if (sum(fit_df$.miss) < 10 || (nrow(fit_df) - sum(fit_df$.miss)) < 10) {
      cat("\n  Insufficient cases for logit on", var_name,
          "(missing =", sum(fit_df$.miss),
          ", observed =", nrow(fit_df) - sum(fit_df$.miss), ") — skipped.\n")
      return(invisible(NULL))
    }
    
    fit_full <- tryCatch(
      glm(.miss ~ ., data = fit_df, family = binomial()),
      error = function(e) { cat("  Logit failed:", e$message, "\n"); NULL }
    )
    fit_null <- glm(.miss ~ 1, data = fit_df, family = binomial())
    
    if (is.null(fit_full)) return(invisible(NULL))
    
    lr_p <- anova(fit_null, fit_full, test = "Chisq")$`Pr(>Chi)`[2]
    
    cat("\n(b) Missingness on", var_name, "~", paste(aux_vars, collapse = " + "), "\n")
    cat("    Joint LR test p-value:", signif(lr_p, 3), "\n")
    cat(if (lr_p < 0.05)
      "    → Observed covariates jointly predict missingness — inconsistent with MCAR, consistent with MAR.\n"
      else
        "    → No evidence that observed covariates predict missingness on this variable.\n")
    print(tidy(fit_full, conf.int = TRUE, exponentiate = FALSE))
  }
  
  if (n_miss_opl > 0) run_miss_logit("Opleiding_Cat")
  if (n_miss_ink > 0) run_miss_logit("Inkomen_Cat")
  
  invisible(NULL)
}

check_sep_missingness(df_da_recoded, "DA")
check_sep_missingness(df_l_recoded,  "L")

# ── 3.2 Exclude participants with missing SEP components ──────────────────────
# Participants are logged in the removal log before being dropped so they appear
# in the exported removal log Excel file alongside Section 2 exclusions.
#
# Justification for complete-case deletion: if the MCAR/MAR diagnostics above
# suggest MCAR, complete-case analysis is unbiased; under plausible MAR,
# complete-case analysis remains valid for the LCA provided that the variables
# driving missingness are unrelated to the latent class structure — this should
# be reconsidered in sensitivity analyses if 3.1 flags strong MAR signals.

sep_missing_da <- is.na(df_da_recoded$SEP_Score)
sep_missing_l  <- is.na(df_l_recoded$SEP_Score)

if (any(sep_missing_da)) {
  new_entries <- df_da_recoded[sep_missing_da, c("Persoon_ID", "sample")] |>
    mutate(reason = "Missing SEP component (Opleiding_Cat or Inkomen_Cat)")
  removal_log_da <- bind_rows(removal_log_da, new_entries)
}
if (any(sep_missing_l)) {
  new_entries <- df_l_recoded[sep_missing_l, c("Persoon_ID", "sample")] |>
    mutate(reason = "Missing SEP component (Opleiding_Cat or Inkomen_Cat)")
  removal_log_l <- bind_rows(removal_log_l, new_entries)
}

n_before_da   <- nrow(df_da_recoded)
n_before_l    <- nrow(df_l_recoded)

df_da_recoded <- df_da_recoded |> filter(!is.na(SEP_Score))
df_l_recoded  <- df_l_recoded  |> filter(!is.na(SEP_Score))

cat("\n── SEP complete-case exclusions ──\n")
cat("DA: excluded", n_before_da - nrow(df_da_recoded),
    "| retained", nrow(df_da_recoded), "\n")
cat("L:  excluded", n_before_l  - nrow(df_l_recoded),
    "| retained", nrow(df_l_recoded),  "\n")

# ── 3.3 Missing data check on analysis-relevant variables ─────────────────────
# Restricted to LCA indicators, outcomes, and key demographics.
# Raw item columns (IAS_1–IAS_21, BDI_1–BDI_21, etc.) are excluded from this
# check — their item-level quality was already verified in Section 2.

analysis_vars <- c(
  # Outcomes
  "IAS_Totaal", "IATS_Totaal",
  # Demographics and SEP
  "Leeftijd", "Leeftijd_Cat", "Geslacht",
  "Opleiding_Cat", "Inkomen_Cat", "SEP_Score", "SEP_3Cat",
  # Disease and risk factors
  "Disease_Cat", "RiskFactor_Cat",
  # DA-only LCA indicators
  "BDI_Totaal", "BDI_Cat", "TAS_Totaal", "TAS_Cat",
  # L-only LCA indicators
  "IPAQ_Cat",
  "Alcohol_Totaal", "Alcohol_Cat",
  "Roken_Cat",
  "SSRQ_Totaal", "SSRQ_Cat",
  "EEQ_Totaal", "EEQ_Cat",
  "JSS_Totaal", "JSS_Cat"
)

cat("\n── Missing data (analysis variables) — DA ──\n")
df_da_recoded |>
  select(any_of(analysis_vars)) |>
  miss_var_summary() |>
  filter(pct_miss > 0) |>
  print(n = Inf)

cat("\n── Missing data (analysis variables) — L ──\n")
df_l_recoded |>
  select(any_of(analysis_vars)) |>
  miss_var_summary() |>
  filter(pct_miss > 0) |>
  print(n = Inf)

cat("\nSection 3 complete.\n")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 4 — Normality & Multicollinearity Checks
# ══════════════════════════════════════════════════════════════════════════════

# ── 4.1 Define variable sets ──────────────────────────────────────────────────
# Continuous variables for normality checks.
# IAS_Totaal and IATS_Totaal (outcomes) are included — useful to inspect their
# distributions before modelling, even though OLS requires normality of residuals
# rather than of the outcome itself.
cont_da <- c("IAS_Totaal", "IATS_Totaal", "Leeftijd", "BDI_Totaal", "TAS_Totaal")
cont_l  <- c("IAS_Totaal", "IATS_Totaal", "Leeftijd",
             "SSRQ_Totaal", "EEQ_Totaal", "JSS_Totaal", "Alcohol_Totaal")

# ── 4.2 Normality helper functions ────────────────────────────────────────────
recommend_test <- function(n) {
  if (n < 2000) "Shapiro-Wilk recommended (n < 2000)" else
    "Kolmogorov-Smirnov recommended (n ≥ 2000)"
}

run_normality_single <- function(df, vars, label) {
  n_val <- nrow(df)
  cat("\n=== Normality —", label, "| n =", n_val,
      "|", recommend_test(n_val), "===\n")
  results <- bind_rows(lapply(vars, function(v) {
    x    <- as.numeric(df[[v]])
    x    <- x[!is.na(x)]
    n    <- length(x)
    skew <- as.numeric(psych::describe(x)$skew)
    kurt <- as.numeric(psych::describe(x)$kurtosis)
    sw_stat <- NA_real_; sw_p <- NA_real_
    tryCatch({ sw <- shapiro.test(x); sw_stat <- sw$statistic; sw_p <- sw$p.value },
             error = function(e) NULL)
    ks      <- ks.test(x, "pnorm", mean(x), sd(x))
    data.frame(
      variable   = v, n = as.integer(n),
      mean_val   = round(mean(x), 3), sd_val = round(sd(x), 3),
      skewness   = round(skew, 3), skew_flag = if_else(abs(skew) >= 2, "FLAG", "OK"),
      ex_kurt    = round(kurt, 3), kurt_flag = if_else(abs(kurt) >= 7, "FLAG", "OK"),
      SW_W       = round(sw_stat, 3), SW_p = round(sw_p, 3),
      SW_flag    = if_else(is.na(sw_p), "SW unavailable",
                           if_else(sw_p < 0.05, "Non-normal (SW)", "Normal (SW)")),
      KS_D       = round(ks$statistic, 3), KS_p = round(ks$p.value, 3),
      KS_flag    = if_else(ks$p.value < 0.05, "Non-normal (KS)", "Normal (KS)"),
      stringsAsFactors = FALSE
    )
  }))
  print(as.data.frame(results), row.names = FALSE)
  invisible(results)
}

norm_da <- run_normality_single(df_da_recoded, cont_da, "DA sample")
norm_l  <- run_normality_single(df_l_recoded,  cont_l,  "L sample")

# ── 4.3 Normality visualisation ───────────────────────────────────────────────
plot_normality <- function(df, vars, title) {
  plots <- lapply(vars, function(v) {
    x   <- as.numeric(df[[v]])
    dat <- data.frame(x = x)
    qq <- ggplot(dat, aes(sample = x)) +
      stat_qq(alpha = 0.4, colour = "#3182bd") +
      stat_qq_line(colour = "#e34a33") +
      labs(title = v, x = "Theoretical quantiles", y = "Sample quantiles") +
      theme_minimal(base_size = 10)
    dens <- ggplot(dat, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30,
                     fill = "#3182bd", alpha = 0.5) +
      geom_density(colour = "#e34a33", linewidth = 0.8) +
      labs(x = v, y = "Density") +
      theme_minimal(base_size = 10)
    qq + dens
  })
  wrap_plots(plots, ncol = 2) + plot_annotation(title = title)
}

plot_normality(df_da_recoded, cont_da, "Normality — DA sample")
plot_normality(df_l_recoded,  cont_l,  "Normality — L sample")

# ── 4.4 Cramér's V helpers (LCA indicators) ───────────────────────────────────
cramers_v <- function(x, y) {
  tab <- table(x, y); n <- sum(tab)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  k   <- min(nrow(tab), ncol(tab))
  if (k <= 1 || n == 0) return(NA_real_)
  as.numeric(sqrt(chi / (n * (k - 1))))
}

cramers_v_matrix <- function(df, vars, label, threshold = 0.50) {
  n   <- length(vars)
  mat <- matrix(NA_real_, n, n, dimnames = list(vars, vars))
  for (i in seq_len(n))
    for (j in seq_len(n))
      mat[i, j] <- if (i == j) 1 else cramers_v(df[[vars[i]]], df[[vars[j]]])
  mat_r <- round(mat, 3)
  cat("\nCramér's V matrix —", label, ":\n"); print(mat_r)
  flagged <- which(upper.tri(mat) & mat > threshold, arr.ind = TRUE)
  if (nrow(flagged) == 0) cat("\n  OK — no pairs with V >", threshold, "\n")
  else {
    cat("\n  FLAG — pairs with V >", threshold, ":\n")
    for (r in seq_len(nrow(flagged))) {
      i <- flagged[r, 1]; j <- flagged[r, 2]
      cat("    ", vars[i], "~", vars[j], " V =", mat_r[i, j], "  <<< FLAG\n")
    }
  }
  invisible(mat_r)
}

plot_cramers_v <- function(v_mat, label) {
  diag(v_mat) <- NA
  col_ramp <- colorRampPalette(c("#ffffff", "#d1e5f0", "#4393c3", "#2166ac"))(200)
  corrplot(v_mat, is.corr = FALSE, method = "color", type = "upper",
           col = col_ramp, addCoef.col = "black", number.cex = 0.70,
           tl.cex = 0.80, na.label = " ",
           title = paste("Cramér's V — LCA indicators:", label),
           mar = c(0, 0, 2, 0))
}


# ── 4.5 Multicollinearity checks — LCA indicators (Cramér's V) ────────────────
# LCA variable sets defined here (single source of truth used in Section 7)
lca_vars_da <- c("Leeftijd_Cat", "Geslacht", "SEP_3Cat", "Disease_Cat",
                 "RiskFactor_Cat", "BDI_Cat", "TAS_Cat")

lca_vars_l  <- c("Leeftijd_Cat", "Geslacht", "SEP_3Cat", "Disease_Cat",
                 "RiskFactor_Cat", "IPAQ_Cat", "Alcohol_Cat", "Roken_Cat",
                 "SSRQ_Cat", "EEQ_Cat", "JSS_Cat")

cat("\n══ Cramér's V — LCA indicators — DA ══\n")
v_mat_da <- cramers_v_matrix(df_da_recoded, lca_vars_da, "DA")
plot_cramers_v(v_mat_da, "DA sample")

cat("\n══ Cramér's V — LCA indicators — L ══\n")
v_mat_l <- cramers_v_matrix(df_l_recoded, lca_vars_l, "L")
plot_cramers_v(v_mat_l, "L sample")

cat("\nSection 4 complete.\n")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 5 — Outlier Detection
# ══════════════════════════════════════════════════════════════════════════════

# Continuous variable sets for outlier detection (outcomes excluded)
cont_outlier_da <- c("Leeftijd", "BDI_Totaal", "TAS_Totaal")
cont_outlier_l  <- c("Leeftijd", "SSRQ_Totaal", "EEQ_Totaal",
                     "JSS_Totaal", "Alcohol_Totaal")

# ── 5.1 Univariate Z-score helper ─────────────────────────────────────────────
compute_z_flags <- function(data, vars) {
  present <- intersect(vars, names(data))
  if (length(present) == 0) return(rep(FALSE, nrow(data)))
  flag_mat <- sapply(present, function(v) {
    x     <- data[[v]]; mu <- mean(x, na.rm = TRUE); sigma <- sd(x, na.rm = TRUE)
    z     <- (x - mu) / sigma
    ifelse(is.na(z), FALSE, abs(z) > 3)
  })
  if (!is.matrix(flag_mat)) flag_mat <- matrix(flag_mat, ncol = 1)
  cat("  Univariate outlier counts per variable (|z| > 3):\n")
  counts <- colSums(flag_mat); names(counts) <- present; print(counts)
  rowSums(flag_mat) > 0
}

# ── 5.2 Mahalanobis distance helper ───────────────────────────────────────────
compute_mahal_flags <- function(data, vars, label) {
  present <- intersect(vars, names(data))
  if (length(present) < 2) {
    cat("  Fewer than 2 variables for", label, "— skipping.\n")
    return(rep(FALSE, nrow(data)))
  }
  mat      <- data |> select(all_of(present)) |> as.matrix()
  complete <- complete.cases(mat)
  if (sum(complete) < (length(present) + 1)) {
    warning("Insufficient complete cases for Mahalanobis in ", label)
    return(rep(FALSE, nrow(data)))
  }
  mat_c   <- mat[complete, , drop = FALSE]
  cov_mat <- cov(mat_c)
  if (det(cov_mat) < .Machine$double.eps) {
    warning("Singular covariance matrix for ", label)
    return(rep(FALSE, nrow(data)))
  }
  mah_dist <- mahalanobis(mat_c, colMeans(mat_c), cov_mat)
  crit_val <- qchisq(0.999, df = length(present))
  cat("  Variables used:", paste(present, collapse = ", "), "\n")
  cat("  Critical value (chi-sq df =", length(present), ", p = .001):",
      round(crit_val, 2), "\n")
  cat("  Distance range:", round(min(mah_dist), 2), "–",
      round(max(mah_dist), 2), "\n")
  cat("  Flagged:", sum(mah_dist > crit_val), "\n")
  flags_full           <- rep(FALSE, nrow(data))
  flags_full[complete] <- mah_dist > crit_val
  flags_full
}

# ── 5.3 Run outlier detection — DA ────────────────────────────────────────────
cat("\n── Outlier detection — DA ──\n")
flag_z_da     <- compute_z_flags(df_da_recoded, cont_outlier_da)
flag_mahal_da <- compute_mahal_flags(df_da_recoded, cont_outlier_da, "DA")
flag_either_da <- flag_z_da | flag_mahal_da
cat("  Univariate Z-score flags:", sum(flag_z_da),      "\n")
cat("  Mahalanobis flags:       ", sum(flag_mahal_da),  "\n")
cat("  Combined (either):       ", sum(flag_either_da), "\n")

result         <- flag_and_remove(df_da_recoded, removal_log_da, flag_either_da,
                                  "Outlier: Z-score or Mahalanobis")
df_da_recoded  <- result$df; removal_log_da <- result$log

# ── 5.4 Run outlier detection — L ─────────────────────────────────────────────
cat("\n── Outlier detection — L ──\n")
flag_z_l     <- compute_z_flags(df_l_recoded, cont_outlier_l)
flag_mahal_l <- compute_mahal_flags(df_l_recoded, cont_outlier_l, "L")
flag_either_l <- flag_z_l | flag_mahal_l
cat("  Univariate Z-score flags:", sum(flag_z_l),      "\n")
cat("  Mahalanobis flags:       ", sum(flag_mahal_l),  "\n")
cat("  Combined (either):       ", sum(flag_either_l), "\n")

result        <- flag_and_remove(df_l_recoded, removal_log_l, flag_either_l,
                                 "Outlier: Z-score or Mahalanobis")
df_l_recoded  <- result$df; removal_log_l <- result$log

# ── 5.5 Final sample sizes and export ─────────────────────────────────────────
cat("\n── Final sample sizes after all exclusions ──\n")
cat("DA:", nrow(df_da_recoded), "\n")
cat("L: ", nrow(df_l_recoded),  "\n")

# Export combined removal log
sec5_reasons <- "Outlier: Z-score or Mahalanobis"

export_combined <- bind_rows(
  removal_log_da |> mutate(section = if_else(reason %in% sec5_reasons,
                                             "Section 5 — Outlier detection",
                                             "Section 2–3 — Data quality")),
  removal_log_l  |> mutate(section = if_else(reason %in% sec5_reasons,
                                             "Section 5 — Outlier detection",
                                             "Section 2–3 — Data quality"))
) |> select(section, sample, Persoon_ID, reason) |> arrange(section, sample)

summary_log <- export_combined |>
  group_by(section, sample, reason) |>
  summarise(n_removed = n(), .groups = "drop") |>
  arrange(section, sample)

writexl::write_xlsx(
  list(Summary  = as.data.frame(summary_log),
       Full_log = as.data.frame(export_combined)),
  path = file.path(base_path, "removal_log.xlsx")
)
cat("Removal log exported.\n")

df_da_clean <- df_da_recoded
df_l_clean  <- df_l_recoded
cat("Section 5 complete. df_da_clean (n =", nrow(df_da_clean),
    ") and df_l_clean (n =", nrow(df_l_clean), ") ready.\n")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 6 — Descriptive Statistics & Internal Consistency
# ══════════════════════════════════════════════════════════════════════════════

# ── 6.1 Helper functions ──────────────────────────────────────────────────────
fmt_continuous  <- function(df, v) {
  x <- as.numeric(df[[v]])
  paste0(round(mean(x, na.rm = TRUE), 1), " (", round(sd(x, na.rm = TRUE), 1), ")")
}

fmt_categorical <- function(df, v, level) {
  if (!v %in% names(df)) return("")
  x   <- df[[v]]
  n   <- sum(x == level, na.rm = TRUE)
  tot <- sum(!is.na(x))
  pct <- if (tot > 0) round(100 * n / tot, 1) else 0
  paste0(n, " (", pct, ")")
}

row_header    <- function(l, da = "", lv = "") data.frame(Variable = l, DA = da, L = lv, row_type = "header",    stringsAsFactors = FALSE)
row_subheader <- function(l)                   data.frame(Variable = l, DA = "", L = "",  row_type = "subheader", stringsAsFactors = FALSE)
row_data      <- function(l, da, lv)           data.frame(Variable = l, DA = da, L = lv, row_type = "data",      stringsAsFactors = FALSE)

n_da <- nrow(df_da_clean)
n_l  <- nrow(df_l_clean)

# ── 6.2 Build descriptive table ───────────────────────────────────────────────
tbl <- bind_rows(
  row_header("Continuous variables", "M (SD)", "M (SD)"),
  row_data("Age, years",
           fmt_continuous(df_da_clean, "Leeftijd"),
           fmt_continuous(df_l_clean,  "Leeftijd")),
  row_data("IAS total score",
           fmt_continuous(df_da_clean, "IAS_Totaal"),
           fmt_continuous(df_l_clean,  "IAS_Totaal")),
  row_data("IATS total score",
           fmt_continuous(df_da_clean, "IATS_Totaal"),
           fmt_continuous(df_l_clean,  "IATS_Totaal")),
  
  row_header("Categorical variables", "N (%)", "N (%)"),
  row_subheader("Demographics"),
  row_data("Female sex",
           fmt_categorical(df_da_clean, "Geslacht", "Female"),
           fmt_categorical(df_l_clean,  "Geslacht", "Female")),
  row_data("Age ≥45 years",
           fmt_categorical(df_da_clean, "Leeftijd_Cat", "45 or older"),
           fmt_categorical(df_l_clean,  "Leeftijd_Cat", "45 or older")),
  
  row_subheader("Calculated SEP (3-category)"),
  row_data("Below Average SEP",
           fmt_categorical(df_da_clean, "SEP_3Cat", "Below Average SEP"),
           fmt_categorical(df_l_clean,  "SEP_3Cat", "Below Average SEP")),
  row_data("Average SEP",
           fmt_categorical(df_da_clean, "SEP_3Cat", "Average SEP"),
           fmt_categorical(df_l_clean,  "SEP_3Cat", "Average SEP")),
  row_data("Above Average SEP",
           fmt_categorical(df_da_clean, "SEP_3Cat", "Above Average SEP"),
           fmt_categorical(df_l_clean,  "SEP_3Cat", "Above Average SEP")),
  
  row_subheader("Chronic Disease Status"),
  row_data("No chronic conditions",
           fmt_categorical(df_da_clean, "Disease_Cat", "No Disease"),
           fmt_categorical(df_l_clean,  "Disease_Cat", "No Disease")),
  row_data("Single chronic condition",
           fmt_categorical(df_da_clean, "Disease_Cat", "One Disease"),
           fmt_categorical(df_l_clean,  "Disease_Cat", "One Disease")),
  row_data("Multimorbidity (≥2 conditions)",
           fmt_categorical(df_da_clean, "Disease_Cat", "Multimorbid"),
           fmt_categorical(df_l_clean,  "Disease_Cat", "Multimorbid")),
  
  row_subheader("Risk Factors"),
  row_data("At least one risk factor present",
           fmt_categorical(df_da_clean, "RiskFactor_Cat", "Present"),
           fmt_categorical(df_l_clean,  "RiskFactor_Cat", "Present")),
  row_data("No risk factors",
           fmt_categorical(df_da_clean, "RiskFactor_Cat", "Absent"),
           fmt_categorical(df_l_clean,  "RiskFactor_Cat", "Absent")),
  row_data("  Overweight/obesity",
           fmt_categorical(df_da_clean, "Overgewicht_Obesitas", 1),
           fmt_categorical(df_l_clean,  "Overgewicht_Obesitas", 1)),
  row_data("  High blood pressure",
           fmt_categorical(df_da_clean, "Hoge_Bloeddruk", 1),
           fmt_categorical(df_l_clean,  "Hoge_Bloeddruk", 1)),
  row_data("  Increased cholesterol",
           fmt_categorical(df_da_clean, "Verhoogd_Cholesterol", 1),
           fmt_categorical(df_l_clean,  "Verhoogd_Cholesterol", 1)),
  
  row_subheader("Psychological Variables (DA only)"),
  row_data("Low depressive symptoms",
           fmt_categorical(df_da_clean, "BDI_Cat", "Low depressive symptoms"), ""),
  row_data("Clinically relevant depressive symptoms",
           fmt_categorical(df_da_clean, "BDI_Cat",
                           "Clinically relevant depressive symptoms"), ""),
  row_data("Alexithymia present",
           fmt_categorical(df_da_clean, "TAS_Cat", "Alexithymia Present"), ""),
  
  row_subheader("Lifestyle Behaviours (L only)"),
  row_data("Physically active", "",
           fmt_categorical(df_l_clean, "IPAQ_Cat", "Active")),
  row_data("Non-emotional eater", "",
           fmt_categorical(df_l_clean, "EEQ_Cat", "Non-Emotional Eater")),
  row_data("Low emotional eater", "",
           fmt_categorical(df_l_clean, "EEQ_Cat", "Low Emotional Eater")),
  row_data("Emotional to very emotional eater", "",
           fmt_categorical(df_l_clean, "EEQ_Cat",
                           "Emotional to Very Emotional Eater")),
  row_data("Likelihood of alcohol dependency", "",
           fmt_categorical(df_l_clean, "Alcohol_Cat", "Likelihood Present")),
  row_data("Current smoker", "",
           fmt_categorical(df_l_clean, "Roken_Cat", "Smoker")),
  row_data("Disturbed sleep", "",
           fmt_categorical(df_l_clean, "JSS_Cat", "Disturbed Sleep")),
  row_data("Below average self-regulation", "",
           fmt_categorical(df_l_clean, "SSRQ_Cat", "Below Average Self-Regulation"))
)

col_da <- paste0("DA-dataset (N = ", n_da, ")")
col_l  <- paste0("L-dataset (N = ",  n_l,  ")")
print(as.data.frame(tbl |> select(Variable, DA, L) |>
                      setNames(c("Variable", col_da, col_l))),
      row.names = FALSE)

# ── 6.3 Export descriptive statistics to Excel ────────────────────────────────
wb_desc <- createWorkbook()
addWorksheet(wb_desc, "Descriptive Statistics")

sty_head <- createStyle(fontName = "Calibri", fontSize = 11, textDecoration = "bold",
                        border = c("top", "bottom"), borderStyle = "thin",
                        halign = "center")
sty_sec  <- createStyle(fontName = "Calibri", fontSize = 11, textDecoration = "bold")
sty_sub  <- createStyle(fontName = "Calibri", fontSize = 11, textDecoration = "italic")
sty_dat  <- createStyle(fontName = "Calibri", fontSize = 11, indent = 1)
sty_last <- createStyle(fontName = "Calibri", fontSize = 11, indent = 1,
                        border = "bottom", borderStyle = "thin")

writeData(wb_desc, 1, as.data.frame(t(c("Variable", col_da, col_l))),
          startRow = 1, startCol = 1, colNames = FALSE)
addStyle(wb_desc, 1, sty_head, rows = 1, cols = 1:3, gridExpand = TRUE)

for (i in seq_len(nrow(tbl))) {
  r    <- i + 1
  vals <- as.character(unlist(tbl[i, c("Variable", "DA", "L")]))
  writeData(wb_desc, 1, as.data.frame(t(vals)),
            startRow = r, startCol = 1, colNames = FALSE)
  sty <- switch(tbl$row_type[i],
                header    = sty_sec,
                subheader = sty_sub,
                if (i == nrow(tbl)) sty_last else sty_dat)
  addStyle(wb_desc, 1, sty, rows = r, cols = 1:3, gridExpand = TRUE)
}

setColWidths(wb_desc, 1, cols = 1:3, widths = c(52, 22, 22))
saveWorkbook(wb_desc,
             file.path(base_path, "descriptive_statistics.xlsx"),
             overwrite = TRUE)
cat("\nDescriptive statistics exported.\n")

# ── 6.4 Internal consistency ──────────────────────────────────────────────────
scales <- list(
  list(label = "Interoceptive Accuracy Scale (IAS)",
       items = paste0("IAS_",  1:21), sample = "Both"),
  list(label = "Interoceptive Attention Scale (IATS)",
       items = paste0("IATS_", 1:21), sample = "Both"),
  list(label = "Beck Depression Inventory (BDI)",
       items = paste0("BDI_",  1:21), sample = "DA"),
  list(label = "Toronto Alexithymia Scale (TAS)",
       items = paste0("TAS_",  1:20), sample = "DA"),
  list(label = "Short Self-Regulation Questionnaire (SSRQ)",
       items = paste0("SSRQ_", 1:31), sample = "L"),
  list(label = "Emotional Eating Questionnaire (EEQ)",
       items = paste0("EEQ_",  1:10), sample = "L"),
  list(label = "Jenkins Sleep Scale (JSS)",
       items = paste0("JSS_",  1:4),  sample = "L"),
  list(label = "Alcohol Use Disorders Identification Test (Alcohol)",
       items = c("Alcohol_Frequentie", "Alcohol_AantalDag_R", "Alcohol_MeerDan6"),
       sample = "L")
)

compute_reliability <- function(df, scale_entry, sample_label) {
  items   <- intersect(scale_entry$items, names(df))
  n_items <- length(items)
  if (n_items < 2) {
    message("  Skipping '", scale_entry$label, "' — fewer than 2 items found.")
    return(NULL)
  }
  mat <- df |> select(all_of(items)) |>
    mutate(across(everything(), as.numeric)) |> as.data.frame()
  mat <- mat[rowSums(!is.na(mat)) >= 2, ]
  n   <- nrow(mat)
  if (n < 10) {
    message("  Skipping '", scale_entry$label, "' — fewer than 10 complete cases.")
    return(NULL)
  }
  a_obj   <- suppressWarnings(psych::alpha(mat, check.keys = FALSE))
  alpha_r <- round(a_obj$total$raw_alpha, 3)
  alpha_s <- round(a_obj$total$std.alpha, 3)
  omega_t <- tryCatch({
    suppressWarnings(suppressMessages({
      o <- psych::omega(mat, plot = FALSE, nfactors = 1)
      round(o$omega.tot, 3)
    }))
  }, error = function(e) { message("  omega() failed: ", e$message); NA_real_ })
  data.frame(Scale = scale_entry$label, Sample = sample_label,
             N_items = n_items, N_valid = n,
             Alpha_raw = alpha_r, Alpha_std = alpha_s, Omega_total = omega_t,
             stringsAsFactors = FALSE)
}

reliability_rows <- list()
for (sc in scales) {
  cat("\n──", sc$label, "(", sc$sample, ") ──")
  if (sc$sample %in% c("Both", "DA")) {
    r <- compute_reliability(df_da_clean, sc, "DA")
    if (!is.null(r)) {
      reliability_rows[[length(reliability_rows) + 1]] <- r
      cat("\n  DA — α (raw):", r$Alpha_raw, "| α (std):", r$Alpha_std,
          "| ω:", r$Omega_total, "\n")
    }
  }
  if (sc$sample %in% c("Both", "L")) {
    r <- compute_reliability(df_l_clean, sc, "L")
    if (!is.null(r)) {
      reliability_rows[[length(reliability_rows) + 1]] <- r
      cat("\n  L  — α (raw):", r$Alpha_raw, "| α (std):", r$Alpha_std,
          "| ω:", r$Omega_total, "\n")
    }
  }
}

reliability_tbl <- bind_rows(reliability_rows)
cat("\n═══ Internal consistency summary ═══\n")
print(as.data.frame(reliability_tbl), row.names = FALSE)
cat("\nSection 6 complete.\n")


# ══════════════════════════════════════════════════════════════════════════════
# SHARED HELPER FUNCTIONS — LCA & REGRESSION
# (defined once; used in Section 7 and 8)
# ══════════════════════════════════════════════════════════════════════════════

# ── LCA fit index helpers ─────────────────────────────────────────────────────
calc_entropy <- function(posterior) {
  K <- ncol(posterior)
  if (K == 1) return(NA_real_)
  N <- nrow(posterior)
  p <- pmax(posterior, 1e-10)
  1 - (-sum(p * log(p))) / (N * log(K))
}

# ── Run LCA across K = 1–10 on a single dataset ───────────────────────────────
run_lca_single <- function(lca_data, formula, k_range, nrep = 10, seed = 123) {
  fit_rows   <- list()
  model_list <- list()
  for (K in k_range) {
    set.seed(seed + K)
    cat("\n── LCA K =", K, "(nrep =", nrep, ") ──\n")
    tryCatch({
      mod   <- poLCA(formula, data = lca_data, nclass = K,
                     nrep = nrep, verbose = FALSE, graphs = FALSE)
      ent   <- calc_entropy(mod$posterior)
      tbl_k <- table(factor(mod$predclass, levels = 1:K))
      min_p <- round(min(as.numeric(tbl_k) / sum(tbl_k)), 3)
      cat("  BIC:", round(mod$bic, 1), "| Entropy:", round(ent, 3),
          "| Min class prop:", min_p, "\n")
      fit_rows[[as.character(K)]] <- data.frame(
        K = K, AIC = round(mod$aic, 1), BIC = round(mod$bic, 1),
        Entropy = round(ent, 3), Min_class_prop = min_p
      )
      model_list[[as.character(K)]] <- mod
    }, error = function(e) {
      warning("poLCA failed K = ", K, ": ", e$message)
      fit_rows[[as.character(K)]]   <<- data.frame(K = K, AIC = NA, BIC = NA,
                                                   Entropy = NA, Min_class_prop = NA)
      model_list[[as.character(K)]] <<- NULL
    })
  }
  list(fit_table = bind_rows(fit_rows), models = model_list)
}

# ── LCA formula builder ───────────────────────────────────────────────────────
make_lca_formula <- function(vars) {
  as.formula(paste0("cbind(", paste(vars, collapse = ", "), ") ~ 1"))
}

# ── LCA category frequency check ─────────────────────────────────────────────
check_lca_frequencies <- function(df, vars, label, threshold = 0.10) {
  cat("\n=== LCA indicator frequencies —", label, "===\n")
  for (v in vars) {
    tbl  <- table(df[[v]], useNA = "ifany")
    prop <- prop.table(tbl)
    result <- data.frame(N    = as.integer(tbl),
                         Pct  = round(as.numeric(prop) * 100, 1),
                         Flag = ifelse(prop < threshold, "FLAG < 10%", "OK"))
    cat("\nVariable:", v, "\n"); print(result)
  }
}

# ── K selection ───────────────────────────────────────────────────────────────
# Sequential procedure (matches Methods section):
#   Step 1 — Exclude K where any class < min_class_prop of sample
#   Step 2 — Select lowest BIC among eligible K
#   Step 3 — AIC tiebreaker when two or more K are within ΔBIC ≤ bic_tol
#   Quality — Entropy > .60 considered acceptable (Sorgente et al., 2025)
#   Support — BLRT p-value as supporting evidence (not a selection criterion)

select_best_k <- function(fit_table,
                          bic_col        = "BIC",
                          aic_col        = "AIC",
                          entropy_col    = "Entropy",
                          class_prop_col = "Min_class_prop",
                          entropy_min    = 0.60,
                          min_class_prop = 0.10,
                          bic_tol        = 0,
                          blrt_table     = NULL,
                          label          = "") {
  
  # ── Working table: K ≥ 2, valid BIC ─────────────────────────────────────────
  tbl <- fit_table[fit_table$K >= 2 & !is.na(fit_table[[bic_col]]), ]
  if (nrow(tbl) == 0) stop("select_best_k: no valid rows for K ≥ 2.")
  
  # ΔBIC relative to global minimum (across all K ≥ 2)
  bic_global_min  <- min(tbl[[bic_col]], na.rm = TRUE)
  tbl$delta_bic   <- round(tbl[[bic_col]] - bic_global_min, 1)
  
  # ── Step 1: class-size filter ────────────────────────────────────────────────
  has_cp <- class_prop_col %in% names(tbl)
  if (has_cp) {
    tbl$step1_ok <- !is.na(tbl[[class_prop_col]]) &
      tbl[[class_prop_col]] >= min_class_prop
  } else {
    tbl$step1_ok <- TRUE
  }
  cands <- tbl[tbl$step1_ok, ]
  if (nrow(cands) == 0) {
    warning("select_best_k [", label, "]: all K have a class < ",
            min_class_prop * 100, "% — class-size filter skipped.")
    cands        <- tbl
    tbl$step1_ok <- TRUE
  }
  
  # ── Step 2 & 3: BIC selection + AIC tiebreaker ──────────────────────────────
  bic_cand_min <- min(cands[[bic_col]], na.rm = TRUE)
  tied         <- cands[cands[[bic_col]] - bic_cand_min <= bic_tol, ]
  aic_used     <- nrow(tied) > 1
  winner       <- if (aic_used) tied[which.min(tied[[aic_col]]), ] else
    cands[which.min(cands[[bic_col]]), ]
  selected_k   <- winner$K
  
  # ── Merge BLRT p-values ──────────────────────────────────────────────────────
  if (!is.null(blrt_table) && nrow(blrt_table) > 0) {
    tbl <- dplyr::left_join(
      tbl,
      dplyr::select(blrt_table, K1, p_boot, sig) |>
        dplyr::rename(K = K1, blrt_p = p_boot, blrt_sig = sig),
      by = "K"
    )
  } else {
    tbl$blrt_p   <- NA_real_
    tbl$blrt_sig <- ""
  }
  
  # ── Build display table ──────────────────────────────────────────────────────
  fmt1 <- function(x) ifelse(is.na(x), "—", formatC(round(x, 1), format = "f", digits = 1))
  fmt3 <- function(x) ifelse(is.na(x), "—", formatC(round(x, 3), format = "f", digits = 3))
  fmtp <- function(x) ifelse(is.na(x), "—", formatC(round(x, 2),  format = "f", digits = 2))
  
  # Min class % as percentage string
  min_class_pct <- if (has_cp)
    ifelse(is.na(tbl[[class_prop_col]]), "—",
           paste0(formatC(round(tbl[[class_prop_col]] * 100, 1),
                          format = "f", digits = 1), "%"))
  else rep("—", nrow(tbl))
  
  # Step 1 eligibility
  step1_col <- ifelse(tbl$step1_ok, "Yes", "No  ← excl.")
  
  # Entropy quality flag
  ent_col <- ifelse(
    !is.na(tbl[[entropy_col]]) & tbl[[entropy_col]] > entropy_min,
    "OK", paste0("FLAG ≤ ", entropy_min)
  )
  
  # BLRT p-value with significance marker
  blrt_col <- dplyr::case_when(
    is.na(tbl$blrt_sig)             ~ "—",
    tbl$blrt_sig == "stopped"       ~ "stopped",
    is.na(tbl$blrt_p)               ~ "—",
    tbl$blrt_sig == "*"             ~ paste0(fmtp(tbl$blrt_p), " *"),
    TRUE                            ~ fmtp(tbl$blrt_p)
  )
  
  # Decision column
  decision_col <- dplyr::case_when(
    tbl$K == selected_k & aic_used ~ "SELECTED (AIC tb)",
    tbl$K == selected_k            ~ "SELECTED",
    TRUE                           ~ ""
  )
  
  out <- data.frame(
    K          = tbl$K,
    BIC        = fmt1(tbl[[bic_col]]),
    dBIC       = fmt1(tbl$delta_bic),
    AIC        = fmt1(tbl[[aic_col]]),
    Entropy    = fmt3(tbl[[entropy_col]]),
    Ent_gt60   = ent_col,
    MinClass   = min_class_pct,
    Step1      = step1_col,
    BLRT_p     = blrt_col,
    Decision   = decision_col,
    stringsAsFactors = FALSE
  )
  names(out) <- c("K", "BIC", "dBIC", "AIC", "Entropy", "Ent > .60",
                  "Min class %", "Step 1", "BLRT p", "Decision")
  
  # ── Print ────────────────────────────────────────────────────────────────────
  w <- 90
  cat("\n", strrep("═", w), "\n", sep = "")
  cat("  K SELECTION —", label, "\n")
  cat(strrep("─", w), "\n", sep = "")
  cat(sprintf(
    "  Step 1  Exclude K where any class < %.0f%% of sample  (see 'Step 1' column)\n"
    , min_class_prop * 100))
  cat(  "  Step 2  Select lowest BIC among eligible K            (dBIC = 0.0 marks the minimum)\n")
  if (bic_tol > 0)
    cat(sprintf(
      "  Step 3  AIC tiebreaker when two or more K within dBIC <= %.1f\n", bic_tol))
  else
    cat("  Step 3  AIC tiebreaker when two or more K share the minimum BIC exactly\n")
  cat(sprintf(
    "  Quality Entropy > %.2f considered acceptable           (Sorgente et al., 2025)\n"
    , entropy_min))
  cat("  Support BLRT p-value (* = p < .05, supports adding a class)\n")
  cat(strrep("─", w), "\n", sep = "")
  print(out, row.names = FALSE)
  cat(strrep("─", w), "\n", sep = "")
  cat(sprintf("  ► Selected K = %d", selected_k))
  if (aic_used) cat("  [AIC tiebreaker applied]")
  cat("\n", strrep("═", w), "\n", sep = "")
  
  list(K = selected_k, decision_log = tbl)
}

# ── Bootstrap LRT ─────────────────────────────────────────────────────────────
run_bootstrap_lrt <- function(lca_data, formula, k_range, nrep = 5,
                              nrep_boot = 3, B = 100, alpha = 0.05,
                              seed = 123, label = "") {
  k_pairs  <- data.frame(K0 = k_range[-length(k_range)], K1 = k_range[-1])
  results  <- vector("list", nrow(k_pairs))
  stop_at  <- NA_integer_
  for (i in seq_len(nrow(k_pairs))) {
    k0 <- k_pairs$K0[i]; k1 <- k_pairs$K1[i]
    if (!is.na(stop_at) && i > stop_at) {
      results[[i]] <- data.frame(K0 = k0, K1 = k1, LR_obs = NA_real_,
                                 B_valid = NA_integer_, p_boot = NA_real_,
                                 sig = "stopped")
      next
    }
    cat(sprintf("\n── BLRT %s: K = %d vs K = %d  (B = %d) ──\n",
                label, k0, k1, B))
    set.seed(seed + i)
    mod0 <- tryCatch(poLCA(formula, data = lca_data, nclass = k0,
                           nrep = nrep, verbose = FALSE, graphs = FALSE),
                     error = function(e) NULL)
    mod1 <- tryCatch(poLCA(formula, data = lca_data, nclass = k1,
                           nrep = nrep, verbose = FALSE, graphs = FALSE),
                     error = function(e) NULL)
    if (is.null(mod0) || is.null(mod1)) {
      results[[i]] <- data.frame(K0 = k0, K1 = k1, LR_obs = NA_real_,
                                 B_valid = 0L, p_boot = NA_real_, sig = "")
      next
    }
    lr_obs  <- -2 * (mod0$llik - mod1$llik)
    n       <- nrow(lca_data); vars <- names(lca_data)
    boot_lr <- numeric(B)
    for (b in seq_len(B)) {
      set.seed(seed + i * 1000L + b)
      boot_class <- sample(seq_len(k0), n, replace = TRUE, prob = mod0$P)
      boot_df    <- lca_data
      for (v in vars) {
        prob_mat    <- mod0$probs[[v]]
        boot_df[[v]] <- as.integer(vapply(boot_class, function(cl)
          sample(seq_len(ncol(prob_mat)), 1L, prob = prob_mat[cl, ]),
          integer(1L)))
      }
      tryCatch({
        b0 <- poLCA(formula, data = boot_df, nclass = k0,
                    nrep = nrep_boot, verbose = FALSE, graphs = FALSE)
        b1 <- poLCA(formula, data = boot_df, nclass = k1,
                    nrep = nrep_boot, verbose = FALSE, graphs = FALSE)
        boot_lr[b] <- -2 * (b0$llik - b1$llik)
      }, error = function(e) { boot_lr[b] <<- NA_real_ })
      if (b %% 20 == 0) cat("  b =", b, "/", B, "\r")
    }
    boot_lr_valid <- boot_lr[!is.na(boot_lr)]
    p_boot  <- if (length(boot_lr_valid) > 0) mean(boot_lr_valid >= lr_obs) else NA_real_
    is_sig  <- !is.na(p_boot) && p_boot < alpha
    cat(sprintf("  LR_obs = %.2f | B_valid = %d | p_boot = %s %s\n",
                lr_obs, length(boot_lr_valid),
                if (is.na(p_boot)) "NA" else sprintf("%.3f", p_boot),
                if (is_sig) " *" else ""))
    results[[i]] <- data.frame(K0 = k0, K1 = k1, LR_obs = round(lr_obs, 2),
                               B_valid = length(boot_lr_valid),
                               p_boot = if (!is.na(p_boot)) round(p_boot, 3) else NA_real_,
                               sig = if (is_sig) "*" else "")
    if (!is_sig && is.na(stop_at)) stop_at <- i
  }
  blrt_tbl <- bind_rows(results)
  cat("\n═══ BLRT summary —", label, "═══\n"); print(blrt_tbl, row.names = FALSE)
  blrt_tbl
}

# ── ML replication check ──────────────────────────────────────────────────────
# Runs the model n_reps times from independent random starts (nrep = 1 each).
# Reports how many runs replicated the maximum log-likelihood within tol_pct.
# Solution is considered STABLE only when:
#   (1) all runs land within the tolerance band (range criterion), AND
#   (2) at least min_replications runs matched the maximum likelihood.
check_ml_replication <- function(lca_data, formula, K, n_reps = 20,
                                 tol_pct = 0.005, min_replications = 20,
                                 seed = 77777, label = "") {
  cat("\n── ML replication check:", label, "(K =", K, "|", n_reps, "runs) ──\n")
  
  logliks <- vapply(seq_len(n_reps), function(r) {
    set.seed(seed + r * 31L)
    tryCatch(poLCA(formula, data = lca_data, nclass = K,
                   nrep = 1, verbose = FALSE, graphs = FALSE)$llik,
             error = function(e) NA_real_)
  }, numeric(1))
  
  ll_clean  <- logliks[!is.na(logliks)]
  n_valid   <- length(ll_clean)
  max_ll    <- if (n_valid > 0) max(ll_clean) else NA_real_
  range_ll  <- if (n_valid > 1) max_ll - min(ll_clean) else 0
  tol_abs   <- tol_pct * abs(max_ll)
  
  # Count runs that replicated the maximum likelihood within tolerance
  n_replicated <- if (!is.na(max_ll)) sum(abs(ll_clean - max_ll) <= tol_abs) else 0L
  
  range_ok  <- !is.na(max_ll) && range_ll <= tol_abs
  reps_ok   <- n_replicated >= min_replications
  converged <- range_ok && reps_ok
  
  cat(sprintf("  Max LL = %.2f | Range = %.4f | Tolerance = %.4f\n",
              max_ll, range_ll, tol_abs))
  cat(sprintf("  Replicated max LL: %d / %d runs (minimum required: %d)\n",
              n_replicated, n_valid, min_replications))
  cat("  Replication:", if (converged) "STABLE ✓" else "UNSTABLE — increase nrep", "\n")
  
  if (!range_ok)
    cat("  ✗ Range across runs exceeds tolerance band.\n")
  if (!reps_ok)
    cat(sprintf("  ✗ Fewer than %d runs replicated the maximum likelihood.\n",
                min_replications))
  
  invisible(list(K            = K,
                 max_ll       = max_ll,
                 range_ll     = range_ll,
                 tol_abs      = tol_abs,
                 n_replicated = n_replicated,
                 n_valid      = n_valid,
                 converged    = converged))
}

# ── Local independence check ──────────────────────────────────────────────────
check_local_independence <- function(df_clean, class_vec, lca_vars, K, label) {
  cat("\n=== Local independence check —", label, "===\n")
  df_clean$LCA_Class_num <- class_vec
  for (k in 1:K) {
    cat("\n-- Class", k, "--\n")
    sub   <- df_clean[df_clean$LCA_Class_num == k, lca_vars]
    pairs <- combn(lca_vars, 2, simplify = FALSE)
    res   <- bind_rows(lapply(pairs, function(p) {
      v <- cramers_v(sub[[p[1]]], sub[[p[2]]])
      data.frame(Var1 = p[1], Var2 = p[2], CramersV = round(v, 3),
                 Flag = if_else(!is.na(v) & v > 0.50, "FLAG > 0.50", "OK"))
    }))
    flagged <- res[res$Flag != "OK", ]
    if (nrow(flagged) == 0) cat("  No pairs with Cramér's V > 0.50\n")
    else { cat("  Flagged pairs:\n"); print(flagged, row.names = FALSE) }
  }
}

# ── Extract conditional probabilities ─────────────────────────────────────────
extract_cond_probs <- function(model, vars, cat_labels, var_labels, K) {
  rows <- list()
  for (v in vars) {
    probs_mat <- model$probs[[v]]
    cats      <- cat_labels[[v]]
    for (j in seq_along(cats)) {
      r        <- as.list(round(probs_mat[, j], 3))
      names(r) <- paste0("Class_", 1:K)
      rows[[length(rows) + 1]] <- c(list(Variable = var_labels[[v]],
                                         Category = cats[[j]]), r)
    }
  }
  bind_rows(rows)
}

# ── Build class information block ─────────────────────────────────────────────
build_class_info <- function(model, class_vec, K) {
  n_total  <- length(class_vec)
  mv_n     <- tabulate(class_vec, nbins = K)
  mv_prop  <- round(mv_n / n_total, 2)
  est_prop <- round(model$P, 2)
  mat      <- rbind(mv_n, est_prop, mv_prop)
  colnames(mat) <- paste0("Class_", 1:K)
  cbind(data.frame(Variable = c("Class population (N)",
                                "Estimated class proportion",
                                "Predicted by modal posterior"),
                   stringsAsFactors = FALSE),
        as.data.frame(mat, stringsAsFactors = FALSE))
}

# ── Select class profiles: healthiest and most multimorbid classes ─────────────
# Replaces find_healthy_ref_class(). Computes two composite scores per class:
#
#   HCS (Healthy Composite Score)     = mean P(healthy pole)     across scored vars
#   MCS (Multimorbid Composite Score) = mean P(multimorbid pole) across scored vars
#
# Demographics (Age, Sex, SEP) are excluded from both scores. Column means are
# used so scores remain on a 0–1 scale and stay comparable across datasets with
# different numbers of indicators.
#
# The class with the highest HCS becomes the regression reference (Section 8).
# The class with the highest MCS is flagged as most multimorbid (reporting only).
#
# Arguments:
#   cond_probs   — output of extract_cond_probs() for one dataset
#   class_names  — character vector of K class names
#   scoring_spec — list of lists, each with:
#                    variable        : matches cond_probs$Variable exactly
#                    healthy_cat     : healthy-pole category label
#                    multimorbid_cat : multimorbid-pole category label
#   label        — dataset identifier for console output
#
# Returns a list (invisible):
#   most_healthy, most_multimorbid  — selected class names
#   hcs, mcs                        — named numeric vectors of mean scores
#   score_table                     — wide data.frame (one row per class)
#   hcs_matrix, mcs_matrix          — component probability matrices (vars × classes)

select_class_profiles <- function(cond_probs, class_names, scoring_spec,
                                  label = "") {
  
  K          <- length(class_names)
  class_cols <- paste0("Class_", seq_len(K))
  n_vars     <- length(scoring_spec)
  var_names  <- vapply(scoring_spec, `[[`, character(1L), "variable")
  
  # ── Extract component probabilities ──────────────────────────────────────────
  hcs_matrix <- matrix(NA_real_, nrow = n_vars, ncol = K,
                       dimnames = list(var_names, class_names))
  mcs_matrix <- matrix(NA_real_, nrow = n_vars, ncol = K,
                       dimnames = list(var_names, class_names))
  
  for (i in seq_len(n_vars)) {
    spec <- scoring_spec[[i]]
    
    row_h <- dplyr::filter(cond_probs,
                           Variable == spec$variable,
                           Category == spec$healthy_cat)
    if (nrow(row_h) == 0)
      stop("select_class_profiles [", label, "]: healthy category not found — ",
           "Variable = '", spec$variable,
           "', Category = '", spec$healthy_cat, "'.")
    hcs_matrix[i, ] <- as.numeric(row_h[1L, class_cols])
    
    row_m <- dplyr::filter(cond_probs,
                           Variable == spec$variable,
                           Category == spec$multimorbid_cat)
    if (nrow(row_m) == 0)
      stop("select_class_profiles [", label, "]: multimorbid category not found — ",
           "Variable = '", spec$variable,
           "', Category = '", spec$multimorbid_cat, "'.")
    mcs_matrix[i, ] <- as.numeric(row_m[1L, class_cols])
  }
  
  # ── Composite scores (column means across variables) ─────────────────────────
  hcs <- colMeans(hcs_matrix, na.rm = TRUE)
  mcs <- colMeans(mcs_matrix, na.rm = TRUE)
  
  # ── Winning classes ───────────────────────────────────────────────────────────
  most_healthy     <- class_names[which.max(hcs)]
  most_multimorbid <- class_names[which.max(mcs)]
  
  # ── Tie warnings ──────────────────────────────────────────────────────────────
  if (sum(hcs == max(hcs)) > 1L)
    warning("select_class_profiles [", label, "]: tie on HCS between: ",
            paste(class_names[hcs == max(hcs)], collapse = " | "),
            ". Inspect score_table and resolve manually.")
  if (sum(mcs == max(mcs)) > 1L)
    warning("select_class_profiles [", label, "]: tie on MCS between: ",
            paste(class_names[mcs == max(mcs)], collapse = " | "),
            ". Inspect score_table and resolve manually.")
  
  # ── Console report ────────────────────────────────────────────────────────────
  # Classes as rows; variables as columns. Class names truncated to nm_w chars.
  nm_w    <- 35L
  val_w   <- 11L
  total_w <- nm_w + val_w * (n_vars + 1L) + 2L
  
  header_vars <- vapply(
    var_names,
    function(v) formatC(substr(v, 1L, val_w - 2L), width = val_w, flag = " "),
    character(1L)
  )
  
  print_score_block <- function(mat, scores, winner, score_label) {
    cat("\n ", score_label, "\n\n")
    cat(formatC("Class", width = nm_w, flag = "-"),
        paste(header_vars, collapse = ""),
        formatC(score_label, width = val_w, flag = " "), "\n")
    cat(" ", strrep("-", total_w), "\n", sep = "")
    for (k in seq_len(K)) {
      tag <- if (class_names[k] == winner) "  \u2190" else ""
      cat(formatC(substr(class_names[k], 1L, nm_w), width = nm_w, flag = "-"),
          paste(formatC(round(mat[, k], 3L), format = "f", digits = 3L,
                        width = val_w), collapse = ""),
          formatC(round(scores[k], 3L), format = "f", digits = 3L,
                  width = val_w),
          tag, "\n", sep = "")
    }
    cat("\n  \u2192 ", winner,
        sprintf(" (%s = %.3f)\n", score_label, max(scores)), sep = "")
  }
  
  cat("\n", strrep("\u2550", total_w), "\n", sep = "")
  cat(" Class profile selection \u2014 ", label, "\n", sep = "")
  cat(" Scored variables: ", paste(var_names, collapse = ", "), "\n", sep = "")
  
  print_score_block(hcs_matrix, hcs, most_healthy,     "HCS")
  print_score_block(mcs_matrix, mcs, most_multimorbid, "MCS")
  
  cat(strrep("\u2550", total_w), "\n")
  
  # ── Exportable score table (wide: one row per class) ─────────────────────────
  score_table <- data.frame(Class = class_names, stringsAsFactors = FALSE)
  for (i in seq_len(n_vars))
    score_table[[paste0("HCS_", var_names[i])]] <- round(hcs_matrix[i, ], 3L)
  score_table[["HCS"]] <- round(hcs, 3L)
  for (i in seq_len(n_vars))
    score_table[[paste0("MCS_", var_names[i])]] <- round(mcs_matrix[i, ], 3L)
  score_table[["MCS"]] <- round(mcs, 3L)
  
  # ── Return ────────────────────────────────────────────────────────────────────
  invisible(list(
    most_healthy     = most_healthy,
    most_multimorbid = most_multimorbid,
    hcs              = setNames(round(hcs, 3L), class_names),
    mcs              = setNames(round(mcs, 3L), class_names),
    score_table      = score_table,
    hcs_matrix       = round(hcs_matrix, 3L),
    mcs_matrix       = round(mcs_matrix, 3L),
    label            = label
  ))
}

# ── LCA profile plot ───────────────────────────────────────────────────────────
make_profile_plot <- function(cond_probs, class_info, class_names, K, var_labels) {
  
  vars_ordered <- unique(cond_probs$Variable)
  
  x_order <- cond_probs |>
    mutate(Variable = factor(Variable, levels = vars_ordered)) |>
    arrange(Variable) |>
    mutate(x_pos = row_number())
  
  cond_probs_plot <- cond_probs |>
    left_join(x_order |> select(Variable, Category, x_pos),
              by = c("Variable", "Category"))
  
  # Per-variable x-bounds: used for background shading and separator lines
  group_bounds <- x_order |>
    group_by(Variable) |>
    summarise(x_min = min(x_pos) - 0.5,
              x_max = max(x_pos) + 0.5,
              x_mid = mean(x_pos),
              .groups = "drop") |>
    mutate(shade = row_number() %% 2 == 0)   # alternating: even-numbered groups shaded
  
  class_cols <- grep("^Class_", names(cond_probs_plot), value = TRUE)
  n_classes  <- length(class_cols)
  
  # Legend: class names only — n is available in the Excel class table
  legend_labels <- class_names
  
  pal           <- c("#B85A4A", "#1D4F4C", "#9ED6C8", "#E3C06A")
  class_colours <- setNames(pal[seq_len(n_classes)], class_names)
  line_types    <- setNames(c("solid", "dashed", "dotdash", "longdash",
                              "twodash", "dotted")[seq_len(n_classes)], class_names)
  point_shapes  <- setNames(c(16L, 17L, 15L, 18L, 1L, 2L)[seq_len(n_classes)],
                            class_names)
  
  plot_long <- bind_rows(lapply(class_cols, function(cc) {
    idx <- as.integer(sub("^Class_", "", cc))
    cond_probs_plot |>
      select(Variable, Category, x_pos, prob = !!sym(cc)) |>
      mutate(class_name = class_names[idx])
  }))
  plot_long$class_name <- factor(plot_long$class_name, levels = class_names)
  
  # ── Build plot — background layers first ─────────────────────────────────────
  p <- ggplot(plot_long, aes(x = x_pos, y = prob,
                             colour   = class_name, shape    = class_name,
                             linetype = class_name, group    = class_name))
  
  # Alternating light-grey background per variable group (even-numbered groups)
  shaded <- group_bounds[group_bounds$shade, ]
  for (i in seq_len(nrow(shaded))) {
    p <- p + annotate("rect",
                      xmin  = shaded$x_min[i], xmax = shaded$x_max[i],
                      ymin  = 0,               ymax = Inf,
                      fill  = "#F2EAD8",       alpha = 1)
  }
  
  # Horizontal reference line at p = 0.50 (chance level)
  p <- p + geom_hline(yintercept = 0.50, colour = "grey60",
                      linetype = "dotted", linewidth = 0.6)
  
  # Solid vertical separators between variable groups — prominent dark line
  sep_x <- group_bounds$x_max[-nrow(group_bounds)]
  for (sep in sep_x)
    p <- p + geom_vline(xintercept = sep, colour = "grey20",
                        linetype = "solid", linewidth = 1.0)
  
  # Class lines and points
  p <- p +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.5)
  
  # Variable name labels at top of each group
  p <- p +
    annotate("text",
             x        = group_bounds$x_mid,
             y        = 1.09,
             label    = group_bounds$Variable,
             hjust    = 0.5, vjust = 0,
             size     = 3.2, fontface = "bold", colour = "grey20") +
    scale_x_continuous(
      breaks = x_order$x_pos,
      labels = x_order$Category,
      expand = expansion(add = 0.5)
    ) +
    scale_y_continuous(
      limits = c(0, 1.15),
      breaks = seq(0, 1, by = 0.1),
      expand = expansion(mult = c(0.01, 0))
    ) +
    scale_colour_manual(name = NULL, values = class_colours,
                        labels = legend_labels) +
    scale_linetype_manual(name = NULL, values = line_types,
                          labels = legend_labels) +
    scale_shape_manual(name = NULL, values = point_shapes,
                       labels = legend_labels) +
    labs(
      x       = NULL,
      y       = "Item-Response Probability") +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 8.5),
      axis.title.y       = element_text(size = 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.border       = element_rect(colour = "grey70"),
      legend.position    = "bottom",
      legend.text        = element_text(size = 9.5),
      legend.key.width   = unit(1.8, "cm"),
      plot.caption       = element_text(size = 8, colour = "grey50", hjust = 0),
      plot.margin        = margin(t = 20, r = 10, b = 10, l = 10)
    )
  p
}

# ── Elbow plot ────────────────────────────────────────────────────────────────
make_elbow_plot <- function(fit_table, selected_k,
                            bic_col     = "BIC",
                            aic_col     = "AIC",
                            entropy_col = "Entropy",
                            entropy_min = 0.60,
                            blrt_table  = NULL,
                            title       = "") {
  col_sel  <- "#2166ac"
  col_line <- "#333333"
  col_elb  <- "#d73027"
  
  theme_e <- theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          axis.title       = element_text(size = 10),
          plot.title       = element_text(size = 11, face = "bold"),
          legend.position  = "none")
  
  p_bic <- ggplot(fit_table, aes(x = K, y = .data[[bic_col]])) +
    geom_line(colour = col_line, linewidth = 0.8) +
    geom_point(colour = col_line, size = 2) +
    geom_vline(xintercept = selected_k, colour = col_sel,
               linetype = "dashed", linewidth = 0.7) +
    scale_x_continuous(breaks = fit_table$K) +
    labs(y = "BIC", x = "K") + theme_e
  
  p_aic <- ggplot(fit_table, aes(x = K, y = .data[[aic_col]])) +
    geom_line(colour = col_line, linewidth = 0.8) +
    geom_point(colour = col_line, size = 2) +
    geom_vline(xintercept = selected_k, colour = col_sel,
               linetype = "dashed", linewidth = 0.7) +
    scale_x_continuous(breaks = fit_table$K) +
    labs(y = "AIC", x = "K") + theme_e
  
  p_ent <- ggplot(fit_table, aes(x = K, y = .data[[entropy_col]])) +
    geom_line(colour = col_line, linewidth = 0.8) +
    geom_point(colour = col_line, size = 2) +
    geom_hline(yintercept = entropy_min, colour = col_elb, linetype = "dotted") +
    geom_vline(xintercept = selected_k, colour = col_sel,
               linetype = "dashed", linewidth = 0.7) +
    scale_x_continuous(breaks = fit_table$K) +
    labs(y = "Entropy", x = "K") + theme_e
  
  assembled <- if (!is.null(blrt_table) && nrow(blrt_table) > 0) {
    blrt_plot <- blrt_table |>
      mutate(K = K1, sig_fill = !is.na(p_boot) & p_boot < .05)
    p_blrt <- ggplot(blrt_plot, aes(x = K, y = p_boot)) +
      geom_hline(yintercept = 0.05, colour = col_elb, linetype = "dashed") +
      geom_line(colour = col_line, linewidth = 0.8, na.rm = TRUE) +
      geom_point(aes(fill = sig_fill), shape = 21, size = 2.5,
                 colour = col_line, na.rm = TRUE) +
      scale_fill_manual(values = c("TRUE" = col_elb, "FALSE" = "white"),
                        guide = "none") +
      geom_vline(xintercept = selected_k, colour = col_sel,
                 linetype = "dashed", linewidth = 0.7) +
      scale_x_continuous(breaks = fit_table$K) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(y = "Bootstrap p", x = "K") + theme_e
    (p_bic | p_aic | p_ent | p_blrt) +
      plot_annotation(title = title,
                      subtitle = paste0("Blue dashed = selected K. ",
                                        "Dotted red = entropy threshold 0.60. ",
                                        "BLRT: filled = p < .05."),
                      theme = theme(plot.title    = element_text(size = 13, face = "bold"),
                                    plot.subtitle = element_text(size = 9, colour = "#595959")))
  } else {
    (p_bic | p_aic | p_ent) +
      plot_annotation(title = title,
                      subtitle = "Blue dashed = selected K. Dotted red = entropy threshold 0.60.",
                      theme = theme(plot.title    = element_text(size = 13, face = "bold"),
                                    plot.subtitle = element_text(size = 9, colour = "#595959")))
  }
  assembled
}

# ── Export fit index table ─────────────────────────────────────────────────────
export_fit_table <- function(fit_table, sample_label, path) {
  wb <- createWorkbook(); ws <- paste0("Fit indices — ", sample_label)
  addWorksheet(wb, ws)
  sty_h <- createStyle(fontName = "Calibri", fontSize = 11,
                       textDecoration = "bold",
                       border = c("top", "bottom"), borderStyle = "thin",
                       halign = "center")
  sty_d <- createStyle(fontName = "Calibri", fontSize = 11, halign = "center")
  sty_l <- createStyle(fontName = "Calibri", fontSize = 11, halign = "center",
                       border = "bottom", borderStyle = "thin")
  writeData(wb, ws, as.data.frame(t(names(fit_table))),
            startRow = 1, colNames = FALSE)
  addStyle(wb, ws, sty_h, rows = 1, cols = seq_along(fit_table), gridExpand = TRUE)
  for (i in seq_len(nrow(fit_table))) {
    sty <- if (i == nrow(fit_table)) sty_l else sty_d
    writeData(wb, ws,
              as.data.frame(t(as.character(unlist(fit_table[i, ])))),
              startRow = i + 1, colNames = FALSE)
    addStyle(wb, ws, sty, rows = i + 1, cols = seq_along(fit_table),
             gridExpand = TRUE)
  }
  setColWidths(wb, ws, cols = seq_along(fit_table), widths = 18)
  saveWorkbook(wb, path, overwrite = TRUE)
  cat("Fit index table exported:", path, "\n")
}

# ── Export LCA class probability table ────────────────────────────────────────
export_lca_table <- function(class_info, cond_probs, class_names, K,
                             sample_label, path) {
  wb <- createWorkbook(); ws <- paste0("LCA — ", sample_label)
  addWorksheet(wb, ws)
  sty_h  <- createStyle(fontName = "Calibri", fontSize = 11,
                        textDecoration = "bold",
                        border = c("top", "bottom"), borderStyle = "thin",
                        halign = "center")
  sty_s  <- createStyle(fontName = "Calibri", fontSize = 11,
                        textDecoration = "bold")
  sty_dl <- createStyle(fontName = "Calibri", fontSize = 11, indent = 1)
  sty_dc <- createStyle(fontName = "Calibri", fontSize = 11, halign = "center")
  sty_ll <- createStyle(fontName = "Calibri", fontSize = 11, indent = 1,
                        border = "bottom", borderStyle = "thin")
  sty_lc <- createStyle(fontName = "Calibri", fontSize = 11, halign = "center",
                        border = "bottom", borderStyle = "thin")
  cr <- 1
  write_row <- function(vals, sl, sr = sl, is_last = FALSE) {
    sl2 <- if (is_last && identical(sl, sty_dl)) sty_ll else sl
    sr2 <- if (is_last && identical(sr, sty_dc)) sty_lc else sr
    writeData(wb, ws, as.data.frame(t(as.character(vals))),
              startRow = cr, startCol = 1, colNames = FALSE)
    addStyle(wb, ws, sl2, rows = cr, cols = 1)
    if (K > 0) addStyle(wb, ws, sr2, rows = cr, cols = 2:(K + 1), gridExpand = TRUE)
    cr <<- cr + 1
  }
  write_row(c("Variable", class_names), sty_h)
  write_row(c("Class Information", rep("", K)), sty_s)
  for (i in seq_len(nrow(class_info))) {
    vals_r <- if (i == 1)
      as.character(as.integer(unlist(class_info[i, paste0("Class_", 1:K)])))
    else sprintf("%.2f", as.numeric(unlist(class_info[i, paste0("Class_", 1:K)])))
    write_row(c(class_info$Variable[i], vals_r), sty_dl, sty_dc,
              is_last = i == nrow(class_info))
  }
  current_var <- ""
  all_rows    <- seq_len(nrow(cond_probs))
  for (i in all_rows) {
    vname <- cond_probs$Variable[i]
    if (vname != current_var) {
      write_row(c(vname, rep("", K)), sty_s)
      current_var <- vname
    }
    prob_vals <- sprintf("%.3f",
                         as.numeric(unlist(cond_probs[i, paste0("Class_", 1:K)])))
    write_row(c(paste0("  ", cond_probs$Category[i]), prob_vals),
              sty_dl, sty_dc, is_last = i == max(all_rows))
  }
  setColWidths(wb, ws, cols = 1, widths = 40)
  setColWidths(wb, ws, cols = 2:(K + 1), widths = 14)
  saveWorkbook(wb, path, overwrite = TRUE)
  cat("LCA class table exported:", path, "\n")
}

# ── OLS helper functions ───────────────────────────────────────────────────────
extract_ols_stats <- function(model, dep_var, label, class_col,
                              class_names = NULL) {
  s      <- summary(model); n_obs <- nobs(model)
  r2     <- s$r.squared; adj_r2 <- s$adj.r.squared
  f_val  <- s$fstatistic["value"]; df_mod <- s$fstatistic["numdf"]
  df_res <- s$fstatistic["dendf"]
  p_f    <- pf(f_val, df_mod, df_res, lower.tail = FALSE)
  sigma  <- s$sigma; aic_val <- AIC(model); bic_val <- BIC(model)
  rmse_val <- sqrt(mean(residuals(model)^2)); f2_val <- r2 / (1 - r2)
  
  coef_df <- broom::tidy(model, conf.int = TRUE, conf.level = 0.95) |>
    dplyr::rename(B = estimate, SE = std.error, t = statistic,
                  p = p.value, CI_low = conf.low, CI_high = conf.high) |>
    dplyr::mutate(stars = dplyr::case_when(
      p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "†", TRUE ~ ""))
  
  std_p <- tryCatch({
    effectsize::standardize_parameters(model, method = "refit") |>
      dplyr::select(Parameter, Std_Coefficient) |>
      dplyr::rename(term = Parameter, std_beta = Std_Coefficient)
  }, error = function(e)
    data.frame(term = coef_df$term, std_beta = NA_real_, stringsAsFactors = FALSE))
  
  coef_df <- dplyr::left_join(coef_df, std_p, by = "term") |>
    dplyr::mutate(cohens_d = B / sigma)
  
  cls_levels <- levels(model$model[[class_col]]); ref_class <- cls_levels[1]
  
  # Build term_clean: intercept gets the reference class name; non-intercept
  # terms are labelled "Class X — [name]" where X is the position in
  # class_names (original ordering before releveling). If class_names is not
  # supplied, falls back to stripping the class_col prefix only.
  coef_df <- coef_df |>
    dplyr::mutate(
      cls_name_raw = dplyr::if_else(
        term == "(Intercept)",
        NA_character_,
        sub(paste0("^", class_col), "", term)
      ),
      cls_num = if (!is.null(class_names))
        match(cls_name_raw, class_names)
      else
        NA_integer_,
      term_clean = dplyr::case_when(
        term == "(Intercept)"  ~ paste0("Intercept = ", ref_class),
        !is.na(cls_num)        ~ paste0("Class ", cls_num, "  ", cls_name_raw),
        TRUE                   ~ cls_name_raw
      )
    ) |>
    dplyr::select(-cls_name_raw, -cls_num) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 4)))
  
  list(label = label, dep_var = dep_var, class_col = class_col,
       n = n_obs, ref_class = ref_class,
       r2 = r2, adj_r2 = adj_r2, f_stat = as.numeric(f_val),
       df_mod = as.integer(df_mod), df_res = as.integer(df_res),
       p_f = as.numeric(p_f), sigma = sigma,
       aic = aic_val, bic = bic_val, rmse = rmse_val, cohens_f2 = f2_val,
       coef_table = coef_df)
}

print_ols_table <- function(ols) {
  cat("\n", strrep("═", 60), "\n")
  cat(" Model:", ols$label, "| Outcome:", ols$dep_var, "| n =", ols$n, "\n")
  cat(strrep("─", 60), "\n")
  cat(sprintf(" R² = %.3f  Adj R² = %.3f  F(%d,%d) = %.3f  p = %.4f\n",
              ols$r2, ols$adj_r2, ols$df_mod, ols$df_res, ols$f_stat, ols$p_f))
  cat(sprintf(" σ = %.3f  AIC = %.1f  BIC = %.1f  f² = %.3f\n",
              ols$sigma, ols$aic, ols$bic, ols$cohens_f2))
  cat(strrep("─", 60), "\n")
  ct <- ols$coef_table
  cat(sprintf(" %-48s %8s %8s %8s %6s  %5s  %8s  %8s\n",
              "Term", "B", "SE", "t", "p", "", "95% CI lo", "95% CI hi"))
  for (i in seq_len(nrow(ct)))
    cat(sprintf(" %-48s %8.4f %8.4f %8.4f %6.4f %5s  %8.4f  %8.4f\n",
                ct$term_clean[i], ct$B[i], ct$SE[i], ct$t[i],
                ct$p[i], ct$stars[i], ct$CI_low[i], ct$CI_high[i]))
  cat(strrep("═", 60), "\n")
}

export_ols_workbook <- function(ols_list, path, analysis_label, class_term,
                                class_names_map = NULL) {
  wb <- createWorkbook()
  sty_head <- createStyle(fontName = "Calibri", fontSize = 11,
                          textDecoration = "bold",
                          border = c("top", "bottom"), borderStyle = "thin",
                          halign = "center")
  sty_left <- createStyle(fontName = "Calibri", fontSize = 11)
  sty_data <- createStyle(fontName = "Calibri", fontSize = 11, halign = "center")
  sty_last <- createStyle(fontName = "Calibri", fontSize = 11, halign = "center",
                          border = "bottom", borderStyle = "thin")
  sty_note <- createStyle(fontName = "Calibri", fontSize = 10, wrapText = TRUE)
  sty_leg_head <- createStyle(fontName = "Calibri", fontSize = 10,
                              textDecoration = "bold", wrapText = TRUE)
  
  for (nm in names(ols_list)) {
    ols <- ols_list[[nm]]
    addWorksheet(wb, nm)
    headers <- c("Term", "B", "SE", "t", "p", "Sig", "CI 95% lo", "CI 95% hi",
                 "Std Beta", "Cohen's d")
    writeData(wb, nm, as.data.frame(t(headers)), startRow = 1, colNames = FALSE)
    addStyle(wb, nm, sty_head, rows = 1, cols = seq_along(headers), gridExpand = TRUE)
    ct <- ols$coef_table
    for (i in seq_len(nrow(ct))) {
      r    <- i + 1
      vals <- c(ct$term_clean[i], ct$B[i], ct$SE[i], ct$t[i], ct$p[i],
                ct$stars[i], ct$CI_low[i], ct$CI_high[i],
                if ("std_beta" %in% names(ct)) ct$std_beta[i] else NA,
                if ("cohens_d" %in% names(ct)) ct$cohens_d[i] else NA)
      writeData(wb, nm, as.data.frame(t(as.character(vals))),
                startRow = r, colNames = FALSE)
      sty <- if (i == nrow(ct)) sty_last else sty_data
      addStyle(wb, nm, sty_left, rows = r, cols = 1)
      addStyle(wb, nm, sty, rows = r, cols = 2:length(headers), gridExpand = TRUE)
    }
    
    # ── Fit statistics note ───────────────────────────────────────────────────
    fit_row <- nrow(ct) + 3L
    fit_txt <- sprintf(
      "Fit: R² = %.3f  Adj R² = %.3f  F(%d,%d) = %.3f  p = %.4f  σ = %.3f  f² = %.3f",
      ols$r2, ols$adj_r2, ols$df_mod, ols$df_res, ols$f_stat, ols$p_f,
      ols$sigma, ols$cohens_f2)
    writeData(wb, nm, fit_txt, startRow = fit_row, startCol = 1)
    addStyle(wb, nm, sty_note, rows = fit_row, cols = 1:length(headers),
             gridExpand = TRUE)
    
    # ── Class legend note ─────────────────────────────────────────────────────
    # Lookup class names for this model's sample (DA or L)
    sample_key  <- sub("_.*", "", ols$label)
    cnames      <- if (!is.null(class_names_map) &&
                       sample_key %in% names(class_names_map))
      class_names_map[[sample_key]] else NULL
    
    if (!is.null(cnames)) {
      leg_row <- fit_row + 2L
      
      # Header row
      writeData(wb, nm, "Class legend:", startRow = leg_row, startCol = 1)
      addStyle(wb, nm, sty_leg_head, rows = leg_row,
               cols = 1:length(headers), gridExpand = TRUE)
      
      # One row per class, marking the reference
      for (k in seq_along(cnames)) {
        leg_row <- leg_row + 1L
        is_ref  <- cnames[k] == ols$ref_class
        leg_txt <- sprintf("  Class %d = %s%s",
                           k, cnames[k],
                           if (is_ref) "  [REFERENCE — intercept = this class mean]" else "")
        writeData(wb, nm, leg_txt, startRow = leg_row, startCol = 1)
        addStyle(wb, nm, sty_note, rows = leg_row,
                 cols = 1:length(headers), gridExpand = TRUE)
      }
      
      # Interpretation note
      leg_row <- leg_row + 1L
      writeData(wb, nm,
                paste0("Each B = mean difference between the named class and the ",
                       "reference class. A positive B means the named class scores ",
                       "higher on the outcome than the reference class."),
                startRow = leg_row, startCol = 1)
      addStyle(wb, nm, sty_note, rows = leg_row,
               cols = 1:length(headers), gridExpand = TRUE)
    }
    
    setColWidths(wb, nm, cols = 1, widths = 52)
    setColWidths(wb, nm, cols = 2:length(headers), widths = 12)
  }
  
  # Model fit summary sheet
  addWorksheet(wb, "Model fit")
  fit_df <- dplyr::bind_rows(lapply(ols_list, function(s) {
    data.frame(
      Model = s$label, Dataset = sub("_.*", "", s$label), Outcome = s$dep_var,
      n = s$n, R2 = round(s$r2, 3), Adj_R2 = round(s$adj_r2, 3),
      F_stat = round(s$f_stat, 3), df1 = s$df_mod, df2 = s$df_res,
      p_F    = if (s$p_f < .001) "< .001" else sprintf("%.4f", s$p_f),
      AIC    = round(s$aic, 1), BIC = round(s$bic, 1),
      Sigma  = round(s$sigma, 3), RMSE = round(s$rmse, 3),
      Cohens_f2 = round(s$cohens_f2, 3), stringsAsFactors = FALSE
    )
  }))
  n_fc <- length(names(fit_df))
  writeData(wb, "Model fit", as.data.frame(t(names(fit_df))),
            startRow = 1, colNames = FALSE)
  addStyle(wb, "Model fit", sty_head, rows = 1, cols = 1:n_fc, gridExpand = TRUE)
  n_fit <- nrow(fit_df)
  writeData(wb, "Model fit", fit_df, startRow = 2, colNames = FALSE)
  addStyle(wb, "Model fit", sty_left, rows = 2:(n_fit + 1), cols = 1,
           gridExpand = TRUE)
  if (n_fit > 1)
    addStyle(wb, "Model fit", sty_data, rows = 2:n_fit, cols = 2:n_fc,
             gridExpand = TRUE)
  addStyle(wb, "Model fit", sty_last, rows = n_fit + 1, cols = 2:n_fc,
           gridExpand = TRUE)
  note_row <- n_fit + 3L
  writeData(wb, "Model fit",
            paste0("Note. ", analysis_label,
                   " Reference class = class with highest Healthy Composite Score",
                   " (HCS: mean P(healthy pole) across disease, risk factor,",
                   " and sample-specific health indicators; demographics excluded)."),
            startRow = note_row, startCol = 1)
  addStyle(wb, "Model fit", sty_note, rows = note_row, cols = 1:n_fc,
           gridExpand = TRUE)
  setColWidths(wb, "Model fit", cols = 1:3, widths = 16)
  setColWidths(wb, "Model fit", cols = 4:n_fc, widths = 12)
  saveWorkbook(wb, path, overwrite = TRUE)
  cat("Regression workbook exported:", path, "\n")
}

make_coef_plot <- function(ols_list, sample, title) {
  entries <- ols_list[grepl(paste0("^", sample, "_"), names(ols_list))]
  if (length(entries) == 0) { warning("No models for sample: ", sample); return(NULL) }
  dep_pretty <- c(IAS_Totaal = "IAS", IATS_Totaal = "IATS")
  plot_df <- dplyr::bind_rows(lapply(entries, function(s) {
    lbl <- if (s$dep_var %in% names(dep_pretty)) dep_pretty[[s$dep_var]] else s$dep_var
    s$coef_table |> dplyr::filter(term != "(Intercept)") |>
      dplyr::mutate(outcome = lbl, ref_class = s$ref_class)
  }))
  if (nrow(plot_df) == 0) { warning("No non-intercept terms for sample: ", sample); return(NULL) }
  ref_label <- unique(plot_df$ref_class)
  ggplot(plot_df, aes(x = term_clean, y = B, colour = outcome, shape = outcome)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40", linewidth = 0.6) +
    geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.18, linewidth = 0.65,
                  position = position_dodge(width = 0.45)) +
    geom_point(size = 3, position = position_dodge(width = 0.45)) +
    scale_colour_manual(name = "Outcome",
                        values = c(IAS = "#1f77b4", IATS = "#d62728")) +
    scale_shape_manual(name = "Outcome",
                       values = c(IAS = 16L, IATS = 17L)) +
    labs(x       = paste0("LCA class  (reference: ", ref_label, ")"),
         y       = "B  (unstandardised regression coefficient)",
         title   = title,
         caption = "Points = B; error bars = 95% CI; dashed line = zero (reference class mean).") +
    theme_bw(base_size = 11) +
    theme(legend.position    = "bottom",
          axis.text.x        = element_text(angle = 22, hjust = 1, size = 9.5),
          plot.title         = element_text(size = 11, face = "bold"),
          plot.caption       = element_text(size = 8, colour = "grey40"),
          panel.grid.major.x = element_blank())
}

cat("\nAll shared helper functions defined.\n")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 7 — Latent Class Analysis
# ══════════════════════════════════════════════════════════════════════════════

# ── 7.1 LCA variable sets (defined in Section 4; listed here for reference) ───
# lca_vars_da and lca_vars_l were set in Section 4.6 and are the single source
# of truth. Any change to those vectors automatically propagates here.
#
# DA: Leeftijd_Cat, Geslacht, SEP_3Cat, Disease_Cat, RiskFactor_Cat,
#     BDI_Cat, TAS_Cat
#  L: Leeftijd_Cat, Geslacht, SEP_3Cat, Disease_Cat, RiskFactor_Cat,
#     IPAQ_Cat, Alcohol_Cat, Roken_Cat, SSRQ_Cat, EEQ_Cat, JSS_Cat

# ── 7.2 Category frequency check ─────────────────────────────────────────────
check_lca_frequencies(df_da_clean, lca_vars_da, "DA")
check_lca_frequencies(df_l_clean,  lca_vars_l,  "L")

# ── 7.3 Cramér's V among LCA indicators ──────────────────────────────────────
# Final check on the clean datasets (post-outlier removal)
cat("\n── Cramér's V — LCA indicators — DA (final clean dataset) ──\n")
cramers_v_matrix(df_da_clean, lca_vars_da, "DA (final)")
cat("\n── Cramér's V — LCA indicators — L (final clean dataset) ──\n")
cramers_v_matrix(df_l_clean,  lca_vars_l,  "L (final)")

# ── 7.4 Prepare integer-coded LCA datasets ───────────────────────────────────
# poLCA requires integer-coded factor variables (1-based).
prep_lca_data <- function(df, vars) {
  df |> select(all_of(vars)) |>
    mutate(across(everything(), as.integer)) |>
    as.data.frame()
}

lca_dat_da <- prep_lca_data(df_da_clean, lca_vars_da)
lca_dat_l  <- prep_lca_data(df_l_clean,  lca_vars_l)

f_da <- make_lca_formula(lca_vars_da)
f_l  <- make_lca_formula(lca_vars_l)

# ── 7.5 Run LCA across K = 1–10 ──────────────────────────────────────────────
# nrep = 10 random starts per K to help avoid local maxima.
# For a quick verification run, reduce to nrep = 1 and k_range = 1:5.
# For the final analysis use nrep = 10–20.
# Computationally intensive — runtime varies by hardware (allow 10–30+ min
# per sample with nrep = 10 on a modern laptop).

lca_da <- run_lca_single(lca_dat_da, f_da, k_range = 1:10, nrep = 10, seed = 123)
lca_l  <- run_lca_single(lca_dat_l,  f_l,  k_range = 1:10, nrep = 10, seed = 456)

cat("\n═══ Fit indices — DA sample ═══\n")
print(lca_da$fit_table, row.names = FALSE)
cat("\n═══ Fit indices — L sample ═══\n")
print(lca_l$fit_table,  row.names = FALSE)

# ── 7.6 Bootstrap LRT ────────────────────────────────────────────────────────
# Supporting evidence for K selection alongside BIC.
# B = 100 recommended; reduce to B = 10 and nrep = 3 for a quick verification
# run. Computationally intensive — runtime varies by hardware (allow 15–40+
# min per sample with B = 100, nrep = 5 on a modern laptop).

cat("\nRunning BLRT — DA sample ...\n")
blrt_da <- run_bootstrap_lrt(lca_dat_da, f_da, k_range = 1:10,
                             nrep = 5, nrep_boot = 3, B = 100,
                             seed = 123, label = "DA")

cat("\nRunning BLRT — L sample ...\n")
blrt_l  <- run_bootstrap_lrt(lca_dat_l,  f_l,  k_range = 1:10,
                             nrep = 5, nrep_boot = 3, B = 100,
                             seed = 456, label = "L")

# ── 7.7 Select best K ─────────────────────────────────────────────────────────
# Sequential procedure: (1) exclude K with any class < 10%; (2) lowest BIC;
# (3) AIC tiebreaker for tied BIC. Entropy > .60 as quality indicator
# (Sorgente et al., 2025). BLRT as supporting evidence only.

sel_da <- select_best_k(lca_da$fit_table,
                        bic_col        = "BIC",
                        aic_col        = "AIC",
                        entropy_col    = "Entropy",
                        class_prop_col = "Min_class_prop",
                        entropy_min    = 0.60,
                        min_class_prop = 0.10,
                        blrt_table     = blrt_da,
                        label          = "DA")

sel_l  <- select_best_k(lca_l$fit_table,
                        bic_col        = "BIC",
                        aic_col        = "AIC",
                        entropy_col    = "Entropy",
                        class_prop_col = "Min_class_prop",
                        entropy_min    = 0.60,
                        min_class_prop = 0.10,
                        blrt_table     = blrt_l,
                        label          = "L")

best_k_da <- sel_da$K
best_k_l  <- sel_l$K

# ── MANUAL K OVERRIDE ─────────────────────────────────────────────────────────
# The automatic selection procedure (select_best_k) identified K = 4 as
# optimal for the DA sample based on BIC. However, the ML replication check
# in Section 7.10 revealed that the K = 4 solution was unstable: fewer than
# the required number of independent random starts replicated the maximum
# log-likelihood within the tolerance band, indicating that the optimisation
# landscape has multiple local maxima at K = 4 and that the solution cannot
# be trusted without substantially more starting values. K = 3 was therefore
# selected manually as the most stable solution with acceptable fit indices
# and class sizes.
#
# To revert to fully automatic K selection, comment out the line(s) below.
best_k_da <- 3L
# best_k_l <- 3L   # Uncomment and set if a manual override is also needed for L

# ── 7.8 Elbow plots ───────────────────────────────────────────────────────────
p_elbow_da <- make_elbow_plot(
  fit_table  = lca_da$fit_table,
  selected_k = best_k_da,
  blrt_table = blrt_da,
  title      = paste0("LCA fit indices — DA sample  |  Selected K = ", best_k_da)
)

p_elbow_l <- make_elbow_plot(
  fit_table  = lca_l$fit_table,
  selected_k = best_k_l,
  blrt_table = blrt_l,
  title      = paste0("LCA fit indices — L sample  |  Selected K = ", best_k_l)
)

ggsave(file.path(base_path, "LCA_elbowplot_DA.png"),
       plot = p_elbow_da, width = 14, height = 5, dpi = 300)
ggsave(file.path(base_path, "LCA_elbowplot_L.png"),
       plot = p_elbow_l,  width = 14, height = 5, dpi = 300)
cat("Elbow plots saved.\n")

# ── 7.9 Fit best-K model with many random starts ──────────────────────────────
# nrep = 50 for reliable conditional probability estimates
set.seed(999)
model_da <- poLCA(f_da, data = lca_dat_da, nclass = best_k_da,
                  nrep = 50, verbose = FALSE, graphs = FALSE)
set.seed(1001)
model_l  <- poLCA(f_l,  data = lca_dat_l,  nclass = best_k_l,
                  nrep = 50, verbose = FALSE, graphs = FALSE)

class_da <- model_da$predclass
class_l  <- model_l$predclass

cat("\nClass distribution — DA:\n"); print(table(class_da))
cat("\nClass distribution — L:\n");  print(table(class_l))

# ── 7.10 ML replication check ─────────────────────────────────────────────────
ml_rep_da <- check_ml_replication(lca_dat_da, f_da, best_k_da,
                                  n_reps = 20, seed = 11111, label = "DA")
ml_rep_l  <- check_ml_replication(lca_dat_l,  f_l,  best_k_l,
                                  n_reps = 20, seed = 22222, label = "L")

if (!ml_rep_da$converged)
  warning("ML replication check — DA: solution is UNSTABLE. ",
          "Increase nrep in 7.9 before interpreting results.")
if (!ml_rep_l$converged)
  warning("ML replication check — L: solution is UNSTABLE. ",
          "Increase nrep in 7.9 before interpreting results.")

# ── 7.11 Indicator category labels ────────────────────────────────────────────
# Order must match factor level order used in prep_lca_data (as.integer)

category_labels_da <- list(
  Leeftijd_Cat   = c("18 to 44 years", "45 years or older"),
  Geslacht       = c("Male", "Female"),
  SEP_3Cat       = c("Below Average SEP", "Average SEP", "Above Average SEP"),
  Disease_Cat    = c("No chronic conditions", "Single chronic condition",
                     "Multimorbidity"),
  RiskFactor_Cat = c("Absent", "Present"),
  BDI_Cat        = c("Low depressive symptoms",
                     "Clinically relevant depressive symptoms"),
  TAS_Cat        = c("No alexithymia", "Alexithymia present")
)

var_labels_da <- c(
  Leeftijd_Cat   = "Age",
  Geslacht       = "Sex",
  SEP_3Cat       = "Socioeconomic Position",
  Disease_Cat    = "Disease Status",
  RiskFactor_Cat = "Risk Factors",
  BDI_Cat        = "Depression",
  TAS_Cat        = "Alexithymia"
)

category_labels_l <- list(
  Leeftijd_Cat   = c("18 to 44 years", "45 years or older"),
  Geslacht       = c("Male", "Female"),
  SEP_3Cat       = c("Below Average SEP", "Average SEP", "Above Average SEP"),
  Disease_Cat    = c("No chronic conditions", "Single chronic condition",
                     "Multimorbidity"),
  RiskFactor_Cat = c("Absent", "Present"),
  IPAQ_Cat       = c("Insufficiently active", "Active"),
  Alcohol_Cat    = c("No likelihood", "Likelihood present"),
  Roken_Cat      = c("Non-smoker", "Smoker"),
  SSRQ_Cat       = c("Below average self-regulation",
                     "Above average self-regulation"),
  EEQ_Cat        = c("Non-Emotional Eater", "Low Emotional Eater",
                     "Emotional to Very Emotional Eater"),
  JSS_Cat        = c("No disturbed sleep", "Disturbed sleep")
)

var_labels_l <- c(
  Leeftijd_Cat   = "Age",
  Geslacht       = "Sex",
  SEP_3Cat       = "Socioeconomic Position",
  Disease_Cat    = "Disease Status",
  RiskFactor_Cat = "Risk Factors",
  IPAQ_Cat       = "Physical Activity",
  Alcohol_Cat    = "Alcohol Use",
  Roken_Cat      = "Smoking",
  SSRQ_Cat       = "Self-Regulation",
  EEQ_Cat        = "Emotional Eating",
  JSS_Cat        = "Sleep"
)

# ── 7.12 Extract conditional probabilities ────────────────────────────────────
cond_probs_da <- extract_cond_probs(model_da, lca_vars_da,
                                    category_labels_da, var_labels_da, best_k_da)
cond_probs_l  <- extract_cond_probs(model_l,  lca_vars_l,
                                    category_labels_l,  var_labels_l,  best_k_l)

# ── 7.13 Class information block ───────────────────────────────────────────────
class_info_da <- build_class_info(model_da, class_da, best_k_da)
class_info_l  <- build_class_info(model_l,  class_l,  best_k_l)

# ── 7.14 Class names ──────────────────────────────────────────────────────────
# Based on inspection of conditional probability tables and profile plots.
# These names propagate to the Excel tables, profile plots, and LCA_Class factor.

class_names_da <- c(
  "Healthy and wealthy males",
  "Aging, multimorbid, with socioeconomic burden",
  "Young, wealthy, and healthy females"
)

class_names_l <- c(
  "Aging, multimorbid females with behavioral dysregulation",
  "Aging, healthy males with high self-regulation",
  "Young, healthy, and wealthy"
)

# ── 7.14a Scoring specifications ──────────────────────────────────────────────
# Variable labels must match cond_probs$Variable exactly (from var_labels_*).
# Category labels must match cond_probs$Category exactly (from category_labels_*).

scoring_spec_da <- list(
  list(variable        = "Disease Status",
       healthy_cat     = "No chronic conditions",
       multimorbid_cat = "Multimorbidity"),
  list(variable        = "Risk Factors",
       healthy_cat     = "Absent",
       multimorbid_cat = "Present"),
  list(variable        = "Depression",
       healthy_cat     = "Low depressive symptoms",
       multimorbid_cat = "Clinically relevant depressive symptoms"),
  list(variable        = "Alexithymia",
       healthy_cat     = "No alexithymia",
       multimorbid_cat = "Alexithymia present")
)

scoring_spec_l <- list(
  list(variable        = "Disease Status",
       healthy_cat     = "No chronic conditions",
       multimorbid_cat = "Multimorbidity"),
  list(variable        = "Risk Factors",
       healthy_cat     = "Absent",
       multimorbid_cat = "Present"),
  list(variable        = "Physical Activity",
       healthy_cat     = "Active",
       multimorbid_cat = "Insufficiently active"),
  list(variable        = "Alcohol Use",
       healthy_cat     = "No likelihood",
       multimorbid_cat = "Likelihood present"),
  list(variable        = "Smoking",
       healthy_cat     = "Non-smoker",
       multimorbid_cat = "Smoker"),
  list(variable        = "Self-Regulation",
       healthy_cat     = "Above average self-regulation",
       multimorbid_cat = "Below average self-regulation"),
  list(variable        = "Emotional Eating",
       healthy_cat     = "Non-Emotional Eater",
       multimorbid_cat = "Emotional to Very Emotional Eater"),
  list(variable        = "Sleep",
       healthy_cat     = "No disturbed sleep",
       multimorbid_cat = "Disturbed sleep")
)

# ── 7.14b Run composite class profile scoring ─────────────────────────────────
# Depends on: cond_probs_da / cond_probs_l (Section 7.12) and
#             class_names_da / class_names_l (Section 7.14).
# Outputs feed into Section 8.1 for regression reference class selection and
# into manuscript reporting for class labelling.

cat("\n\u2550\u2550\u2550 Class profile scoring \u2014 DA \u2550\u2550\u2550\n")
profile_da <- select_class_profiles(cond_probs_da, class_names_da,
                                    scoring_spec_da, label = "DA")

cat("\n\u2550\u2550\u2550 Class profile scoring \u2014 L \u2550\u2550\u2550\n")
profile_l  <- select_class_profiles(cond_probs_l,  class_names_l,
                                    scoring_spec_l,  label = "L")

cat("\nProfile scoring summary:\n")
cat(sprintf("  Most healthy class     \u2014 DA : %s (HCS = %.3f)\n",
            profile_da$most_healthy,     max(profile_da$hcs)))
cat(sprintf("  Most multimorbid class \u2014 DA : %s (MCS = %.3f)\n",
            profile_da$most_multimorbid, max(profile_da$mcs)))
cat(sprintf("  Most healthy class     \u2014 L  : %s (HCS = %.3f)\n",
            profile_l$most_healthy,      max(profile_l$hcs)))
cat(sprintf("  Most multimorbid class \u2014 L  : %s (MCS = %.3f)\n",
            profile_l$most_multimorbid,  max(profile_l$mcs)))

# ── 7.15 Export fit index tables ─────────────────────────────────────────────
export_fit_table(lca_da$fit_table, "DA",
                 file.path(base_path, "LCA_fitindices_DA.xlsx"))
export_fit_table(lca_l$fit_table,  "L",
                 file.path(base_path, "LCA_fitindices_L.xlsx"))

# ── 7.16 Export class probability tables ─────────────────────────────────────
export_lca_table(class_info_da, cond_probs_da, class_names_da, best_k_da,
                 "DA", file.path(base_path, "LCA_classtable_DA.xlsx"))
export_lca_table(class_info_l,  cond_probs_l,  class_names_l,  best_k_l,
                 "L",  file.path(base_path, "LCA_classtable_L.xlsx"))

# ── 7.17 Profile plots ────────────────────────────────────────────────────────
p_profile_da <- make_profile_plot(cond_probs_da, class_info_da,
                                  class_names_da, best_k_da, var_labels_da)
p_profile_l  <- make_profile_plot(cond_probs_l,  class_info_l,
                                  class_names_l,  best_k_l,  var_labels_l)

ggsave(file.path(base_path, "LCA_profileplot_DA.png"), plot = p_profile_da,
       width = max(10, nrow(cond_probs_da) * 0.45 + 7.5), height = 6, dpi = 300)
ggsave(file.path(base_path, "LCA_profileplot_L.png"),  plot = p_profile_l,
       width = max(10, nrow(cond_probs_l)  * 0.45 + 7.5), height = 6, dpi = 300)
cat("Profile plots saved.\n")

# ── 7.18 Attach LCA_Class to clean datasets ───────────────────────────────────
df_da_clean$LCA_Class <- factor(class_da, levels = 1:best_k_da,
                                labels = class_names_da)
df_l_clean$LCA_Class  <- factor(class_l,  levels = 1:best_k_l,
                                labels = class_names_l)

cat("\nLCA_Class appended.\n")
cat("Distribution — DA:\n"); print(table(df_da_clean$LCA_Class))
cat("Distribution — L:\n");  print(table(df_l_clean$LCA_Class))

# ── 7.19 Local independence check ─────────────────────────────────────────────
check_local_independence(df_da_clean, class_da, lca_vars_da, best_k_da, "DA")
check_local_independence(df_l_clean,  class_l,  lca_vars_l,  best_k_l,  "L")

cat("\nSection 7 complete.\n")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 8 — Regression Analyses
# ══════════════════════════════════════════════════════════════════════════════
# Four OLS models: IAS_Totaal ~ LCA_Class and IATS_Totaal ~ LCA_Class,
# run separately for DA and L samples.
# Reference class = class with the highest conditional P(No chronic disease)
# (i.e. the healthiest class), identified automatically from the conditional
# probability table. Ties broken by lowest P(RiskFactor = "Present").
# ══════════════════════════════════════════════════════════════════════════════

# ── 8.1 Identify healthiest reference classes ─────────────────────────────────
# Reference classes are taken directly from the composite HCS computed in
# Section 7.14b. select_class_profiles() supersedes find_healthy_ref_class().

ref_class_da <- profile_da$most_healthy
ref_class_l  <- profile_l$most_healthy

cat(sprintf("\nReference class \u2014 DA : %s (HCS = %.3f)\n",
            ref_class_da, profile_da$hcs[[ref_class_da]]))
cat(sprintf("Reference class \u2014 L  : %s (HCS = %.3f)\n",
            ref_class_l,  profile_l$hcs[[ref_class_l]]))

# ── 8.2 Relevel LCA_Class and apply treatment coding ─────────────────────────
# contr.treatment() receives the factor LEVELS (character vector) so that the
# contrast matrix columns are named after the actual class labels. Passing an
# integer (nlevels) instead produces columns named "2","3",... which prevents
# class names from appearing in the model term table.

df_da_clean$LCA_Class <- relevel(df_da_clean$LCA_Class, ref = ref_class_da)
df_l_clean$LCA_Class  <- relevel(df_l_clean$LCA_Class,  ref = ref_class_l)

contrasts(df_da_clean$LCA_Class) <- contr.treatment(levels(df_da_clean$LCA_Class))
contrasts(df_l_clean$LCA_Class)  <- contr.treatment(levels(df_l_clean$LCA_Class))

cat("\nTreatment coding applied to LCA_Class.\n")
cat("Reference class — DA:", ref_class_da, "\n")
cat("Reference class — L: ", ref_class_l,  "\n")
cat("\nContrast matrix — DA:\n"); print(contrasts(df_da_clean$LCA_Class))
cat("\nContrast matrix — L:\n");  print(contrasts(df_l_clean$LCA_Class))

# ── 8.3 Fit regression models ─────────────────────────────────────────────────
primary_models <- list(
  DA_IAS  = lm(IAS_Totaal  ~ LCA_Class, data = df_da_clean),
  DA_IATS = lm(IATS_Totaal ~ LCA_Class, data = df_da_clean),
  L_IAS   = lm(IAS_Totaal  ~ LCA_Class, data = df_l_clean),
  L_IATS  = lm(IATS_Totaal ~ LCA_Class, data = df_l_clean)
)
cat("\nFour primary regression models fitted.\n")

# ── 8.4 Extract and print results ─────────────────────────────────────────────
dep_var_map <- list(
  DA_IAS  = "IAS_Totaal", DA_IATS = "IATS_Totaal",
  L_IAS   = "IAS_Totaal", L_IATS  = "IATS_Totaal"
)

ols_list <- setNames(
  lapply(names(primary_models), function(nm) {
    sample <- sub("_.*", "", nm)
    cnames <- if (sample == "DA") class_names_da else class_names_l
    extract_ols_stats(primary_models[[nm]], dep_var = dep_var_map[[nm]],
                      label = nm, class_col = "LCA_Class",
                      class_names = cnames)
  }),
  names(primary_models)
)

cat("\n")
for (nm in names(ols_list)) print_ols_table(ols_list[[nm]])

# ── 8.5 Export to Excel ───────────────────────────────────────────────────────
export_ols_workbook(
  ols_list        = ols_list,
  path            = file.path(base_path, "primary_regression_results.xlsx"),
  analysis_label  = paste0(
    "Primary analysis: LCA with WHO chronic diseases and composite risk factor ",
    "indicator, 3-category calculated SEP. ",
    "Model: outcome ~ LCA_Class; reference = healthiest class."
  ),
  class_term      = "LCA_Class",
  class_names_map = list(DA = class_names_da, L = class_names_l)
)

# ── 8.6 Coefficient plots ─────────────────────────────────────────────────────
p_coef_da <- make_coef_plot(
  ols_list, "DA",
  "Regression coefficients by LCA class — DA dataset"
)
p_coef_l  <- make_coef_plot(
  ols_list, "L",
  "Regression coefficients by LCA class — L dataset"
)

ggsave(file.path(base_path, "primary_regression_coef_DA.png"),
       plot = p_coef_da, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(base_path, "primary_regression_coef_L.png"),
       plot = p_coef_l,  width = 9, height = 5.5, dpi = 300)
cat("Coefficient plots saved.\n")

cat("\nSection 8 complete.\n")
cat("\n═══ ANALYSIS COMPLETE ═══\n")
cat("All outputs saved to:", base_path, "\n")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 9 — Sensitivity Analyses: Covariate-Adjusted Regressions
# ══════════════════════════════════════════════════════════════════════════════
# Same structure as Section 8 (separate LCA classes, multimorbid reference),
# but with additional covariates added to the right-hand side.
# Model: outcome ~ LCA_Class + covariate_1 + covariate_2 + ...
#
# The LCA_Class coefficients now represent the adjusted mean difference in
# IAS/IATS between each class and the reference class, at equal levels of
# the covariates. Comparing these to the unadjusted coefficients from
# Section 8 shows whether class differences are attenuated (i.e. partially
# explained) by the covariates.
# ══════════════════════════════════════════════════════════════════════════════

# ── 9.1 Define covariates ────────────────────────────────────────────────────
# EDIT THESE VECTORS to control which variables are adjusted for.
# Each element must be a variable name present in the corresponding
# clean dataset (df_da_clean or df_l_clean).
# Leave empty — c() — to skip the sensitivity analysis for that sample.

sensitivity_covariates_da <- c("Leeftijd", "Geslacht", "BDI_Totaal")
sensitivity_covariates_l  <- c("Leeftijd", "Geslacht")

# ── 9.2 Validate covariate availability ──────────────────────────────────────
validate_covariates <- function(df, covariates, label) {
  if (length(covariates) == 0) {
    cat("No sensitivity covariates specified for", label, "— skipping.\n")
    return(FALSE)
  }
  missing <- setdiff(covariates, names(df))
  if (length(missing) > 0)
    stop("Sensitivity covariates not found in ", label, " dataset: ",
         paste(missing, collapse = ", "))
  na_counts <- colSums(is.na(df[, covariates, drop = FALSE]))
  if (any(na_counts > 0)) {
    cat("  Note — missing values in covariates (", label, "):\n")
    print(na_counts[na_counts > 0])
    cat("  Cases with missing covariates will be dropped by lm().\n")
  }
  cat("Covariates validated for", label, ":",
      paste(covariates, collapse = ", "), "\n")
  TRUE
}

run_da_sens <- validate_covariates(df_da_clean, sensitivity_covariates_da, "DA")
run_l_sens  <- validate_covariates(df_l_clean,  sensitivity_covariates_l,  "L")

# ── 9.3 Build formulas ──────────────────────────────────────────────────────
build_sens_formula <- function(outcome, covariates) {
  rhs <- paste(c("LCA_Class", covariates), collapse = " + ")
  as.formula(paste(outcome, "~", rhs))
}

# ── 9.4 Fit sensitivity models ──────────────────────────────────────────────
sens_models <- list()

if (run_da_sens) {
  sens_models$DA_IAS  <- lm(build_sens_formula("IAS_Totaal",  sensitivity_covariates_da),
                            data = df_da_clean)
  sens_models$DA_IATS <- lm(build_sens_formula("IATS_Totaal", sensitivity_covariates_da),
                            data = df_da_clean)
  cat("DA sensitivity models fitted.\n")
}

if (run_l_sens) {
  sens_models$L_IAS  <- lm(build_sens_formula("IAS_Totaal",  sensitivity_covariates_l),
                           data = df_l_clean)
  sens_models$L_IATS <- lm(build_sens_formula("IATS_Totaal", sensitivity_covariates_l),
                           data = df_l_clean)
  cat("L sensitivity models fitted.\n")
}

if (length(sens_models) == 0) {
  cat("\nNo sensitivity models to run. Section 9 skipped.\n")
} else {
  
  # ── 9.5 Extract and print results ──────────────────────────────────────────
  sens_dep_var_map <- list(
    DA_IAS  = "IAS_Totaal", DA_IATS = "IATS_Totaal",
    L_IAS   = "IAS_Totaal", L_IATS  = "IATS_Totaal"
  )
  
  sens_ols_list <- setNames(
    lapply(names(sens_models), function(nm) {
      sample <- sub("_.*", "", nm)
      cnames <- if (sample == "DA") class_names_da else class_names_l
      extract_ols_stats(sens_models[[nm]], dep_var = sens_dep_var_map[[nm]],
                        label = nm, class_col = "LCA_Class",
                        class_names = cnames)
    }),
    names(sens_models)
  )
  
  cat("\n")
  for (nm in names(sens_ols_list)) print_ols_table(sens_ols_list[[nm]])
  
  # ── 9.6 Export to Excel ────────────────────────────────────────────────────
  covariates_note <- paste0(
    "Sensitivity analysis: LCA_Class + ",
    paste(unique(c(sensitivity_covariates_da, sensitivity_covariates_l)),
          collapse = ", "), ". ",
    "Model: outcome ~ LCA_Class + covariates; reference = healthiest class."
  )
  
  export_ols_workbook(
    ols_list        = sens_ols_list,
    path            = file.path(base_path, "sensitivity_regression_results.xlsx"),
    analysis_label  = covariates_note,
    class_term      = "LCA_Class",
    class_names_map = list(DA = class_names_da, L = class_names_l)
  )
  
  # ── 9.7 Coefficient plots ─────────────────────────────────────────────────
  if (run_da_sens) {
    p_sens_da <- make_coef_plot(
      sens_ols_list, "DA",
      paste0("Sensitivity: regression coefficients by LCA class — DA\n",
             "Adjusted for: ", paste(sensitivity_covariates_da, collapse = ", "))
    )
    ggsave(file.path(base_path, "sensitivity_regression_coef_DA.png"),
           plot = p_sens_da, width = 9, height = 5.5, dpi = 300)
  }
  
  if (run_l_sens) {
    p_sens_l <- make_coef_plot(
      sens_ols_list, "L",
      paste0("Sensitivity: regression coefficients by LCA class — L\n",
             "Adjusted for: ", paste(sensitivity_covariates_l, collapse = ", "))
    )
    ggsave(file.path(base_path, "sensitivity_regression_coef_L.png"),
           plot = p_sens_l, width = 9, height = 5.5, dpi = 300)
  }
  
  cat("Sensitivity coefficient plots saved.\n")
}

cat("\nSection 9 complete.\n")
cat("\n═══ ANALYSIS COMPLETE ═══\n")
cat("All outputs saved to:", base_path, "\n")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 10 — Reproducibility Check & Session Information
# ══════════════════════════════════════════════════════════════════════════════
# This block runs automatically at the end of the script. It validates that
# all key pipeline objects exist, reports package versions, and writes a full
# sessionInfo() to a text file in the output directory so that the exact
# computational environment is permanently recorded alongside the results.
# ══════════════════════════════════════════════════════════════════════════════

cat("\n══ Reproducibility check ══\n")

# ── 10.1 Pipeline object existence check ──────────────────────────────────────
qc <- list(
  section_1_data_loaded     = all(exists("df_da_clean"), exists("df_l_clean")),
  section_3_sep_calculated  = all("SEP_3Cat" %in% names(df_da_clean),
                                  "SEP_3Cat" %in% names(df_l_clean)),
  section_5_outliers_done   = all(exists("removal_log_da"), exists("removal_log_l")),
  section_6_reliability_run = exists("reliability_tbl"),
  section_7_lca_fitted      = all(exists("model_da"), exists("model_l"),
                                  exists("best_k_da"), exists("best_k_l")),
  section_7_classes_appended = all("LCA_Class" %in% names(df_da_clean),
                                   "LCA_Class" %in% names(df_l_clean)),
  section_8_regression_done = exists("ols_list"),
  output_dir_exists         = dir.exists(base_path)
)

cat("\nPipeline object QC:\n")
for (nm in names(qc)) {
  status <- if (isTRUE(qc[[nm]])) "  OK" else "  FAIL"
  cat(sprintf("  %-40s %s\n", nm, status))
}

all_pass <- all(unlist(qc))
if (all_pass) {
  cat("\nAll QC checks passed.\n")
} else {
  warning("One or more QC checks failed. Review the output above.")
}

# ── 10.2 Package version summary ──────────────────────────────────────────────
cat("\nPackage versions used in this analysis:\n")
pkg_versions <- sapply(required_packages, function(p)
  as.character(packageVersion(p)))
print(as.data.frame(pkg_versions, col.names = "Version"))

# ── 10.3 Write sessionInfo() to file ──────────────────────────────────────────
session_path <- file.path(base_path, "sessionInfo.txt")
tryCatch({
  si <- utils::capture.output(utils::sessionInfo())
  writeLines(si, session_path)
  cat("\nsessionInfo() written to:", session_path, "\n")
}, error = function(e) {
  warning("Could not write sessionInfo: ", e$message)
})

# ── 10.4 Print sessionInfo() to console ───────────────────────────────────────
cat("\n── sessionInfo() ──\n")
utils::sessionInfo()