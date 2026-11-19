# ══════════════════════════════════════════════════════════════════════════════
# LCA ANALYSES
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 1 — Project Setup & Data Loading
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Load packages ──────────────────────────────────────────────────────────
# mice removed: SEP is calculated, not imputed
# clue removed: majority-vote class alignment step no longer used
required_packages <- c(
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
  "broom",      # Tidy model output
  "car"         # Multicollinearity: vif() / GVIF for multi-df predictors
)

load_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Installing ", pkg, " ...")
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

load_packages(required_packages)

# ── 1. File paths ─────────────────────────────────────────────────────────────
path_da <- file.path("data", "raw", "Part1_19082024_clean_JM_version2.sav")
path_l  <- file.path("data", "raw", "Part2_19082024_clean_JM_version2.sav")

# Output directory — created here so all later sections can write to it directly
base_path <- file.path("results")
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

# Prevent masking by MASS or other packages
select  <- dplyr::select
filter  <- dplyr::filter
mutate  <- dplyr::mutate
rename  <- dplyr::rename

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
# AUDIT-C screens for hazardous drinking (not alcohol dependence).
# Cutoffs: No Hazardous Drinking (<5 males, <4 females), Hazardous Drinking (≥cutoff)
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
      Geslacht == "Male"   & Alcohol_Totaal < 5 ~ "No Hazardous Drinking",
      Geslacht == "Female" & Alcohol_Totaal < 4 ~ "No Hazardous Drinking",
      TRUE                                       ~ "Hazardous Drinking"
    ),
    levels = c("No Hazardous Drinking", "Hazardous Drinking")
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

# ── 2.15b Alternative multimorbidity definitions (sensitivity, reviewer M8) ───
# Disease_Cat_Alt uses the same three levels as Disease_Cat, but counts more
# conditions toward multimorbidity:
#   DA: chronic conditions + each risk factor (overweight/obesity, high blood
#       pressure, increased cholesterol) + clinically relevant depressive
#       symptoms (BDI > 13)
#   L:  chronic conditions + each risk factor (no depression measure in L)
# As in 2.5a, a missing indicator counts as absent.

make_disease_cat <- function(count) {
  factor(
    case_when(
      count == 0 ~ "No Disease",
      count == 1 ~ "One Disease",
      count  > 1 ~ "Multimorbid"
    ),
    levels = c("No Disease", "One Disease", "Multimorbid")
  )
}

df_da <- df_da |>
  mutate(
    Disease_Count_Alt = Disease_Count +
      rowSums(across(any_of(risk_factor_vars)), na.rm = TRUE) +
      coalesce(as.integer(BDI_Cat == "Clinically relevant depressive symptoms"), 0L),
    Disease_Cat_Alt   = make_disease_cat(Disease_Count_Alt)
  )

df_l <- df_l |>
  mutate(
    Disease_Count_Alt = Disease_Count +
      rowSums(across(any_of(risk_factor_vars)), na.rm = TRUE),
    Disease_Cat_Alt   = make_disease_cat(Disease_Count_Alt)
  )

cat("\nDisease_Cat_Alt — DA (conditions + risk factors + depression):\n")
print(table(df_da$Disease_Cat_Alt, useNA = "ifany"))
cat("Disease_Cat_Alt — L (conditions + risk factors):\n")
print(table(df_l$Disease_Cat_Alt, useNA = "ifany"))

# ── 2.16 Final variable overview ──────────────────────────────────────────────
print_overview <- function(df, log, label) {
  cat("\n══ Recoded variables summary —", label, "══\n")
  df |>
    select(any_of(c(
      "Geslacht", "Leeftijd_Cat", "Opleiding_Cat", "Inkomen_Cat",
      "Disease_Cat", "Disease_Cat_Alt", "RiskFactor_Cat",
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
    print(broom::tidy(fit_full, conf.int = TRUE, exponentiate = FALSE))
  }
  
  if (n_miss_opl > 0) run_miss_logit("Opleiding_Cat")
  if (n_miss_ink > 0) run_miss_logit("Inkomen_Cat")
  
  invisible(NULL)
}

check_sep_missingness(df_da_recoded, "DA")
check_sep_missingness(df_l_recoded,  "L")

# ── 3.2 Participants with missing SEP components ──────────────────────────────
# SEP is no longer an LCA indicator, so participants with missing education or
# income are RETAINED: the LCA and all unadjusted analyses use the full sample.
# Missing values on LCA indicators (if any) are handled by maximum likelihood in
# poLCA (na.rm = FALSE). Models that adjust for SEP_3Cat drop the missing-SEP
# cases listwise; their N is reported per model. The MCAR/MAR diagnostics in
# 3.1 characterise that listwise deletion.

sep_missing_da <- is.na(df_da_recoded$SEP_Score)
sep_missing_l  <- is.na(df_l_recoded$SEP_Score)

cat("\n── Missing SEP (retained; excluded only from SEP-adjusted models) ──\n")
cat("DA: missing SEP", sum(sep_missing_da), "of", nrow(df_da_recoded), "\n")
cat("L:  missing SEP", sum(sep_missing_l),  "of", nrow(df_l_recoded),  "\n")

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
  "Disease_Cat", "Disease_Cat_Alt", "RiskFactor_Cat",
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
# Continuous variables for distribution checks (descriptive; these feed the
# outlier detection in Section 5). Normality is NOT an LCA assumption for the
# categorical indicators, and the outcomes IAS_Totaal / IATS_Totaal are checked
# where it matters: residual normality and heteroscedasticity of the regression
# models (regression_diagnostics(), Sections 8–10).
cont_da <- c("Leeftijd", "BDI_Totaal", "TAS_Totaal")
cont_l  <- c("Leeftijd", "SSRQ_Totaal", "EEQ_Totaal", "JSS_Totaal", "Alcohol_Totaal")

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


# ── 4.6 Multicollinearity checks — LCA indicators (Cramér's V) ────────────────
# LCA variable sets defined here (single source of truth used in Section 7)
# Health-only indicator sets (reviewer M1): age, sex and SEP are NOT class
# indicators, so the classes reflect health status rather than demographic
# profiles. Age, sex and SEP enter the regressions as covariates (Section 9)
# and are described per class (Section 7.18a).
lca_vars_da <- c("Disease_Cat", "RiskFactor_Cat", "BDI_Cat", "TAS_Cat")

lca_vars_l  <- c("Disease_Cat", "RiskFactor_Cat", "IPAQ_Cat", "Alcohol_Cat",
                 "Roken_Cat", "SSRQ_Cat", "EEQ_Cat", "JSS_Cat")

# Alternative-definition LCA (reviewer M8; L only, Section 12).
# Disease_Cat_Alt already counts the risk factors, so RiskFactor_Cat is dropped.
# No DA equivalent: Disease_Cat_Alt absorbs RiskFactor_Cat and BDI_Cat, leaving
# only Disease_Cat_Alt + TAS_Cat, which cannot identify K ≥ 2.
lca_vars_l_alt <- c("Disease_Cat_Alt", "IPAQ_Cat", "Alcohol_Cat", "Roken_Cat",
                    "SSRQ_Cat", "EEQ_Cat", "JSS_Cat")

# Burden-pole category (integer factor level) per indicator, used only for the
# descriptive salsa-effect check in candidate_diagnostics().
burden_levels_da <- c(Disease_Cat = 3, RiskFactor_Cat = 2, BDI_Cat = 2, TAS_Cat = 2)
burden_levels_l  <- c(Disease_Cat = 3, RiskFactor_Cat = 2,
                      IPAQ_Cat = 1,     # Insufficiently Active
                      Alcohol_Cat = 2, Roken_Cat = 2,
                      SSRQ_Cat = 1,     # Below Average Self-Regulation
                      EEQ_Cat = 3, JSS_Cat = 2)
burden_levels_l_alt <- c(Disease_Cat_Alt = 3,
                         burden_levels_l[setdiff(lca_vars_l_alt, "Disease_Cat_Alt")])

cat("\n══ Cramér's V — LCA indicators — DA ══\n")
v_mat_da <- cramers_v_matrix(df_da_recoded, lca_vars_da, "DA")
plot_cramers_v(v_mat_da, "DA sample")

cat("\n══ Cramér's V — LCA indicators — L ══\n")
v_mat_l <- cramers_v_matrix(df_l_recoded, lca_vars_l, "L")
plot_cramers_v(v_mat_l, "L sample")

cat("\n══ Cramér's V — LCA indicators — L (alternative definition) ══\n")
v_mat_l_alt <- cramers_v_matrix(df_l_recoded, lca_vars_l_alt, "L (alternative)")
plot_cramers_v(v_mat_l_alt, "L sample — alternative definition")

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
  row_data("Hazardous drinking (AUDIT-C positive screen)", "",
           fmt_categorical(df_l_clean, "Alcohol_Cat", "Hazardous Drinking")),
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

# Export: single source for the Methods 2.2 text and Table 1 (reviewer m1)
writexl::write_xlsx(
  list(
    Reliability = as.data.frame(reliability_tbl),
    Note        = data.frame(Note = paste0(
      "Alpha_raw = raw Cronbach's alpha; Alpha_std = standardized alpha ",
      "(reference only); Omega_total = McDonald's omega total. Report ",
      "Alpha_raw and Omega_total consistently in the Methods text and Table 1."
    ))
  ),
  path = file.path(base_path, "internal_consistency.xlsx")
)
cat("Internal consistency exported.\n")

# ── 6.5 IAS–IATS correlation (reviewer M6) ────────────────────────────────────
# Overall Pearson correlation per dataset. Per-class correlations follow in
# Section 11, once LCA classes are assigned.
cor_ias_iats <- function(df, label, group = "Overall") {
  ok <- complete.cases(df[, c("IAS_Totaal", "IATS_Totaal")])
  ct <- cor.test(df$IAS_Totaal[ok], df$IATS_Totaal[ok], method = "pearson")
  data.frame(Dataset = label, Group = group, n = sum(ok),
             r       = round(unname(ct$estimate), 3),
             CI_low  = round(ct$conf.int[1], 3),
             CI_high = round(ct$conf.int[2], 3),
             p       = signif(ct$p.value, 3),
             stringsAsFactors = FALSE)
}

cor_overall <- bind_rows(cor_ias_iats(df_da_clean, "DA"),
                         cor_ias_iats(df_l_clean,  "L"))
cat("\n═══ IAS–IATS correlation (Pearson) ═══\n")
print(cor_overall, row.names = FALSE)

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

# ── Run LCA across the K range on a single dataset ────────────────────────────
# Fits K = k_range with nrep random starts each (poLCA keeps the best run).
# Missing indicator values are handled by maximum likelihood (na.rm = FALSE).
# Per K, the fit table records the full set of enumeration statistics:
#   LL, free parameters, residual df, AIC, BIC, SABIC (Sclove), CAIC
#   (Bozdogan), AWE (Banfield & Raftery), relative entropy, smallest class
#   (estimated π and modal n), and ML replication: the share of random starts
#   that reached the best log-likelihood within rep_tol LL units (poLCA's
#   $attempts holds the final LL of every start).
run_lca_single <- function(lca_data, formula, k_range, nrep = 50, seed = 123,
                           rep_tol = 0.01, rep_min_prop = 0.10) {
  fit_rows   <- list()
  model_list <- list()
  for (K in k_range) {
    set.seed(seed + K)
    cat("\n── LCA K =", K, "(nrep =", nrep, ") ──\n")
    tryCatch({
      mod  <- poLCA(formula, data = lca_data, nclass = K, nrep = nrep,
                    na.rm = FALSE, verbose = FALSE, graphs = FALSE)
      N    <- mod$N
      ll   <- mod$llik
      npar <- mod$npar
      att  <- mod$attempts
      if (length(att) == 0) att <- ll   # K = 1: unique solution, no restarts
      n_rep   <- sum(abs(att - max(att)) < rep_tol)
      modal_n <- tabulate(mod$predclass, nbins = K)
      fit_rows[[as.character(K)]] <- data.frame(
        K = K, LL = ll, npar = npar, resid_df = mod$resid.df,
        AIC   = mod$aic,
        BIC   = mod$bic,
        SABIC = -2 * ll + npar * log((N + 2) / 24),
        CAIC  = -2 * ll + npar * (log(N) + 1),
        AWE   = -2 * ll + 2 * npar * (log(N) + 1.5),
        Entropy = calc_entropy(mod$posterior),
        Min_pi  = min(mod$P),
        Min_n   = min(modal_n),
        Starts  = length(att),
        Replicated      = n_rep,
        Replicated_prop = n_rep / length(att),
        Replication_ok  = K == 1 || n_rep / length(att) >= rep_min_prop
      )
      cat(sprintf("  BIC: %.1f | Min π: %.3f | Min n: %d | LL replicated: %d/%d\n",
                  mod$bic, min(mod$P), min(modal_n), n_rep, length(att)))
      model_list[[as.character(K)]] <- mod
    }, error = function(e) {
      warning("poLCA failed K = ", K, ": ", e$message)
    })
  }
  list(fit_table = bind_rows(fit_rows), models = model_list)
}

# ── Integer-coded LCA data ────────────────────────────────────────────────────
# poLCA requires integer-coded factor variables (1-based); NA stays NA.
prep_lca_data <- function(df, vars) {
  df |> select(all_of(vars)) |>
    mutate(across(everything(), as.integer)) |>
    as.data.frame()
}

# ── LCA formula builder ───────────────────────────────────────────────────────
make_lca_formula <- function(vars) {
  as.formula(paste0("cbind(", paste(vars, collapse = ", "), ") ~ 1"))
}

# ── Identified K range ────────────────────────────────────────────────────────
# Keeps only K whose model has positive residual degrees of freedom:
#   df = min(N, number of response patterns) − 1 − free parameters,
#   free parameters = (K − 1) + K × Σ(categories − 1)
# (the same df poLCA reports as resid.df). Needed because the health-only DA
# indicator set (3 × 2 × 2 × 2 = 24 patterns) cannot identify K ≥ 4.
identified_k_range <- function(df, vars, k_max = 10, label = "") {
  n_cat    <- vapply(vars, function(v) nlevels(df[[v]]), integer(1))
  cells    <- min(nrow(df), prod(n_cat))
  k_all    <- seq_len(k_max)
  npar     <- (k_all - 1) + k_all * sum(n_cat - 1)
  resid_df <- cells - 1 - npar
  k_ok     <- k_all[resid_df > 0]
  cat(sprintf("\nIdentified K range — %s: K = %d–%d (residual df at K = %d: %d)\n",
              label, min(k_ok), max(k_ok), max(k_ok), resid_df[max(k_ok)]))
  if (max(k_ok) < k_max)
    cat(sprintf("  K = %d–%d skipped: residual df ≤ 0 (model not identified).\n",
                max(k_ok) + 1, k_max))
  k_ok
}

# ── LCA category frequency check ─────────────────────────────────────────────
check_lca_frequencies <- function(df, vars, label, threshold = 0.10) {
  cat("\n=== LCA indicator frequencies —", label, "===\n")
  for (v in vars) {
    # Percentages among observed values; missing values reported separately
    tbl  <- table(df[[v]])
    prop <- prop.table(tbl)
    result <- data.frame(N    = as.integer(tbl),
                         Pct  = round(as.numeric(prop) * 100, 1),
                         Flag = ifelse(prop < threshold, "FLAG < 10%", "OK"),
                         row.names = names(tbl))
    cat("\nVariable:", v, " (missing:", sum(is.na(df[[v]])), ")\n"); print(result)
  }
}

# ── Class enumeration: candidate shortlist ────────────────────────────────────
# Candidate-set approach (Masyn, 2013; Nylund-Gibson & Choi, 2018; Sorgente et
# al., 2025):
#   1. Size filter: exclude K with any class of estimated π < min_pi or fewer
#      than min_n modally assigned cases (Nylund-Gibson & Choi: 5% / 50).
#   2. Shortlist (max_cand, ordered by BIC) from the eligible K:
#        - the lowest-BIC model;
#        - every model with approximate correct-model probability cmP > cmp_min
#          (cmP computed across the eligible models; Masyn: > .10);
#        - the smallest K whose approximate Bayes factor BF(K, K+1) > 3.
#   Entropy and other classification diagnostics are NOT used here; they
#   describe the shortlisted candidates (candidate_diagnostics()).
select_k_candidates <- function(fit_table, blrt_table = NULL,
                                min_pi = 0.05, min_n = 50,
                                cmp_min = 0.10, max_cand = 3, label = "") {
  ft  <- fit_table[order(fit_table$K), ]
  log <- character(0)

  sic        <- -0.5 * ft$BIC
  ft$BF_next <- c(exp(sic[-nrow(ft)] - sic[-1]), NA_real_)   # BF(K vs K+1)
  ft$Size_ok <- ft$K == 1 | (ft$Min_pi >= min_pi & ft$Min_n >= min_n)

  elig <- ft$Size_ok
  ft$cmP <- NA_real_
  w <- exp(sic[elig] - max(sic[elig]))
  ft$cmP[elig] <- w / sum(w)

  if (!is.null(blrt_table) && nrow(blrt_table) > 0)
    ft <- dplyr::left_join(ft, dplyr::select(blrt_table, K = K1, BLRT_p = p_boot),
                           by = "K")
  else ft$BLRT_p <- NA_real_

  excluded <- ft$K[!elig]
  log <- c(log, sprintf("[%s] Step 1 — size filter (π ≥ %.2f and n ≥ %d): %s",
                        label, min_pi, min_n,
                        if (length(excluded)) paste0("excluded K = ",
                                                     paste(excluded, collapse = ", "))
                        else "no K excluded"))

  el      <- ft[elig, ]
  k_bic   <- el$K[which.min(el$BIC)]
  k_cmp   <- el$K[el$cmP > cmp_min]
  bf_ok   <- el$K[!is.na(el$BF_next) & el$BF_next > 3 &
                    el$K %in% ft$K[elig]]
  k_bf    <- if (length(bf_ok)) min(bf_ok) else integer(0)
  cand    <- unique(c(k_bic, k_cmp, k_bf))
  cand    <- cand[order(el$BIC[match(cand, el$K)])][seq_len(min(max_cand, length(cand)))]

  log <- c(log,
           sprintf("[%s] Step 2 — lowest BIC among eligible K: K = %d", label, k_bic),
           sprintf("[%s]          cmP > %.2f: K = %s", label, cmp_min,
                   paste(k_cmp, collapse = ", ")),
           sprintf("[%s]          smallest K with BF(K, K+1) > 3: %s", label,
                   if (length(k_bf)) k_bf else "none"),
           sprintf("[%s]          shortlist (ordered by BIC): K = %s", label,
                   paste(cand, collapse = ", ")))

  ft$Shortlisted <- ft$K %in% cand
  list(fit_table = ft, shortlist = cand, log = log)
}

# ── Bootstrap LRT ─────────────────────────────────────────────────────────────
# Parametric bootstrap LRT for K−1 vs K (reported, not a selection rule). The
# observed LR uses the enumeration models (best of nrep starts); bootstrap
# samples are refitted with nrep_boot starts. Stops after the first
# non-significant comparison.
run_bootstrap_lrt <- function(lca_data, formula, models, k_range,
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
    mod0 <- models[[as.character(k0)]]
    mod1 <- models[[as.character(k1)]]
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
        prob_mat     <- mod0$probs[[v]]
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

# ── Deterministic class order ─────────────────────────────────────────────────
# poLCA returns classes in an arbitrary order (it depends on the random starts),
# so letters would not be stable across runs. The final model's classes are
# ordered by the Methods reference rule: descending P(healthy category of
# disease_var), ties (at 3 decimals) broken by the lowest P(tiebreak level).
# Class A is therefore always the regression reference class.
order_by_health <- function(model, disease_var, healthy_level = 1L,
                            tiebreak_var, tiebreak_level) {
  p_h <- round(model$probs[[disease_var]][, healthy_level], 3)
  p_t <- round(model$probs[[tiebreak_var]][, tiebreak_level], 3)
  order(-p_h, p_t)
}

# Refits the model from its own estimates in the new class order
# (poLCA.reorder + probs.start), so every element of the poLCA object
# (P, probs, posterior, predclass, SEs) is permuted consistently. The refit
# starts at the MLE, so the log-likelihood must be unchanged.
reorder_lca_model <- function(model, new_order, lca_data, formula, label = "") {
  if (identical(as.integer(new_order), seq_along(new_order))) return(model)
  start <- poLCA.reorder(model$probs, new_order)
  mod2  <- poLCA(formula, data = lca_data, nclass = length(new_order),
                 probs.start = start, nrep = 1, na.rm = FALSE,
                 verbose = FALSE, graphs = FALSE)
  if (abs(mod2$llik - model$llik) > 0.01 ||
      max(abs(mod2$P - model$P[new_order])) > 0.001)
    stop("[", label, "] Class reordering changed the solution (LL ",
         round(model$llik, 3), " vs ", round(mod2$llik, 3), ").")
  mod2$attempts <- model$attempts   # keep replication record of the final fit
  mod2
}

# ── Final K: pre-specified rule (no manual override) ──────────────────────────
# Shortlisted candidates are tried in BIC order. Each is refitted with
# nrep_final random starts; the first whose best log-likelihood is replicated
# (≥ rep_min_prop of starts within rep_tol LL units) becomes the final model.
# If none replicates, the lowest-BIC candidate is kept with a warning. Every
# step is logged.
choose_final_k <- function(shortlist, lca_data, formula, nrep_final = 100,
                           seed = 999, rep_tol = 0.01, rep_min_prop = 0.10,
                           label = "") {
  log <- character(0)
  fits <- list()
  for (k in shortlist) {
    set.seed(seed + k)
    mod <- poLCA(formula, data = lca_data, nclass = k, nrep = nrep_final,
                 na.rm = FALSE, verbose = FALSE, graphs = FALSE)
    att   <- mod$attempts
    if (length(att) == 0) att <- mod$llik
    n_rep <- sum(abs(att - max(att)) < rep_tol)
    ok    <- k == 1 || n_rep / length(att) >= rep_min_prop
    fits[[as.character(k)]] <- mod
    log <- c(log, sprintf(
      "[%s] Step 3 — refit K = %d with %d starts: best LL = %.2f reached by %d/%d starts (%.0f%%) → %s",
      label, k, length(att), max(att), n_rep, length(att),
      100 * n_rep / length(att), if (ok) "REPLICATED" else "not replicated"))
    if (ok) {
      log <- c(log, sprintf("[%s] Final K = %d (first replicated candidate in BIC order)",
                            label, k))
      return(list(K = k, model = mod, log = log, replicated = TRUE))
    }
  }
  k <- shortlist[1]
  msg <- sprintf(paste0("[%s] No shortlisted candidate replicated its best ",
                        "log-likelihood; K = %d (lowest BIC) kept. Increase ",
                        "nrep_final and re-run before interpreting."), label, k)
  warning(msg)
  list(K = k, model = fits[[as.character(k)]], log = c(log, msg), replicated = FALSE)
}

# ── Candidate diagnostics (classification quality, local independence, salsa) ─
# Descriptive diagnostics for one shortlisted model (Masyn, 2013; Sorgente et
# al., 2025), one row per class:
#   π with 95% CI (poLCA SE), modal class proportion (mcaP) and whether it lies
#   in that CI, average posterior probability (AvePP; > .70 adequate), odds of
#   correct classification (OCC; > 5 adequate), relative entropy, the number of
#   indicator pairs with Cramér's V > .50 within the modal class (local
#   independence screen, Sinha et al., 2021), and a salsa-effect summary: the
#   share of indicators on which the classes are ordered identically by
#   P(burden category) (1 = fully parallel profiles, i.e. a severity gradient).
candidate_diagnostics <- function(model, df_clean, vars, burden_levels, label) {
  K     <- length(model$P)
  post  <- model$posterior
  modal <- model$predclass
  N     <- nrow(post)
  pi_se <- if (!is.null(model$P.se)) model$P.se else rep(NA_real_, K)

  avepp <- vapply(seq_len(K), function(k)
    if (any(modal == k)) mean(post[modal == k, k]) else NA_real_, numeric(1))
  mcap  <- tabulate(modal, nbins = K) / N
  occ   <- (avepp / (1 - avepp)) / (model$P / (1 - model$P))

  li_flags <- vapply(seq_len(K), function(k) {
    sub   <- df_clean[modal == k, vars, drop = FALSE]
    pairs <- combn(vars, 2, simplify = FALSE)
    sum(vapply(pairs, function(p) {
      v <- cramers_v(sub[[p[1]]], sub[[p[2]]])
      !is.na(v) && v > 0.50
    }, logical(1)))
  }, integer(1))

  # Salsa summary: class ordering by P(burden category) per indicator
  orders <- vapply(vars, function(v) {
    p <- model$probs[[v]][, burden_levels[[v]]]
    paste(order(p, decreasing = TRUE), collapse = ">")
  }, character(1))
  modal_order <- names(sort(table(orders), decreasing = TRUE))[1]
  parallel_n  <- sum(orders == modal_order)

  data.frame(
    Dataset = label, K = K, Class = paste("Class", LETTERS[seq_len(K)]),
    pi = round(model$P, 3),
    pi_CI_low  = round(model$P - 1.96 * pi_se, 3),
    pi_CI_high = round(model$P + 1.96 * pi_se, 3),
    mcaP = round(mcap, 3),
    mcaP_in_CI = mcap >= model$P - 1.96 * pi_se & mcap <= model$P + 1.96 * pi_se,
    AvePP = round(avepp, 3),
    OCC   = round(occ, 2),
    Entropy = round(calc_entropy(post), 3),
    LI_pairs_V_gt_.50 = li_flags,
    # With K = 2 there are only two possible orders, so identical ordering on
    # every indicator is expected and the check is not informative.
    Salsa_parallel = if (K == 2) "not informative for K = 2" else
      sprintf("%d of %d indicators in order %s",
              parallel_n, length(vars), modal_order),
    Salsa_flag = if (K == 2) NA else parallel_n == length(vars),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

# ── Full enumeration pipeline for one dataset ─────────────────────────────────
# Frequencies and Cramér's V screen → identified K range → enumeration (nrep
# starts per K) → BLRT → candidate shortlist → candidate diagnostics → final K
# by the pre-specified replication rule (nrep_final starts) → exports:
#   LCA_fitindices_<file_label>.xlsx  full fit table (all K) + entropy note
#   LCA_diagnostics_<file_label>.xlsx classification diagnostics, candidates
#   LCA_decisionlog_<file_label>.txt  and .xlsx: every enumeration step
#   LCA_elbowplot_<file_label>.png    fit-index plot with shortlist and final K
enumerate_lca <- function(df_clean, vars, burden_levels, label, file_label,
                          seed, k_max = 10, nrep = 50, nrep_final = 100,
                          B = 100, nrep_boot = 3, min_pi = 0.05, min_n = 50,
                          entropy_flag = 0.80,
                          order_var = "Disease_Cat",
                          tiebreak_var = "RiskFactor_Cat",
                          tiebreak_level = 2L) {
  check_lca_frequencies(df_clean, vars, label)
  cat("\n── Cramér's V — LCA indicators —", label, "──\n")
  cramers_v_matrix(df_clean, vars, label)

  lca_data <- prep_lca_data(df_clean, vars)
  formula  <- make_lca_formula(vars)
  k_range  <- identified_k_range(df_clean, vars, k_max = k_max, label = label)

  enum <- run_lca_single(lca_data, formula, k_range, nrep = nrep, seed = seed)
  cat("\nRunning BLRT —", label, "...\n")
  blrt <- run_bootstrap_lrt(lca_data, formula, enum$models, enum$fit_table$K,
                            nrep_boot = nrep_boot, B = B, seed = seed,
                            label = label)
  sel  <- select_k_candidates(enum$fit_table, blrt, min_pi = min_pi,
                              min_n = min_n, label = label)

  fin <- choose_final_k(sel$shortlist, lca_data, formula,
                        nrep_final = nrep_final, seed = seed + 1000L,
                        label = label)
  if (fin$K == 1)
    stop("[", label, "] The one-class model is selected: the data do not ",
         "support latent classes with these indicators. Revisit the indicator ",
         "set before continuing.")

  # Order the final classes by the Methods reference rule (Class A = reference)
  ord <- order_by_health(fin$model, order_var, 1L, tiebreak_var, tiebreak_level)
  fin$model <- reorder_lca_model(fin$model, ord, lca_data, formula, label)
  fin$log <- c(fin$log, sprintf(
    "[%s] Classes ordered by descending P(%s = level 1), ties by lowest P(%s = level %d): poLCA classes %s → Class %s",
    label, order_var, tiebreak_var, tiebreak_level,
    paste(ord, collapse = ", "), paste(LETTERS[seq_along(ord)], collapse = ", ")))

  # Diagnostics: the final K uses the final (ordered) model, so its class
  # letters match all other outputs. Other shortlisted K use their enumeration
  # fit, whose class order is their own.
  diag <- dplyr::bind_rows(lapply(sel$shortlist[sel$shortlist > 1], function(k) {
    is_final <- k == fin$K
    mod <- if (is_final) fin$model else enum$models[[as.character(k)]]
    cbind(candidate_diagnostics(mod, df_clean, vars, burden_levels, label),
          Fit = if (is_final) "final model (classes as in all other outputs)"
                else "enumeration fit (own class order)",
          stringsAsFactors = FALSE)
  }))

  ft <- sel$fit_table
  ft$Final <- ft$K == fin$K
  ent_final <- calc_entropy(fin$model$posterior)
  entropy_note <- if (ent_final < entropy_flag)
    sprintf(paste0("Note. Relative entropy of the final model (K = %d) is %.3f ",
                   "(< %.2f): limited classification precision, so class ",
                   "differences are estimated with BCH (corrects for ",
                   "classification error). Entropy was not used to select K."),
            fin$K, ent_final, entropy_flag) else NULL
  if (!is.null(entropy_note)) warning("[", label, "] ", entropy_note)

  log <- c(sel$log, fin$log,
           if (!is.null(entropy_note)) paste0("[", label, "] ", entropy_note))
  cat("\n═══ Enumeration decision log —", label, "═══\n")
  cat(log, sep = "\n")

  # ── Exports ──
  num_cols <- vapply(ft, is.numeric, logical(1))
  ft_out <- ft
  ft_out[num_cols] <- lapply(ft_out[num_cols], function(x) round(x, 3))
  export_fit_table(ft_out, file_label,
                   file.path(base_path, paste0("LCA_fitindices_", file_label, ".xlsx")),
                   note = paste(c(
                     paste0("BF_next = approximate Bayes factor K vs K+1; cmP = ",
                            "approximate correct-model probability across eligible K; ",
                            "Size_ok: π ≥ ", min_pi, " and modal n ≥ ", min_n,
                            "; Replicated = starts reaching the best LL within 0.01."),
                     entropy_note), collapse = " "))
  writexl::write_xlsx(list(Diagnostics = diag),
                      path = file.path(base_path,
                                       paste0("LCA_diagnostics_", file_label, ".xlsx")))
  writeLines(log, file.path(base_path, paste0("LCA_decisionlog_", file_label, ".txt")),
             useBytes = TRUE)
  writexl::write_xlsx(list(Decision_log = data.frame(Step = log),
                           Fit_table = ft_out),
                      path = file.path(base_path,
                                       paste0("LCA_decisionlog_", file_label, ".xlsx")))
  p_elbow <- make_elbow_plot(ft, final_k = fin$K, shortlist = sel$shortlist,
                             blrt_table = blrt,
                             title = paste0("LCA fit indices — ", label,
                                            "  |  Final K = ", fin$K))
  ggsave(file.path(base_path, paste0("LCA_elbowplot_", file_label, ".png")),
         plot = p_elbow, width = 15, height = 5, dpi = 300)

  list(fit_table = ft, blrt = blrt, shortlist = sel$shortlist,
       final_k = fin$K, model = fin$model, replicated = fin$replicated,
       diagnostics = diag, log = log, entropy_note = entropy_note,
       lca_data = lca_data, formula = formula)
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

# ── Select class profiles: healthiest and most burdened classes ───────────────
# Computes two composite scores per class (descriptive reporting only):
#
#   HCS (Healthy Composite Score)     = mean P(healthy pole)     across scored vars
#   BCS (Burden Composite Score)      = mean P(burden pole)      across scored vars
#
# Column means are used so scores remain on a 0–1 scale and stay comparable
# across datasets with different numbers of indicators.
#
# The regression reference class is NOT taken from the HCS; it follows the
# Methods rule in select_reference_class() (reviewer M3).
#
# Arguments:
#   cond_probs   — output of extract_cond_probs() for one dataset
#   class_names  — character vector of K class names
#   scoring_spec — list of lists, each with:
#                    variable        : matches cond_probs$Variable exactly
#                    healthy_cat     : healthy-pole category label
#                    burden_cat      : burden-pole category label (psychosocial,
#                                      behavioural or disease burden)
#   label        — dataset identifier for console output
#
# Returns a list (invisible):
#   most_healthy, most_burdened     — selected class names
#   hcs, bcs                        — named numeric vectors of mean scores
#   score_table                     — wide data.frame (one row per class)
#   hcs_matrix, bcs_matrix          — component probability matrices (vars × classes)

select_class_profiles <- function(cond_probs, class_names, scoring_spec,
                                  label = "") {
  
  K          <- length(class_names)
  class_cols <- paste0("Class_", seq_len(K))
  n_vars     <- length(scoring_spec)
  var_names  <- vapply(scoring_spec, `[[`, character(1L), "variable")
  
  # ── Extract component probabilities ──────────────────────────────────────────
  hcs_matrix <- matrix(NA_real_, nrow = n_vars, ncol = K,
                       dimnames = list(var_names, class_names))
  bcs_matrix <- matrix(NA_real_, nrow = n_vars, ncol = K,
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
                           Category == spec$burden_cat)
    if (nrow(row_m) == 0)
      stop("select_class_profiles [", label, "]: burden category not found — ",
           "Variable = '", spec$variable,
           "', Category = '", spec$burden_cat, "'.")
    bcs_matrix[i, ] <- as.numeric(row_m[1L, class_cols])
  }
  
  # ── Composite scores (column means across variables) ─────────────────────────
  hcs <- colMeans(hcs_matrix, na.rm = TRUE)
  bcs <- colMeans(bcs_matrix, na.rm = TRUE)
  
  # ── Winning classes ───────────────────────────────────────────────────────────
  most_healthy     <- class_names[which.max(hcs)]
  most_burdened    <- class_names[which.max(bcs)]
  
  # ── Tie warnings ──────────────────────────────────────────────────────────────
  if (sum(hcs == max(hcs)) > 1L)
    warning("select_class_profiles [", label, "]: tie on HCS between: ",
            paste(class_names[hcs == max(hcs)], collapse = " | "),
            ". Inspect score_table and resolve manually.")
  if (sum(bcs == max(bcs)) > 1L)
    warning("select_class_profiles [", label, "]: tie on BCS between: ",
            paste(class_names[bcs == max(bcs)], collapse = " | "),
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
  print_score_block(bcs_matrix, bcs, most_burdened, "BCS")
  
  cat(strrep("\u2550", total_w), "\n")
  
  # ── Exportable score table (wide: one row per class) ─────────────────────────
  score_table <- data.frame(Class = class_names, stringsAsFactors = FALSE)
  for (i in seq_len(n_vars))
    score_table[[paste0("HCS_", var_names[i])]] <- round(hcs_matrix[i, ], 3L)
  score_table[["HCS"]] <- round(hcs, 3L)
  for (i in seq_len(n_vars))
    score_table[[paste0("BCS_", var_names[i])]] <- round(bcs_matrix[i, ], 3L)
  score_table[["BCS"]] <- round(bcs, 3L)
  
  # ── Return ────────────────────────────────────────────────────────────────────
  invisible(list(
    most_healthy     = most_healthy,
    most_burdened    = most_burdened,
    hcs              = setNames(round(hcs, 3L), class_names),
    bcs              = setNames(round(bcs, 3L), class_names),
    score_table      = score_table,
    hcs_matrix       = round(hcs_matrix, 3L),
    bcs_matrix       = round(bcs_matrix, 3L),
    label            = label
  ))
}

# ── Select regression reference class (Methods rule, reviewer M3) ─────────────
# Reference = class with the highest P(healthy disease category). Ties are
# broken by the lowest P(tiebreak category) — by default risk factors present.
select_reference_class <- function(cond_probs, class_names,
                                   disease_var    = "Disease Status",
                                   healthy_cat    = "No chronic conditions",
                                   tiebreak_var   = "Risk Factors",
                                   tiebreak_cat   = "Present",
                                   label          = "") {
  class_cols <- paste0("Class_", seq_along(class_names))
  get_row <- function(v, cat_lbl) {
    row <- dplyr::filter(cond_probs, Variable == v, Category == cat_lbl)
    if (nrow(row) == 0)
      stop("select_reference_class [", label, "]: '", v, "' / '", cat_lbl,
           "' not found in cond_probs.")
    setNames(as.numeric(row[1L, class_cols]), class_names)
  }
  p_healthy  <- get_row(disease_var,  healthy_cat)
  p_tiebreak <- get_row(tiebreak_var, tiebreak_cat)

  cands <- class_names[p_healthy == max(p_healthy)]
  ref   <- if (length(cands) == 1) cands else
    cands[which.min(p_tiebreak[cands])]

  cat(sprintf("\nReference class — %s: %s  [P(%s) = %.3f%s]\n",
              label, ref, healthy_cat, p_healthy[[ref]],
              if (length(cands) > 1)
                sprintf("; tie broken by lowest P(%s) = %.3f",
                        tiebreak_cat, p_tiebreak[[ref]]) else ""))
  print(data.frame(Class = class_names,
                   P_healthy = p_healthy, P_tiebreak = p_tiebreak,
                   row.names = NULL), row.names = FALSE)
  ref
}

# ── Select multimorbidity class (explicit rule) ───────────────────────────────
# Mirror of the reference rule: the class with the highest P(Multimorbidity);
# ties broken by the HIGHEST P(risk factors present). This is the class the text
# may call the "multimorbidity class"; the BCS in select_class_profiles() is a
# broader burden composite and is reported alongside it, not used for this.
select_multimorbid_class <- function(cond_probs, class_names,
                                     disease_var  = "Disease Status",
                                     mm_cat       = "Multimorbidity",
                                     tiebreak_var = "Risk Factors",
                                     tiebreak_cat = "Present",
                                     label        = "") {
  class_cols <- paste0("Class_", seq_along(class_names))
  get_row <- function(v, cat_lbl) {
    row <- dplyr::filter(cond_probs, Variable == v, Category == cat_lbl)
    if (nrow(row) == 0)
      stop("select_multimorbid_class [", label, "]: '", v, "' / '", cat_lbl,
           "' not found in cond_probs.")
    setNames(as.numeric(row[1L, class_cols]), class_names)
  }
  p_mm <- get_row(disease_var,  mm_cat)
  p_tb <- get_row(tiebreak_var, tiebreak_cat)
  cands <- class_names[p_mm == max(p_mm)]
  mm    <- if (length(cands) == 1) cands else cands[which.max(p_tb[cands])]
  cat(sprintf("\nMultimorbidity class — %s: %s  [P(%s) = %.3f%s]\n",
              label, mm, mm_cat, p_mm[[mm]],
              if (length(cands) > 1)
                sprintf("; tie broken by highest P(%s) = %.3f",
                        tiebreak_cat, p_tb[[mm]]) else ""))
  list(class = mm, p_mm = p_mm, p_tiebreak = p_tb)
}

# ── p-value formatting for exports and console ────────────────────────────────
fmt_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .001, "< .001", sprintf("%.4f", p)))
}

# ── Class demographics by modal class ─────────────────────────────────────────
# Age, sex and SEP are no longer LCA indicators, so they are described per
# class here (for Results 3.2 and Tables 3–4).
describe_class_demographics <- function(df, class_col, label) {
  pct <- function(x) round(100 * mean(x, na.rm = TRUE), 1)
  df |>
    dplyr::group_by(Class = .data[[class_col]]) |>
    dplyr::summarise(
      n                   = dplyr::n(),
      Age_M               = round(mean(Leeftijd, na.rm = TRUE), 1),
      Age_SD              = round(sd(Leeftijd,   na.rm = TRUE), 1),
      Age_45plus_pct      = pct(Leeftijd_Cat == "45 or older"),
      Female_pct          = pct(Geslacht     == "Female"),
      SEP_below_avg_pct   = pct(SEP_3Cat     == "Below Average SEP"),
      SEP_average_pct     = pct(SEP_3Cat     == "Average SEP"),
      SEP_above_avg_pct   = pct(SEP_3Cat     == "Above Average SEP"),
      .groups = "drop"
    ) |>
    dplyr::mutate(Dataset = label, .before = 1) |>
    as.data.frame()
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
# Panels: information criteria (BIC, SABIC, CAIC), AIC, relative entropy
# (descriptive only) and BLRT p. Shortlisted candidates are drawn as filled
# points; the dashed line marks the final K.
make_elbow_plot <- function(fit_table, final_k, shortlist = integer(0),
                            blrt_table = NULL, title = "") {
  col_sel  <- "#2166ac"
  col_line <- "#333333"
  col_elb  <- "#d73027"

  theme_e <- theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          axis.title       = element_text(size = 10),
          plot.title       = element_text(size = 11, face = "bold"),
          legend.position  = "bottom",
          legend.title     = element_blank())

  ft <- fit_table
  ft$cand <- ft$K %in% shortlist
  v_final <- geom_vline(xintercept = final_k, colour = col_sel,
                        linetype = "dashed", linewidth = 0.7)
  x_scale <- scale_x_continuous(breaks = ft$K)

  ic_long <- dplyr::bind_rows(lapply(c("BIC", "SABIC", "CAIC"), function(ic)
    data.frame(K = ft$K, value = ft[[ic]], index = ic, cand = ft$cand)))
  p_ic <- ggplot(ic_long, aes(x = K, y = value, linetype = index, shape = index)) +
    geom_line(colour = col_line, linewidth = 0.7) +
    geom_point(aes(fill = cand), size = 2.3, colour = col_line) +
    scale_shape_manual(values = c(BIC = 21, SABIC = 22, CAIC = 24)) +
    scale_fill_manual(values = c(`TRUE` = col_elb, `FALSE` = "white"),
                      guide = "none") +
    v_final + x_scale + labs(y = "Information criterion", x = "K") + theme_e

  p_aic <- ggplot(ft, aes(x = K, y = AIC)) +
    geom_line(colour = col_line, linewidth = 0.8) +
    geom_point(colour = col_line, size = 2) +
    v_final + x_scale + labs(y = "AIC", x = "K") + theme_e

  p_ent <- ggplot(ft, aes(x = K, y = Entropy)) +
    geom_line(colour = col_line, linewidth = 0.8, na.rm = TRUE) +
    geom_point(colour = col_line, size = 2, na.rm = TRUE) +
    geom_hline(yintercept = 0.80, colour = col_elb, linetype = "dotted") +
    v_final + x_scale + labs(y = "Relative entropy (descriptive)", x = "K") +
    theme_e

  panels <- p_ic | p_aic | p_ent
  if (!is.null(blrt_table) && nrow(blrt_table) > 0) {
    blrt_plot <- blrt_table |>
      mutate(K = K1, sig_fill = !is.na(p_boot) & p_boot < .05)
    p_blrt <- ggplot(blrt_plot, aes(x = K, y = p_boot)) +
      geom_hline(yintercept = 0.05, colour = col_elb, linetype = "dashed") +
      geom_line(colour = col_line, linewidth = 0.8, na.rm = TRUE) +
      geom_point(aes(fill = sig_fill), shape = 21, size = 2.5,
                 colour = col_line, na.rm = TRUE) +
      scale_fill_manual(values = c("TRUE" = col_elb, "FALSE" = "white"),
                        guide = "none") +
      v_final + x_scale + scale_y_continuous(limits = c(0, 1)) +
      labs(y = "BLRT p (K−1 vs K)", x = "K") + theme_e
    panels <- panels | p_blrt
  }
  panels +
    plot_annotation(
      title = title,
      subtitle = paste0("Dashed blue = final K. Red-filled IC points = shortlisted ",
                        "candidates. Dotted red = entropy .80 (descriptive; not used ",
                        "for selection). BLRT: filled = p < .05."),
      theme = theme(plot.title    = element_text(size = 13, face = "bold"),
                    plot.subtitle = element_text(size = 9, colour = "#595959")))
}

# ── Export fit index table ─────────────────────────────────────────────────────
export_fit_table <- function(fit_table, sample_label, path, note = NULL) {
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
  if (!is.null(note)) {
    note_row <- nrow(fit_table) + 3L
    writeData(wb, ws, note, startRow = note_row, startCol = 1)
    addStyle(wb, ws, createStyle(fontName = "Calibri", fontSize = 10),
             rows = note_row, cols = 1)
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

# ── Pairwise class contrasts ──────────────────────────────────────────────────
# All pairwise differences between the levels of class_col (reviewer M3), from
# a coefficient vector and its covariance matrix. Works for both lm() fits and
# BCH fits (fit_bch). The first level is the reference; contrasts against it
# carry term_vs_ref so their (Holm-adjusted) p can be joined back onto the
# reference-coded coefficient table.
pairwise_contrasts <- function(coefs, V, class_col, cls_levels, df_res, sigma) {
  crit <- qt(0.975, df_res)
  dplyr::bind_rows(lapply(combn(cls_levels, 2, simplify = FALSE), function(pr) {
    term_a <- paste0(class_col, pr[1])
    term_b <- paste0(class_col, pr[2])
    L <- setNames(numeric(length(coefs)), names(coefs))
    if (term_b %in% names(L)) L[term_b] <-  1
    if (term_a %in% names(L)) L[term_a] <- -1
    est   <- sum(L * coefs)
    se    <- sqrt(as.numeric(t(L) %*% V %*% L))
    t_val <- est / se
    data.frame(
      contrast    = paste0(pr[2], " vs ", pr[1]),
      term_vs_ref = if (pr[1] == cls_levels[1]) term_b else NA_character_,
      B = est, SE = se, t = t_val,
      p = 2 * pt(abs(t_val), df_res, lower.tail = FALSE),
      CI_low = est - crit * se, CI_high = est + crit * se,
      cohens_d = est / sigma,
      stringsAsFactors = FALSE
    )
  })) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 4)))
}

# ── Holm correction (reviewer M2) ─────────────────────────────────────────────
# One family per dataset within a results list (i.e. per dataset × analysis):
# all pairwise class contrasts of both outcomes. Adds p_holm to $pairwise and,
# for contrasts against the reference, to $coef_table.
add_holm <- function(res_list) {
  ds_of <- sub("_.*", "", names(res_list))
  for (ds in unique(ds_of)) {
    nms   <- names(res_list)[ds_of == ds]
    p_adj <- p.adjust(unlist(lapply(nms, function(nm) res_list[[nm]]$pairwise$p)),
                      method = "holm")
    idx <- 0L
    for (nm in nms) {
      pw        <- res_list[[nm]]$pairwise
      pw$p_holm <- round(p_adj[idx + seq_len(nrow(pw))], 4)
      idx       <- idx + nrow(pw)
      ct        <- res_list[[nm]]$coef_table
      ct$p_holm <- pw$p_holm[match(ct$term, pw$term_vs_ref)]
      res_list[[nm]]$pairwise   <- pw
      res_list[[nm]]$coef_table <- ct
    }
  }
  res_list
}

# ── BCH three-step estimator (reviewer M5) ────────────────────────────────────
# Bolck, Croon & Hagenaars (2004); Bakk & Vermunt (2016). Corrects class
# differences for classification error in the modal assignment.
#   1. D[t, s] = P(assigned s | true class t), from the posteriors:
#        D[t, s] = Σ_i p(t | y_i) · 1(w_i = s) / Σ_i p(t | y_i)
#   2. Each person is expanded to K rows (one per class t) with weight
#        H[w_i, t], where H = D⁻¹ (weights can be negative).
#   3. Weighted least squares of the outcome on class (+ covariates) in the
#      expanded data, with person-clustered sandwich SEs.
# D is computed on the full LCA sample; the regression uses complete cases on
# outcome and covariates. Returns the same structure as extract_ols_stats(),
# with fit statistics that do not apply to BCH set to NA.
bch_error_matrix <- function(posterior, modal) {
  assign_mat <- outer(modal, seq_len(ncol(posterior)), "==") * 1
  crossprod(posterior, assign_mat) / colSums(posterior)
}

# ── BCH diagnostics export (reviewer M5) ──────────────────────────────────────
# Per dataset: classification-error matrix D (rows = true class, columns =
# assigned class; the diagonal is the probability of correct assignment), plus
# entropy and modal class sizes.
export_bch_diagnostics <- function(inputs, path) {
  sheets <- list()
  for (ds in names(inputs)) {
    inp <- inputs[[ds]]
    D   <- bch_error_matrix(inp$post, inp$modal)
    d_df <- data.frame(True_class = inp$cnames, round(D, 3), check.names = FALSE)
    names(d_df)[-1] <- paste("Assigned:", inp$cnames)
    summ <- data.frame(
      Statistic = c("N", "Entropy (relative)", "Mean max posterior",
                    paste("Modal n —", inp$cnames)),
      Value = c(nrow(inp$post), round(calc_entropy(inp$post), 3),
                round(mean(apply(inp$post, 1, max)), 3),
                tabulate(inp$modal, nbins = length(inp$cnames)))
    )
    sheets[[paste0(ds, "_D")]]       <- d_df
    sheets[[paste0(ds, "_summary")]] <- summ
  }
  writexl::write_xlsx(sheets, path = path)
  cat("BCH diagnostics exported:", path, "\n")
}
# complete_on: extra variables that must be observed (fixes the analysis sample,
#   e.g. the SEP-complete sample for unadjusted same-N models).
# extra_terms: additional model terms such as "LCA_Class:Geslacht".
fit_bch <- function(df, dep_var, posterior, modal, class_names, ref_class,
                    covariates = character(0), label,
                    class_col = "LCA_Class", complete_on = character(0),
                    extra_terms = character(0)) {
  K <- length(class_names)
  stopifnot(nrow(posterior) == nrow(df), length(modal) == nrow(df),
            ncol(posterior) == K)
  D <- bch_error_matrix(posterior, modal)
  H <- solve(D)

  keep <- complete.cases(df[, unique(c(dep_var, covariates, complete_on)),
                            drop = FALSE])
  n    <- sum(keep)
  id   <- rep(seq_len(n), each = K)
  cls_levels <- c(ref_class, setdiff(class_names, ref_class))

  exp_df <- data.frame(.y = df[[dep_var]][keep][id])
  exp_df[[class_col]] <- factor(rep(class_names, times = n), levels = cls_levels)
  for (cv in covariates) exp_df[[cv]] <- df[[cv]][keep][id]
  w <- as.vector(t(H[modal[keep], , drop = FALSE]))

  X     <- model.matrix(as.formula(paste("~", paste(c(class_col, covariates,
                                                      extra_terms),
                                                    collapse = " + "))),
                        data = exp_df)
  XtWX  <- crossprod(X, X * w)
  bread <- solve(XtWX)
  beta  <- as.vector(bread %*% crossprod(X, w * exp_df$.y))
  names(beta) <- colnames(X)
  e     <- as.vector(exp_df$.y - X %*% beta)

  score  <- rowsum(X * (w * e), id)
  p_par  <- ncol(X)
  V      <- bread %*% crossprod(score) %*% bread * n / (n - 1)
  dimnames(V) <- list(colnames(X), colnames(X))
  se     <- sqrt(diag(V))
  df_res <- n - p_par
  sigma  <- sqrt(sum(w * e^2) / df_res)
  crit   <- qt(0.975, df_res)
  t_val  <- beta / se
  p_val  <- 2 * pt(abs(t_val), df_res, lower.tail = FALSE)

  coef_df <- data.frame(term = names(beta), B = beta, SE = se, t = t_val,
                        p = p_val, CI_low = beta - crit * se,
                        CI_high = beta + crit * se,
                        stringsAsFactors = FALSE, row.names = NULL) |>
    dplyr::mutate(
      stars = dplyr::case_when(
        p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", p < .10 ~ "†", TRUE ~ ""),
      std_beta   = NA_real_,
      cohens_d   = dplyr::if_else(term == "(Intercept)", NA_real_, B / sigma),
      term_clean = dplyr::if_else(term == "(Intercept)",
                                  paste0("Intercept = ", ref_class),
                                  sub(paste0("^", class_col), "", term))
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 4)))

  list(label = label, dep_var = dep_var, class_col = class_col,
       estimator = "BCH", n = n, ref_class = ref_class,
       r2 = NA_real_, adj_r2 = NA_real_, f_stat = NA_real_,
       df_mod = NA_integer_, df_res = as.integer(df_res), p_f = NA_real_,
       sigma = sigma, aic = NA_real_, bic = NA_real_,
       rmse = sqrt(sum(w * e^2) / n), cohens_f2 = NA_real_,
       coef_table = coef_df,
       pairwise = pairwise_contrasts(beta, V, class_col, cls_levels,
                                     df_res, sigma),
       D = round(D, 3), beta = beta, V = V)
}

# ── OLS helper functions ───────────────────────────────────────────────────────
extract_ols_stats <- function(model, dep_var, label, class_col) {
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
  
  # Effect sizes are not defined for the intercept (a class mean, not a contrast)
  coef_df <- dplyr::left_join(coef_df, std_p, by = "term") |>
    dplyr::mutate(cohens_d = dplyr::if_else(term == "(Intercept)", NA_real_, B / sigma),
                  std_beta = dplyr::if_else(term == "(Intercept)", NA_real_, std_beta))
  
  cls_levels <- levels(model$model[[class_col]]); ref_class <- cls_levels[1]

  # term_clean: the intercept gets the reference class name; class terms are
  # shown by class name only (the class_col prefix is stripped).
  coef_df <- coef_df |>
    dplyr::mutate(
      term_clean = dplyr::if_else(
        term == "(Intercept)",
        paste0("Intercept = ", ref_class),
        sub(paste0("^", class_col), "", term)
      )
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 4)))

  list(label = label, dep_var = dep_var, class_col = class_col,
       estimator = "OLS (modal assignment)",
       n = n_obs, ref_class = ref_class,
       r2 = r2, adj_r2 = adj_r2, f_stat = as.numeric(f_val),
       df_mod = as.integer(df_mod), df_res = as.integer(df_res),
       p_f = as.numeric(p_f), sigma = sigma,
       aic = aic_val, bic = bic_val, rmse = rmse_val, cohens_f2 = f2_val,
       coef_table = coef_df,
       pairwise = pairwise_contrasts(coef(model), vcov(model), class_col,
                                     cls_levels, df_res, sigma),
       model = model)
}

# ── Regression residual diagnostics ───────────────────────────────────────────
# Replaces the outcome normality check (outcome normality is not an LCA
# assumption; OLS assumes normal, homoscedastic residuals). Per OLS model:
# residual skewness and excess kurtosis (|skew| < 2, |kurtosis| < 7; Kim, 2013),
# Shapiro–Wilk (n ≤ 5000), and the Koenker studentized Breusch–Pagan test
# (n·R² of the squared residuals regressed on the model's predictors). BCH
# models have no single residual vector; their modal-OLS counterparts serve as
# the check.
regression_diagnostics <- function(res_list, analysis) {
  dplyr::bind_rows(lapply(names(res_list), function(nm) {
    m <- res_list[[nm]]$model
    if (is.null(m)) return(NULL)
    e  <- residuals(m); n <- length(e)
    sw <- if (n >= 3 && n <= 5000) shapiro.test(e) else NULL
    X  <- model.matrix(m)
    e2 <- e^2
    aux_res <- lm.fit(X, e2)$residuals
    r2  <- 1 - sum(aux_res^2) / sum((e2 - mean(e2))^2)
    bp  <- n * r2; bp_df <- ncol(X) - 1L
    data.frame(
      Analysis = analysis, Model = nm, n = n,
      Resid_skew     = round(psych::skew(e), 3),
      Resid_kurtosis = round(psych::kurtosi(e), 3),
      Skew_kurt_ok   = abs(psych::skew(e)) < 2 & abs(psych::kurtosi(e)) < 7,
      SW_W = if (is.null(sw)) NA_real_ else round(unname(sw$statistic), 3),
      SW_p = if (is.null(sw)) NA_real_ else signif(sw$p.value, 3),
      BP_stat = round(bp, 2), BP_df = bp_df,
      BP_p = signif(pchisq(bp, bp_df, lower.tail = FALSE), 3),
      stringsAsFactors = FALSE
    )
  }))
}

print_ols_table <- function(ols) {
  cat("\n", strrep("═", 60), "\n")
  cat(" Model:", ols$label, "| Outcome:", ols$dep_var, "| n =", ols$n,
      "|", ols$estimator, "\n")
  cat(strrep("─", 60), "\n")
  if (!is.na(ols$r2))
    cat(sprintf(" R² = %.3f  Adj R² = %.3f  F(%d,%d) = %.3f  p = %s\n",
                ols$r2, ols$adj_r2, ols$df_mod, ols$df_res, ols$f_stat,
                fmt_p(ols$p_f)))
  cat(sprintf(" σ = %.3f  AIC = %.1f  BIC = %.1f  f² = %.3f\n",
              ols$sigma, ols$aic, ols$bic, ols$cohens_f2))
  cat(strrep("─", 60), "\n")
  ct <- ols$coef_table
  p_holm <- if ("p_holm" %in% names(ct)) ct$p_holm else rep(NA_real_, nrow(ct))
  cat(sprintf(" %-40s %8s %8s %8s %6s  %5s  %8s  %8s  %7s\n",
              "Term", "B", "SE", "t", "p", "", "95% CI lo", "95% CI hi", "p Holm"))
  for (i in seq_len(nrow(ct)))
    cat(sprintf(" %-40s %8.4f %8.4f %8.4f %6s %5s  %8.4f  %8.4f  %7s\n",
                ct$term_clean[i], ct$B[i], ct$SE[i], ct$t[i],
                fmt_p(ct$p[i]), ct$stars[i], ct$CI_low[i], ct$CI_high[i],
                fmt_p(p_holm[i])))
  pw <- ols$pairwise
  if (!is.null(pw) && nrow(pw) > 0) {
    cat(strrep("─", 60), "\n")
    cat(" Pairwise class contrasts\n")
    for (i in seq_len(nrow(pw)))
      cat(sprintf(" %-40s %8.4f %8.4f %8.4f %6s %16s  %7s\n",
                  pw$contrast[i], pw$B[i], pw$SE[i], pw$t[i], fmt_p(pw$p[i]), "",
                  if (is.null(pw$p_holm)) "" else fmt_p(pw$p_holm[i])))
  }
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
                 "Std Beta", "Cohen's d", "p (Holm)")
    writeData(wb, nm, as.data.frame(t(headers)), startRow = 1, colNames = FALSE)
    addStyle(wb, nm, sty_head, rows = 1, cols = seq_along(headers), gridExpand = TRUE)
    ct <- ols$coef_table
    for (i in seq_len(nrow(ct))) {
      r    <- i + 1
      vals <- c(ct$term_clean[i], ct$B[i], ct$SE[i], ct$t[i], fmt_p(ct$p[i]),
                ct$stars[i], ct$CI_low[i], ct$CI_high[i],
                if ("std_beta" %in% names(ct)) ct$std_beta[i] else NA,
                if ("cohens_d" %in% names(ct)) ct$cohens_d[i] else NA,
                if ("p_holm"   %in% names(ct)) fmt_p(ct$p_holm[i]) else NA)
      vals[is.na(vals)] <- ""
      writeData(wb, nm, as.data.frame(t(as.character(vals))),
                startRow = r, colNames = FALSE)
      sty <- if (i == nrow(ct)) sty_last else sty_data
      addStyle(wb, nm, sty_left, rows = r, cols = 1)
      addStyle(wb, nm, sty, rows = r, cols = 2:length(headers), gridExpand = TRUE)
    }
    
    # ── Fit statistics note ───────────────────────────────────────────────────
    fit_row <- nrow(ct) + 3L
    fit_txt <- if (is.na(ols$r2))
      sprintf("Estimator: %s  σ = %.3f  (R², F and f² not defined for BCH)",
              ols$estimator, ols$sigma)
    else sprintf(
      "Fit: R² = %.3f  Adj R² = %.3f  F(%d,%d) = %.3f  p = %s  σ = %.3f  f² = %.3f",
      ols$r2, ols$adj_r2, ols$df_mod, ols$df_res, ols$f_stat, fmt_p(ols$p_f),
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
        leg_txt <- sprintf("  %s%s",
                           cnames[k],
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
  
  # Pairwise contrasts sheet (all models; Holm within dataset × analysis)
  addWorksheet(wb, "Pairwise")
  pw_df <- dplyr::bind_rows(lapply(ols_list, function(s) {
    if (is.null(s$pairwise)) return(NULL)
    cbind(data.frame(Model = s$label, Outcome = s$dep_var,
                     stringsAsFactors = FALSE),
          dplyr::select(s$pairwise, -term_vs_ref))
  }))
  if (nrow(pw_df) > 0) {
    pw_df$p <- fmt_p(pw_df$p)
    if ("p_holm" %in% names(pw_df)) pw_df$p_holm <- fmt_p(pw_df$p_holm)
  }
  if (nrow(pw_df) > 0) {
    writeData(wb, "Pairwise", pw_df, startRow = 1)
    addStyle(wb, "Pairwise", sty_head, rows = 1, cols = seq_along(pw_df),
             gridExpand = TRUE)
    setColWidths(wb, "Pairwise", cols = 1:3, widths = c(16, 14, 30))
    setColWidths(wb, "Pairwise", cols = 4:ncol(pw_df), widths = 11)
  }

  # Model fit summary sheet
  addWorksheet(wb, "Model fit")
  fit_df <- dplyr::bind_rows(lapply(ols_list, function(s) {
    data.frame(
      Model = s$label, Dataset = sub("_.*", "", s$label), Outcome = s$dep_var,
      Estimator = s$estimator,
      n = s$n, R2 = round(s$r2, 3), Adj_R2 = round(s$adj_r2, 3),
      F_stat = round(s$f_stat, 3), df1 = s$df_mod, df2 = s$df_res,
      p_F    = if (is.na(s$p_f)) NA_character_ else
        if (s$p_f < .001) "< .001" else sprintf("%.4f", s$p_f),
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
                   " p (Holm) = Holm-adjusted across all pairwise class ",
                   "contrasts of both outcomes within each dataset."),
            startRow = note_row, startCol = 1)
  addStyle(wb, "Model fit", sty_note, rows = note_row, cols = 1:n_fc,
           gridExpand = TRUE)
  setColWidths(wb, "Model fit", cols = 1:4, widths = 16)
  setColWidths(wb, "Model fit", cols = 5:n_fc, widths = 12)
  saveWorkbook(wb, path, overwrite = TRUE)
  cat("Regression workbook exported:", path, "\n")
}

make_coef_plot <- function(ols_list, sample, title) {
  entries <- ols_list[grepl(paste0("^", sample, "_"), names(ols_list))]
  if (length(entries) == 0) { warning("No models for sample: ", sample); return(NULL) }
  dep_pretty <- c(IAS_Totaal = "IAS", IATS_Totaal = "IATS")
  plot_df <- dplyr::bind_rows(lapply(entries, function(s) {
    lbl <- if (s$dep_var %in% names(dep_pretty)) dep_pretty[[s$dep_var]] else s$dep_var
    # Class contrasts only; covariate coefficients are not plotted
    s$coef_table |> dplyr::filter(startsWith(term, s$class_col)) |>
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
# Health-only indicators: age, sex and SEP are covariates, not indicators.
#
# DA: Disease_Cat, RiskFactor_Cat, BDI_Cat, TAS_Cat
#  L: Disease_Cat, RiskFactor_Cat, IPAQ_Cat, Alcohol_Cat, Roken_Cat,
#     SSRQ_Cat, EEQ_Cat, JSS_Cat

# ── 7.2 Class enumeration and selection ──────────────────────────────────────
# enumerate_lca() runs the full, pre-specified procedure per dataset:
#   - indicator frequency check and Cramér's V screen
#   - K = 1–10, restricted to identified models (residual df > 0)
#   - 50 random starts per K; LL replication recorded per K
#   - BLRT (B = 100) as reported evidence
#   - size filter: estimated π ≥ .05 and modal n ≥ 50
#   - shortlist (≤ 3, by BIC): lowest BIC, cmP > .10, smallest K with BF > 3
#   - classification diagnostics, local independence and salsa summary for
#     the shortlisted candidates (descriptive; entropy is not a selection rule)
#   - final K: first shortlisted candidate (BIC order) whose best LL is
#     replicated across 100 starts; no manual override
# Run time: several hours per dataset (enumeration + BLRT + refits).

enum_da <- enumerate_lca(df_da_clean, lca_vars_da, burden_levels_da,
                         label = "DA", file_label = "DA", seed = 123)
enum_l  <- enumerate_lca(df_l_clean,  lca_vars_l,  burden_levels_l,
                         label = "L",  file_label = "L",  seed = 456)

best_k_da <- enum_da$final_k
best_k_l  <- enum_l$final_k
model_da  <- enum_da$model
model_l   <- enum_l$model

class_da <- model_da$predclass
class_l  <- model_l$predclass

# Posterior class probabilities (N × K), used by the BCH estimator (Section 8)
post_da <- model_da$posterior
post_l  <- model_l$posterior

cat("\nClass distribution — DA:\n"); print(table(class_da))
cat("\nClass distribution — L:\n");  print(table(class_l))

# ── 7.11 Indicator category labels ────────────────────────────────────────────
# Order must match factor level order used in prep_lca_data (as.integer)

category_labels_da <- list(
  Disease_Cat    = c("No chronic conditions", "Single chronic condition",
                     "Multimorbidity"),
  RiskFactor_Cat = c("Absent", "Present"),
  BDI_Cat        = c("Low depressive symptoms",
                     "Clinically relevant depressive symptoms"),
  TAS_Cat        = c("No alexithymia", "Alexithymia present")
)

var_labels_da <- c(
  Disease_Cat    = "Disease Status",
  RiskFactor_Cat = "Risk Factors",
  BDI_Cat        = "Depression",
  TAS_Cat        = "Alexithymia"
)

category_labels_l <- list(
  Disease_Cat    = c("No chronic conditions", "Single chronic condition",
                     "Multimorbidity"),
  RiskFactor_Cat = c("Absent", "Present"),
  IPAQ_Cat       = c("Insufficiently active", "Active"),
  Alcohol_Cat    = c("No hazardous drinking", "Hazardous drinking"),
  Roken_Cat      = c("Non-smoker", "Smoker"),
  SSRQ_Cat       = c("Below average self-regulation",
                     "Above average self-regulation"),
  EEQ_Cat        = c("Non-Emotional Eater", "Low Emotional Eater",
                     "Emotional to Very Emotional Eater"),
  JSS_Cat        = c("No disturbed sleep", "Disturbed sleep")
)

var_labels_l <- c(
  Disease_Cat    = "Disease Status",
  RiskFactor_Cat = "Risk Factors",
  IPAQ_Cat       = "Physical Activity",
  Alcohol_Cat    = "Alcohol Use (AUDIT-C)",
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
# Neutral labels (reviewer m4). enumerate_lca() orders the final classes by the
# Methods reference rule (descending P(No chronic conditions), ties by lowest
# P(risk factors present)), so Class A is always the reference class and the
# letters are stable across runs. Class characteristics are described in the
# text instead.
# Interpretation: the health-only classes separate psychosocial/behavioural
# burden profiles (DA: depression and alexithymia; L: self-regulation,
# emotional eating and sleep); multimorbidity differs between classes but does
# not define them. The composite in 7.14b is therefore a burden score.
# These names propagate to the Excel tables, profile plots, and LCA_Class factor.

class_names_da <- paste("Class", LETTERS[seq_len(best_k_da)])
class_names_l  <- paste("Class", LETTERS[seq_len(best_k_l)])

# ── 7.14a Scoring specifications ──────────────────────────────────────────────
# Variable labels must match cond_probs$Variable exactly (from var_labels_*).
# Category labels must match cond_probs$Category exactly (from category_labels_*).

scoring_spec_da <- list(
  list(variable        = "Disease Status",
       healthy_cat     = "No chronic conditions",
       burden_cat      = "Multimorbidity"),
  list(variable        = "Risk Factors",
       healthy_cat     = "Absent",
       burden_cat      = "Present"),
  list(variable        = "Depression",
       healthy_cat     = "Low depressive symptoms",
       burden_cat      = "Clinically relevant depressive symptoms"),
  list(variable        = "Alexithymia",
       healthy_cat     = "No alexithymia",
       burden_cat      = "Alexithymia present")
)

scoring_spec_l <- list(
  list(variable        = "Disease Status",
       healthy_cat     = "No chronic conditions",
       burden_cat      = "Multimorbidity"),
  list(variable        = "Risk Factors",
       healthy_cat     = "Absent",
       burden_cat      = "Present"),
  list(variable        = "Physical Activity",
       healthy_cat     = "Active",
       burden_cat      = "Insufficiently active"),
  list(variable        = "Alcohol Use (AUDIT-C)",
       healthy_cat     = "No hazardous drinking",
       burden_cat      = "Hazardous drinking"),
  list(variable        = "Smoking",
       healthy_cat     = "Non-smoker",
       burden_cat      = "Smoker"),
  list(variable        = "Self-Regulation",
       healthy_cat     = "Above average self-regulation",
       burden_cat      = "Below average self-regulation"),
  list(variable        = "Emotional Eating",
       healthy_cat     = "Non-Emotional Eater",
       burden_cat      = "Emotional to Very Emotional Eater"),
  list(variable        = "Sleep",
       healthy_cat     = "No disturbed sleep",
       burden_cat      = "Disturbed sleep")
)

# ── 7.14b Run composite class profile scoring ─────────────────────────────────
# Depends on: cond_probs_da / cond_probs_l (Section 7.12) and
#             class_names_da / class_names_l (Section 7.14).
# Outputs are descriptive (manuscript reporting only); the regression reference
# class is selected in Section 8.1 with select_reference_class().

cat("\n\u2550\u2550\u2550 Class profile scoring \u2014 DA \u2550\u2550\u2550\n")
profile_da <- select_class_profiles(cond_probs_da, class_names_da,
                                    scoring_spec_da, label = "DA")

cat("\n\u2550\u2550\u2550 Class profile scoring \u2014 L \u2550\u2550\u2550\n")
profile_l  <- select_class_profiles(cond_probs_l,  class_names_l,
                                    scoring_spec_l,  label = "L")

cat("\nProfile scoring summary:\n")
cat(sprintf("  Most healthy class     \u2014 DA : %s (HCS = %.3f)\n",
            profile_da$most_healthy,     max(profile_da$hcs)))
cat(sprintf("  Most burdened class    \u2014 DA : %s (BCS = %.3f)\n",
            profile_da$most_burdened, max(profile_da$bcs)))
cat(sprintf("  Most healthy class     \u2014 L  : %s (HCS = %.3f)\n",
            profile_l$most_healthy,      max(profile_l$hcs)))
cat(sprintf("  Most burdened class    \u2014 L  : %s (BCS = %.3f)\n",
            profile_l$most_burdened,  max(profile_l$bcs)))

# Multimorbidity class: explicit rule (highest P(Multimorbidity), ties by
# highest P(risk factors present)). This, not the BCS, defines the class the
# text may call the "multimorbidity class".
mm_class_da <- select_multimorbid_class(cond_probs_da, class_names_da, label = "DA")
mm_class_l  <- select_multimorbid_class(cond_probs_l,  class_names_l,  label = "L")

profile_selection <- data.frame(
  Dataset = c("DA", "L"),
  Reference_class_rule = c(
    select_reference_class(cond_probs_da, class_names_da, label = "DA"),
    select_reference_class(cond_probs_l,  class_names_l,  label = "L")),
  Most_healthy_HCS     = c(profile_da$most_healthy,  profile_l$most_healthy),
  Multimorbidity_class_rule = c(mm_class_da$class, mm_class_l$class),
  Most_burdened_BCS    = c(profile_da$most_burdened, profile_l$most_burdened),
  stringsAsFactors = FALSE
)
profile_selection$Healthy_rules_agree <-
  profile_selection$Reference_class_rule == profile_selection$Most_healthy_HCS
profile_selection$Burden_rules_agree <-
  profile_selection$Multimorbidity_class_rule == profile_selection$Most_burdened_BCS
print(profile_selection, row.names = FALSE)
if (!all(profile_selection$Burden_rules_agree))
  warning("Multimorbidity class (P(Multimorbidity) rule) differs from the ",
          "most burdened class (BCS) in: ",
          paste(profile_selection$Dataset[!profile_selection$Burden_rules_agree],
                collapse = ", "), ". Describe them separately in the text.")

mm_prob_table <- function(mm, ds) {
  data.frame(Dataset = ds, Class = names(mm$p_mm),
             P_Multimorbidity = round(mm$p_mm, 3),
             P_RiskFactors_Present = round(mm$p_tiebreak, 3),
             row.names = NULL)
}
writexl::write_xlsx(
  list(Selection   = profile_selection,
       MM_rule     = rbind(mm_prob_table(mm_class_da, "DA"),
                           mm_prob_table(mm_class_l,  "L")),
       DA_scores   = profile_da$score_table,
       L_scores    = profile_l$score_table,
       Note = data.frame(Note = paste0(
         "Reference class rule: highest P(No chronic conditions), ties by lowest ",
         "P(risk factors present). Multimorbidity class rule: highest ",
         "P(Multimorbidity), ties by highest P(risk factors present). ",
         "HCS/BCS = mean P(healthy/burden pole) across all indicators ",
         "(descriptive composites; HCS_/BCS_ columns are their components)."))),
  path = file.path(base_path, "LCA_class_profiles.xlsx"))
cat("Class profile selection exported.\n")

# ── 7.15 Fit index tables ─────────────────────────────────────────────────────
# Exported by enumerate_lca() in 7.2 (LCA_fitindices_*, LCA_diagnostics_*,
# LCA_decisionlog_*, LCA_elbowplot_*).

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

# ── 7.18a Class demographics (modal assignment) ───────────────────────────────
# Age, sex and SEP are no longer indicators, so they are reported per class here.
class_demo_da <- describe_class_demographics(df_da_clean, "LCA_Class", "DA")
class_demo_l  <- describe_class_demographics(df_l_clean,  "LCA_Class", "L")
cat("\n═══ Class demographics (modal class) ═══\n")
print(class_demo_da, row.names = FALSE)
print(class_demo_l,  row.names = FALSE)
writexl::write_xlsx(list(DA = class_demo_da, L = class_demo_l),
                    path = file.path(base_path, "LCA_class_demographics.xlsx"))
cat("Class demographics exported.\n")

# ── 7.19 Local independence check ─────────────────────────────────────────────
check_local_independence(df_da_clean, class_da, lca_vars_da, best_k_da, "DA")
check_local_independence(df_l_clean,  class_l,  lca_vars_l,  best_k_l,  "L")

cat("\nSection 7 complete.\n")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 8 — Regression Analyses
# ══════════════════════════════════════════════════════════════════════════════
# Models: IAS_Totaal ~ LCA_Class and IATS_Totaal ~ LCA_Class, separately for
# the DA and L samples.
#   Primary:    BCH three-step estimates (corrects for classification error;
#               reviewer M5).
#   Comparison: OLS on modal class assignment.
# Reference class = class with the highest conditional P(No chronic
# conditions); ties broken by the lowest P(Risk Factors = "Present") (Methods
# rule; reviewer M3). All pairwise class contrasts are reported as well.
# p-values are Holm-adjusted per dataset across all pairwise contrasts of both
# outcomes (reviewer M2).
# ══════════════════════════════════════════════════════════════════════════════

# ── 8.1 Identify reference classes (Methods rule) ─────────────────────────────
ref_class_da <- select_reference_class(cond_probs_da, class_names_da, label = "DA")
ref_class_l  <- select_reference_class(cond_probs_l,  class_names_l,  label = "L")
# Classes were ordered by this rule in enumerate_lca(), so the reference must be
# Class A; anything else means the ordering and the rule have diverged.
stopifnot(ref_class_da == class_names_da[1], ref_class_l == class_names_l[1])

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
# Comparison: OLS on modal class assignment
primary_models <- list(
  DA_IAS  = lm(IAS_Totaal  ~ LCA_Class, data = df_da_clean),
  DA_IATS = lm(IATS_Totaal ~ LCA_Class, data = df_da_clean),
  L_IAS   = lm(IAS_Totaal  ~ LCA_Class, data = df_l_clean),
  L_IATS  = lm(IATS_Totaal ~ LCA_Class, data = df_l_clean)
)
cat("\nFour modal-assignment OLS models fitted.\n")

dep_var_map <- list(
  DA_IAS  = "IAS_Totaal", DA_IATS = "IATS_Totaal",
  L_IAS   = "IAS_Totaal", L_IATS  = "IATS_Totaal"
)

# Primary: BCH three-step. Posteriors and modal classes come from Section 7.2
# (final, ordered model); the class order of post_* matches class_names_*.
lca_bch_inputs <- list(
  DA = list(df = df_da_clean, post = post_da, modal = class_da,
            cnames = class_names_da, ref = ref_class_da),
  L  = list(df = df_l_clean,  post = post_l,  modal = class_l,
            cnames = class_names_l,  ref = ref_class_l)
)

# complete_by_sample: variables that must be observed in each sample, to fit
# models on a fixed analysis sample (e.g. SEP-complete) regardless of the
# covariates in the model.
fit_bch_models <- function(covariates_by_sample = list(DA = character(0),
                                                       L  = character(0)),
                           complete_by_sample = list(DA = character(0),
                                                     L  = character(0)),
                           suffix = "") {
  setNames(
    lapply(names(dep_var_map), function(nm) {
      smp <- sub("_.*", "", nm)
      inp <- lca_bch_inputs[[smp]]
      fit_bch(inp$df, dep_var = dep_var_map[[nm]], posterior = inp$post,
              modal = inp$modal, class_names = inp$cnames,
              ref_class = inp$ref,
              covariates = covariates_by_sample[[smp]],
              complete_on = complete_by_sample[[smp]],
              label = paste0(nm, suffix))
    }),
    paste0(names(dep_var_map), suffix)
  )
}

# Modal-assignment OLS counterpart of fit_bch_models(), on the same samples.
fit_ols_models <- function(covariates_by_sample = list(DA = character(0),
                                                       L  = character(0)),
                           complete_by_sample = list(DA = character(0),
                                                     L  = character(0)),
                           suffix = "") {
  setNames(
    lapply(names(dep_var_map), function(nm) {
      smp  <- sub("_.*", "", nm)
      df   <- lca_bch_inputs[[smp]]$df
      dv   <- dep_var_map[[nm]]
      covs <- covariates_by_sample[[smp]]
      keep <- complete.cases(df[, unique(c(dv, covs, complete_by_sample[[smp]])),
                                drop = FALSE])
      f    <- as.formula(paste(dv, "~", paste(c("LCA_Class", covs), collapse = " + ")))
      extract_ols_stats(lm(f, data = df[keep, ]), dep_var = dv,
                        label = paste0(nm, suffix), class_col = "LCA_Class")
    }),
    paste0(names(dep_var_map), suffix)
  )
}

no_covariates_by_sample <- list(DA = character(0), L = character(0))

bch_list <- fit_bch_models()
cat("Four BCH models fitted.\n")
cat("\nClassification error matrix D (rows = true class, cols = assigned) — DA:\n")
print(bch_list$DA_IAS$D)
cat("Classification error matrix D — L:\n")
print(bch_list$L_IAS$D)
export_bch_diagnostics(lca_bch_inputs,
                       file.path(base_path, "BCH_diagnostics.xlsx"))

# ── 8.4 Extract and print results ─────────────────────────────────────────────
ols_list <- setNames(
  lapply(names(primary_models), function(nm) {
    extract_ols_stats(primary_models[[nm]], dep_var = dep_var_map[[nm]],
                      label = nm, class_col = "LCA_Class")
  }),
  names(primary_models)
)

# Holm correction per dataset (each estimator is its own analysis)
bch_list <- add_holm(bch_list)
ols_list <- add_holm(ols_list)

cat("\n═══ Primary: BCH three-step ═══\n")
for (nm in names(bch_list)) print_ols_table(bch_list[[nm]])
cat("\n═══ Comparison: OLS on modal class assignment ═══\n")
for (nm in names(ols_list)) print_ols_table(ols_list[[nm]])

# ── 8.5 Export to Excel ───────────────────────────────────────────────────────
primary_label <- paste0(
  "Primary analysis: health-only LCA (chronic conditions, composite risk ",
  "factors and sample-specific health indicators). ",
  "Model: outcome ~ LCA_Class; reference = class with the highest ",
  "P(No chronic conditions), ties broken by lowest P(risk factors present)."
)

export_ols_workbook(
  ols_list        = bch_list,
  path            = file.path(base_path, "primary_regression_results_BCH.xlsx"),
  analysis_label  = paste0(primary_label,
                           " Estimator: BCH three-step with person-clustered ",
                           "sandwich SEs; Cohen's d = B / σ."),
  class_term      = "LCA_Class",
  class_names_map = list(DA = class_names_da, L = class_names_l)
)

export_ols_workbook(
  ols_list        = ols_list,
  path            = file.path(base_path, "primary_regression_results_modalOLS.xlsx"),
  analysis_label  = paste0(primary_label,
                           " Estimator: OLS on modal class assignment."),
  class_term      = "LCA_Class",
  class_names_map = list(DA = class_names_da, L = class_names_l)
)

# ── 8.6 Coefficient plots ─────────────────────────────────────────────────────
p_coef_da <- make_coef_plot(
  bch_list, "DA",
  "Regression coefficients by LCA class (BCH) — DA dataset"
)
p_coef_l  <- make_coef_plot(
  bch_list, "L",
  "Regression coefficients by LCA class (BCH) — L dataset"
)

ggsave(file.path(base_path, "primary_regression_coef_DA.png"),
       plot = p_coef_da, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(base_path, "primary_regression_coef_L.png"),
       plot = p_coef_l,  width = 9, height = 5.5, dpi = 300)
cat("Coefficient plots saved.\n")

cat("\nSection 8 complete.\n")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 9 — Sensitivity Analyses: Covariate-Adjusted Regressions
# ══════════════════════════════════════════════════════════════════════════════
# Same structure as Section 8 (same LCA classes and reference class chosen by
# the Methods rule), with covariates added to the right-hand side.
# Model: outcome ~ LCA_Class + covariate_1 + covariate_2 + ...
#   Primary:    BCH three-step (covariates enter the step-3 regression)
#   Comparison: OLS on modal class assignment (also used for VIF diagnostics)
#
# The LCA_Class coefficients now represent the adjusted mean difference in
# IAS/IATS between each class and the reference class, at equal levels of
# the covariates. Comparing these to the unadjusted coefficients from
# Section 8 shows whether class differences are attenuated (i.e. partially
# explained) by the covariates. Age, sex and SEP are not LCA indicators, so
# adjusting for them does not adjust for class-defining variables; BDI_Totaal
# (DA) is the exception, because BDI_Cat is a DA indicator.
#
# Reported models:
#   Main:          DA adjusted for age, sex, SEP (no BDI); L for age, sex, SEP.
#   Same-N:        unadjusted models on the same SEP-complete sample, so the
#                  change from unadjusted to adjusted is not a sample change.
#   Supplementary: DA additionally adjusted for BDI_Totaal. Under BCH this model
#                  is unstable (BDI_Cat defines the DA classes).
# ══════════════════════════════════════════════════════════════════════════════

# ── 9.1 Define covariates ────────────────────────────────────────────────────
# EDIT THESE VECTORS to control which variables are adjusted for.
# Each element must be a variable name present in the corresponding
# clean dataset (df_da_clean or df_l_clean).
# Leave empty — c() — to skip the sensitivity analysis for that sample.

sensitivity_covariates_da <- c("Leeftijd", "Geslacht", "SEP_3Cat", "BDI_Totaal")
sensitivity_covariates_l  <- c("Leeftijd", "Geslacht", "SEP_3Cat")

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
  
  # ── 9.5 Multicollinearity diagnostics ────────────────────────────────────
  # Checks whether any predictor in the covariate-adjusted models is
  # collinear enough to distort its coefficient or inflate its SE.
  #
  # Method: car::vif() is used throughout because it handles both
  #   • single-df predictors   → returns VIF  (= GVIF when Df = 1)
  #   • multi-df predictors    → returns GVIF and GVIF^(1/(2*Df)), the
  #     scale-comparable generalisation (Fox & Monette 1992)
  #
  # Thresholds applied to the *adjusted* GVIF^(1/(2*Df)) column
  # (equals sqrt(VIF) for single-df terms; directly comparable across Df):
  #   GVIF^(1/(2*Df)) > sqrt(5)  ≈ 2.24  →  moderate concern (FLAG)
  #   GVIF^(1/(2*Df)) > sqrt(10) ≈ 3.16  →  serious concern  (WARN)
  #
  # Supplementary check: matrix condition number κ of the model-matrix
  # column space. κ > 30 suggests near-collinearity; κ > 100 is severe.
  #
  # Note on LCA_Class: as a K-1 dummy-coded factor it contributes K-1
  # columns to the design matrix. car::vif() correctly computes its GVIF
  # as a single-row entry and adjusts for Df, so the LCA_Class GVIF is
  # directly comparable with scalar covariate VIFs.
  
  # ── Helper: compute and print VIF / GVIF for one model ───────────────────
  check_vif <- function(model, model_label,
                        adj_threshold_moderate = sqrt(5),
                        adj_threshold_severe   = sqrt(10)) {
    
    cat("\n", strrep("─", 65), "\n", sep = "")
    cat(" Multicollinearity —", model_label, "\n")
    cat(strrep("─", 65), "\n", sep = "")
    
    # car::vif() returns a vector (single-df only) or a 3-column matrix
    vif_raw <- tryCatch(
      car::vif(model),
      error = function(e) {
        cat("  car::vif() failed:", e$message,
            "\n  (Model may have only one predictor — VIF not applicable.)\n")
        return(NULL)
      }
    )
    
    if (is.null(vif_raw)) return(invisible(NULL))
    
    # Normalise to a data.frame with columns: Term, GVIF, Df, GVIF_adj
    if (is.matrix(vif_raw)) {
      vif_df <- data.frame(
        Term     = rownames(vif_raw),
        GVIF     = round(vif_raw[, "GVIF"],              3),
        Df       = as.integer(vif_raw[, "Df"]),
        GVIF_adj = round(vif_raw[, "GVIF^(1/(2*Df))"],  3),
        stringsAsFactors = FALSE
      )
    } else {
      # Vector: each element is VIF for a single-df term
      vif_df <- data.frame(
        Term     = names(vif_raw),
        GVIF     = round(as.numeric(vif_raw),              3),
        Df       = 1L,
        GVIF_adj = round(sqrt(as.numeric(vif_raw)),        3),
        stringsAsFactors = FALSE
      )
    }
    
    # Flag column
    vif_df$Flag <- dplyr::case_when(
      vif_df$GVIF_adj > adj_threshold_severe   ~ "WARN  (GVIF_adj > 3.16)",
      vif_df$GVIF_adj > adj_threshold_moderate ~ "FLAG  (GVIF_adj > 2.24)",
      TRUE                                      ~ "OK"
    )
    
    print(vif_df, row.names = FALSE)
    
    n_flags <- sum(vif_df$Flag != "OK")
    if (n_flags == 0) {
      cat("\n  ✓ No multicollinearity concerns (all GVIF_adj ≤ 2.24).\n")
    } else {
      n_warn <- sum(grepl("^WARN", vif_df$Flag))
      n_flag <- sum(grepl("^FLAG", vif_df$Flag))
      if (n_warn > 0)
        cat("\n  ✗ SERIOUS — ", n_warn,
            " term(s) with GVIF_adj > 3.16. ",
            "Consider removing or combining collinear predictors.\n",
            sep = "")
      if (n_flag > 0)
        cat("\n  △ MODERATE — ", n_flag,
            " term(s) with GVIF_adj > 2.24. ",
            "Inspect correlations; coefficient SEs may be inflated.\n",
            sep = "")
    }
    
    # ── Condition number of the model matrix ─────────────────────────────────
    mm <- tryCatch(model.matrix(model), error = function(e) NULL)
    if (!is.null(mm)) {
      # Drop the intercept column before computing kappa
      mm_no_int <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
      if (ncol(mm_no_int) >= 2) {
        kappa_val <- round(kappa(mm_no_int, exact = FALSE), 1)
        kappa_flag <- dplyr::case_when(
          kappa_val > 100 ~ "WARN  (κ > 100: severe near-collinearity)",
          kappa_val >  30 ~ "FLAG  (κ > 30: potential near-collinearity)",
          TRUE            ~ "OK"
        )
        cat(sprintf("\n  Condition number κ = %.1f  %s\n",
                    kappa_val, kappa_flag))
      }
    }
    
    cat(strrep("─", 65), "\n", sep = "")
    invisible(vif_df)
  }
  
  # ── Helper: pairwise Pearson / Cramér's V correlation table for covariates ─
  # Continuous–continuous: Pearson r.
  # Continuous–categorical and categorical–categorical: Cramér's V.
  # Helps trace the source of any flagged VIF.
  
  check_covariate_correlations <- function(df, covariates, label,
                                           r_threshold = 0.70,
                                           v_threshold = 0.50) {
    
    if (length(covariates) < 2) {
      cat("  Fewer than 2 covariates — pairwise correlation skipped.\n")
      return(invisible(NULL))
    }
    
    cat("\n Covariate pairwise associations —", label, "\n")
    
    rows <- list()
    pairs <- combn(covariates, 2, simplify = FALSE)
    
    for (p in pairs) {
      x <- df[[p[1]]]; y <- df[[p[2]]]
      x_cont <- is.numeric(x) || is.integer(x)
      y_cont <- is.numeric(y) || is.integer(y)
      
      if (x_cont && y_cont) {
        stat  <- round(cor(as.numeric(x), as.numeric(y), use = "complete.obs"), 3)
        type  <- "Pearson r"
        flag  <- if (abs(stat) > r_threshold) paste0("FLAG (|r| > ", r_threshold, ")")
        else "OK"
      } else {
        # Use Cramér's V (defined earlier in script)
        stat  <- round(cramers_v(as.factor(x), as.factor(y)), 3)
        type  <- "Cramér's V"
        flag  <- if (!is.na(stat) && stat > v_threshold)
          paste0("FLAG (V > ", v_threshold, ")")
        else "OK"
      }
      
      rows[[length(rows) + 1]] <- data.frame(
        Var1 = p[1], Var2 = p[2], Statistic = type,
        Value = stat, Flag = flag,
        stringsAsFactors = FALSE
      )
    }
    
    corr_tbl <- dplyr::bind_rows(rows)
    print(corr_tbl, row.names = FALSE)
    
    n_flagged <- sum(corr_tbl$Flag != "OK")
    if (n_flagged == 0)
      cat("  ✓ No high pairwise associations among covariates.\n")
    else
      cat("  △", n_flagged,
          "pair(s) exceed the association threshold — these may be driving elevated VIF.\n")
    
    invisible(corr_tbl)
  }
  
  # ── Run multicollinearity checks for all fitted sensitivity models ─────────
  cat("\n\n══════════════════════════════════════════════════════════════════\n")
  cat(" SECTION 9.5 — Multicollinearity Diagnostics (Sensitivity Models)\n")
  cat("══════════════════════════════════════════════════════════════════\n")
  
  # VIF for the main models (DA without BDI_Totaal) and, labelled _BDI, for the
  # supplementary DA models that include BDI_Totaal. All on the same sample.
  vif_models <- sens_models
  da_in <- intersect(c("DA_IAS", "DA_IATS"), names(sens_models))
  if (length(da_in) > 0 && "BDI_Totaal" %in% sensitivity_covariates_da) {
    names(vif_models)[match(da_in, names(vif_models))] <- paste0(da_in, "_BDI")
    da_keep <- complete.cases(df_da_clean[, c("IAS_Totaal", "IATS_Totaal",
                                              sensitivity_covariates_da)])
    cov_nobdi <- setdiff(sensitivity_covariates_da, "BDI_Totaal")
    for (nm in da_in) {
      dv <- if (nm == "DA_IAS") "IAS_Totaal" else "IATS_Totaal"
      vif_models[[nm]] <- lm(build_sens_formula(dv, cov_nobdi),
                             data = df_da_clean[da_keep, ])
    }
    vif_models <- vif_models[c(da_in, paste0(da_in, "_BDI"),
                               setdiff(names(vif_models),
                                       c(da_in, paste0(da_in, "_BDI"))))]
  }

  vif_results <- list()

  for (nm in names(vif_models)) {
    vif_results[[nm]] <- check_vif(vif_models[[nm]], model_label = nm)
  }
  
  # ── Pairwise covariate associations (once per sample, not per model) ───────
  if (run_da_sens && length(sensitivity_covariates_da) >= 2) {
    check_covariate_correlations(
      df_da_clean, sensitivity_covariates_da, label = "DA covariates"
    )
  }
  
  if (run_l_sens && length(sensitivity_covariates_l) >= 2) {
    check_covariate_correlations(
      df_l_clean, sensitivity_covariates_l, label = "L covariates"
    )
  }
  
  # ── Compact VIF summary table across all sensitivity models ───────────────
  # Useful for a single-glance appendix entry.
  
  vif_summary <- dplyr::bind_rows(
    lapply(names(vif_results), function(nm) {
      v <- vif_results[[nm]]
      if (is.null(v)) return(NULL)
      v$Model <- nm
      v[, c("Model", "Term", "GVIF", "Df", "GVIF_adj", "Flag")]
    })
  )
  
  if (nrow(vif_summary) > 0) {
    cat("\n═══ VIF / GVIF summary — all sensitivity models ═══\n")
    print(vif_summary, row.names = FALSE)
    
    # Export VIF summary to the sensitivity Excel workbook sheet
    # (appended after models are exported in 9.7)
    .vif_export_pending <- TRUE   # flag consumed in 9.7
  }
  
  cat("\nSection 9.5 multicollinearity diagnostics complete.\n")
  
  # ── 9.6 Fit, extract and print results ─────────────────────────────────────
  # All Section 9 models use one analysis sample per dataset: participants with
  # all sensitivity covariates observed (SEP-complete).
  sens_samples   <- c(if (run_da_sens) "DA", if (run_l_sens) "L")
  sens_keep      <- function(lst) lst[sub("_.*", "", names(lst)) %in% sens_samples]
  sens_complete  <- list(DA = sensitivity_covariates_da, L = sensitivity_covariates_l)
  sens_covariates_da_nobdi <- setdiff(sensitivity_covariates_da, "BDI_Totaal")
  sens_cov_main  <- list(DA = sens_covariates_da_nobdi, L = sensitivity_covariates_l)

  # Main: DA without BDI_Totaal, L age + sex + SEP
  sens_bch_list <- add_holm(sens_keep(fit_bch_models(sens_cov_main, sens_complete)))
  sens_ols_list <- add_holm(sens_keep(fit_ols_models(sens_cov_main, sens_complete)))

  # Same-N unadjusted: same sample as the adjusted models
  sens_bch_unadj_list <- add_holm(sens_keep(
    fit_bch_models(no_covariates_by_sample, sens_complete, suffix = "_unadj_sameN")))
  sens_ols_unadj_list <- add_holm(sens_keep(
    fit_ols_models(no_covariates_by_sample, sens_complete, suffix = "_unadj_sameN")))

  # Supplementary: DA additionally adjusted for BDI_Totaal. BDI_Cat is a DA
  # indicator and the higher-burden DA class is largely defined by depression,
  # so under the BCH weights BDI_Totaal and class are near-collinear and the
  # BCH estimates are unstable (very large SEs).
  sens_bch_bdi_list <- list(); sens_ols_bdi_list <- list()
  if (run_da_sens && "BDI_Totaal" %in% sensitivity_covariates_da) {
    bdi_cov <- list(DA = sensitivity_covariates_da, L = character(0))
    sens_bch_bdi_list <- add_holm(fit_bch_models(bdi_cov, sens_complete,
                                                 suffix = "_BDI")[c("DA_IAS_BDI", "DA_IATS_BDI")])
    sens_ols_bdi_list <- add_holm(fit_ols_models(bdi_cov, sens_complete,
                                                 suffix = "_BDI")[c("DA_IAS_BDI", "DA_IATS_BDI")])
  }

  cat("\n═══ Sensitivity (main): BCH three-step, covariate-adjusted ═══\n")
  for (nm in names(sens_bch_list)) print_ols_table(sens_bch_list[[nm]])
  cat("\n═══ Sensitivity (main): OLS on modal class assignment ═══\n")
  for (nm in names(sens_ols_list)) print_ols_table(sens_ols_list[[nm]])
  cat("\n═══ Sensitivity: unadjusted on the same sample (BCH, OLS) ═══\n")
  for (nm in names(sens_bch_unadj_list)) print_ols_table(sens_bch_unadj_list[[nm]])
  for (nm in names(sens_ols_unadj_list)) print_ols_table(sens_ols_unadj_list[[nm]])
  cat("\n═══ Sensitivity (supplementary): DA + BDI_Totaal ═══\n")
  cat("NOTE: the BCH estimates are unstable (BDI_Cat defines the DA classes).\n")
  for (nm in names(sens_bch_bdi_list)) print_ols_table(sens_bch_bdi_list[[nm]])
  for (nm in names(sens_ols_bdi_list)) print_ols_table(sens_ols_bdi_list[[nm]])

  # ── 9.7 Export to Excel ────────────────────────────────────────────────────
  covariates_note <- paste0(
    "Sensitivity analysis: outcome ~ LCA_Class + covariates. Main models — DA: ",
    paste(sens_covariates_da_nobdi, collapse = ", "), "; L: ",
    paste(sensitivity_covariates_l,  collapse = ", "), ". ",
    "*_unadj_sameN: unadjusted, on the same sample as the adjusted models. ",
    "DA_*_BDI (supplementary): DA additionally adjusted for BDI_Totaal. ",
    "All models: participants with all of ",
    paste(sensitivity_covariates_da, collapse = ", "), " (DA) / ",
    paste(sensitivity_covariates_l, collapse = ", "), " (L) observed. ",
    "Reference = class with the highest P(No chronic conditions), ties broken ",
    "by lowest P(risk factors present). Holm families: main, same-N and ",
    "supplementary models separately."
  )

  export_ols_workbook(
    ols_list        = c(sens_bch_list, sens_bch_unadj_list, sens_bch_bdi_list),
    path            = file.path(base_path, "sensitivity_regression_results_BCH.xlsx"),
    analysis_label  = paste0(covariates_note,
                             " Estimator: BCH three-step with person-clustered ",
                             "sandwich SEs. The DA_*_BDI BCH models are unstable ",
                             "because BDI_Cat defines the DA classes."),
    class_term      = "LCA_Class",
    class_names_map = list(DA = class_names_da, L = class_names_l)
  )

  export_ols_workbook(
    ols_list        = c(sens_ols_list, sens_ols_unadj_list, sens_ols_bdi_list),
    path            = file.path(base_path, "sensitivity_regression_results_modalOLS.xlsx"),
    analysis_label  = paste0(covariates_note,
                             " Estimator: OLS on modal class assignment."),
    class_term      = "LCA_Class",
    class_names_map = list(DA = class_names_da, L = class_names_l)
  )
  
  # Append VIF summary as an extra sheet to the sensitivity workbook
  if (exists(".vif_export_pending") && .vif_export_pending &&
      nrow(vif_summary) > 0) {
    tryCatch({
      wb_vif <- openxlsx::loadWorkbook(
        file.path(base_path, "sensitivity_regression_results_modalOLS.xlsx")
      )
      sty_vh <- openxlsx::createStyle(
        fontName = "Calibri", fontSize = 11, textDecoration = "bold",
        border = c("top", "bottom"), borderStyle = "thin", halign = "center"
      )
      sty_vd <- openxlsx::createStyle(
        fontName = "Calibri", fontSize = 11, halign = "center"
      )
      sty_vl <- openxlsx::createStyle(
        fontName = "Calibri", fontSize = 11, halign = "center",
        border = "bottom", borderStyle = "thin"
      )
      sty_vn <- openxlsx::createStyle(
        fontName = "Calibri", fontSize = 10, wrapText = TRUE
      )
      openxlsx::addWorksheet(wb_vif, "VIF diagnostics")
      openxlsx::writeData(
        wb_vif, "VIF diagnostics",
        as.data.frame(t(names(vif_summary))),
        startRow = 1, colNames = FALSE
      )
      openxlsx::addStyle(wb_vif, "VIF diagnostics", sty_vh,
                         rows = 1, cols = seq_along(vif_summary),
                         gridExpand = TRUE)
      n_vr <- nrow(vif_summary)
      for (i in seq_len(n_vr)) {
        sty_row <- if (i == n_vr) sty_vl else sty_vd
        openxlsx::writeData(
          wb_vif, "VIF diagnostics",
          as.data.frame(t(as.character(unlist(vif_summary[i, ])))),
          startRow = i + 1, colNames = FALSE
        )
        openxlsx::addStyle(wb_vif, "VIF diagnostics", sty_row,
                           rows = i + 1, cols = seq_along(vif_summary),
                           gridExpand = TRUE)
      }
      note_row_vif <- n_vr + 3L
      openxlsx::writeData(
        wb_vif, "VIF diagnostics",
        paste0(
          "Note. GVIF_adj = GVIF^(1/(2*Df)); comparable across single- and ",
          "multi-df predictors (Fox & Monette 1992). ",
          "Thresholds: GVIF_adj > 2.24 (= sqrt(5)) = moderate concern (FLAG); ",
          "> 3.16 (= sqrt(10)) = serious concern (WARN). ",
          "Condition number κ computed on the model matrix excluding the intercept."
        ),
        startRow = note_row_vif, startCol = 1
      )
      openxlsx::addStyle(wb_vif, "VIF diagnostics", sty_vn,
                         rows = note_row_vif,
                         cols = seq_along(vif_summary), gridExpand = TRUE)
      openxlsx::setColWidths(wb_vif, "VIF diagnostics",
                             cols = 1:2, widths = 28)
      openxlsx::setColWidths(wb_vif, "VIF diagnostics",
                             cols = 3:6, widths = 14)
      openxlsx::saveWorkbook(wb_vif,
                             file.path(base_path,
                                       "sensitivity_regression_results_modalOLS.xlsx"),
                             overwrite = TRUE)
      cat("VIF diagnostics sheet appended to sensitivity_regression_results_modalOLS.xlsx\n")
    }, error = function(e) {
      warning("Could not append VIF sheet: ", e$message)
    })
  }
  
  # ── 9.8 Coefficient plots ─────────────────────────────────────────────────
  if (run_da_sens) {
    p_sens_da <- make_coef_plot(
      sens_bch_list, "DA",
      paste0("Sensitivity: regression coefficients by LCA class (BCH) — DA\n",
             "Adjusted for: ", paste(sens_covariates_da_nobdi, collapse = ", "))
    )
    ggsave(file.path(base_path, "sensitivity_regression_coef_DA.png"),
           plot = p_sens_da, width = 9, height = 5.5, dpi = 300)

    if (length(sens_bch_bdi_list) > 0) {
      p_sens_da_bdi <- make_coef_plot(
        sens_bch_bdi_list, "DA",
        paste0("Supplementary: regression coefficients by LCA class (BCH) — DA\n",
               "Adjusted for: ", paste(sensitivity_covariates_da, collapse = ", "),
               " (unstable: BDI_Cat defines the DA classes)")
      )
      ggsave(file.path(base_path, "sensitivity_regression_coef_DA_BDI_suppl.png"),
             plot = p_sens_da_bdi, width = 9, height = 5.5, dpi = 300)
    }
  }
  
  if (run_l_sens) {
    p_sens_l <- make_coef_plot(
      sens_bch_list, "L",
      paste0("Sensitivity: regression coefficients by LCA class (BCH) — L\n",
             "Adjusted for: ", paste(sensitivity_covariates_l, collapse = ", "))
    )
    ggsave(file.path(base_path, "sensitivity_regression_coef_L.png"),
           plot = p_sens_l, width = 9, height = 5.5, dpi = 300)
  }
  
  cat("Sensitivity coefficient plots saved.\n")
}

cat("\nSection 9 complete.\n")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 10 — Direct Comparison by Disease Status (reviewer M1, M8)
# ══════════════════════════════════════════════════════════════════════════════
# IAS/IATS compared directly between participants with no, one, or multiple
# chronic conditions, independent of the LCA. OLS; reference = No Disease.
#   10.2 Primary definition (Disease_Cat)
#   10.3 Alternative definition (Disease_Cat_Alt)
#        DA: conditions + risk factors + depression; L: conditions + risk factors
# Per definition:
#   _unadj        unadjusted, full sample
#   _unadj_sameN  unadjusted, on the adjusted-model sample (all Section 9
#                 covariates observed), so unadjusted vs adjusted is not a
#                 change of sample
#   _adj          adjusted for age, sex and SEP (the main Section 9 set; BDI is
#                 not part of the main adjustment set)
#   _adj_BDI      DA only, primary definition: additionally adjusted for
#                 BDI_Totaal (supplementary). Not fitted for the alternative
#                 definition, which counts clinically relevant depression.
# Holm correction per dataset within each of these families.
# ══════════════════════════════════════════════════════════════════════════════

# ── 10.1 Helper ───────────────────────────────────────────────────────────────
fit_disease_models <- function(disease_col, covariates_by_sample, suffix,
                               complete_by_sample = list(DA = character(0),
                                                         L  = character(0)),
                               samples = c("DA", "L")) {
  data_by_sample <- list(DA = df_da_clean, L = df_l_clean)
  res <- list()
  for (smp in samples) {
    df <- data_by_sample[[smp]]
    for (dv in c("IAS_Totaal", "IATS_Totaal")) {
      nm   <- paste0(smp, "_", sub("_Totaal", "", dv), suffix)
      covs <- covariates_by_sample[[smp]]
      keep <- complete.cases(df[, unique(c(dv, covs, complete_by_sample[[smp]])),
                                drop = FALSE])
      f    <- as.formula(paste(dv, "~", paste(c(disease_col, covs),
                                              collapse = " + ")))
      res[[nm]] <- extract_ols_stats(lm(f, data = df[keep, ]),
                                     dep_var = dv, label = nm,
                                     class_col = disease_col)
    }
  }
  add_holm(res)
}

no_covariates    <- list(DA = character(0), L = character(0))
disease_cov_main <- list(DA = setdiff(sensitivity_covariates_da, "BDI_Totaal"),
                         L  = setdiff(sensitivity_covariates_l,  "BDI_Totaal"))
disease_complete <- list(DA = sensitivity_covariates_da, L = sensitivity_covariates_l)

# ── 10.2 Primary definition ───────────────────────────────────────────────────
disease_unadj       <- fit_disease_models("Disease_Cat", no_covariates, "_unadj")
disease_unadj_sameN <- fit_disease_models("Disease_Cat", no_covariates,
                                          "_unadj_sameN", disease_complete)
disease_adj         <- fit_disease_models("Disease_Cat", disease_cov_main,
                                          "_adj", disease_complete)
disease_adj_bdi     <- fit_disease_models("Disease_Cat",
                                          list(DA = sensitivity_covariates_da),
                                          "_adj_BDI", disease_complete,
                                          samples = "DA")

cat("\n═══ Disease status (primary definition) ═══\n")
for (res in c(disease_unadj, disease_unadj_sameN, disease_adj, disease_adj_bdi))
  print_ols_table(res)

export_ols_workbook(
  ols_list        = c(disease_unadj, disease_unadj_sameN, disease_adj,
                      disease_adj_bdi),
  path            = file.path(base_path, "disease_status_regression_results.xlsx"),
  analysis_label  = paste0(
    "Direct comparison by disease status (No Disease / One Disease / ",
    "Multimorbid; chronic conditions only). Model: outcome ~ Disease_Cat ",
    "(+ covariates). _unadj: unadjusted, full sample. _unadj_sameN: ",
    "unadjusted, on the adjusted-model sample. _adj: adjusted for DA ",
    paste(disease_cov_main$DA, collapse = ", "), "; L ",
    paste(disease_cov_main$L, collapse = ", "), ". DA_*_adj_BDI ",
    "(supplementary): additionally adjusted for BDI_Totaal. Reference = No ",
    "Disease. Estimator: OLS. Holm families: each model set separately."
  ),
  class_term      = "Disease_Cat"
)

# ── 10.3 Alternative definition ───────────────────────────────────────────────
disease_alt_unadj       <- fit_disease_models("Disease_Cat_Alt", no_covariates,
                                              "_alt_unadj")
disease_alt_unadj_sameN <- fit_disease_models("Disease_Cat_Alt", no_covariates,
                                              "_alt_unadj_sameN", disease_complete)
disease_alt_adj         <- fit_disease_models("Disease_Cat_Alt", disease_cov_main,
                                              "_alt_adj", disease_complete)

cat("\n═══ Disease status (alternative definition) ═══\n")
for (res in c(disease_alt_unadj, disease_alt_unadj_sameN, disease_alt_adj))
  print_ols_table(res)

export_ols_workbook(
  ols_list        = c(disease_alt_unadj, disease_alt_unadj_sameN, disease_alt_adj),
  path            = file.path(base_path, "disease_status_alt_regression_results.xlsx"),
  analysis_label  = paste0(
    "Direct comparison by alternative disease status (DA: chronic conditions ",
    "+ risk factors + clinically relevant depressive symptoms; L: chronic ",
    "conditions + risk factors). Model: outcome ~ Disease_Cat_Alt ",
    "(+ covariates). _alt_unadj: unadjusted, full sample. _alt_unadj_sameN: ",
    "unadjusted, on the adjusted-model sample. _alt_adj: adjusted for DA ",
    paste(disease_cov_main$DA, collapse = ", "), "; L ",
    paste(disease_cov_main$L, collapse = ", "), ". Reference = No Disease. ",
    "Estimator: OLS. Holm families: each model set separately."
  ),
  class_term      = "Disease_Cat_Alt"
)

cat("\nSection 10 complete.\n")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 11 — Joint Analysis of Interoceptive Accuracy and Attention (M6)
# ══════════════════════════════════════════════════════════════════════════════
# 11.1 IAS–IATS correlation within each LCA class (overall: Section 6.5)
# 11.2 Multivariate linear model cbind(IAS, IATS) ~ LCA_Class (modal classes):
#      tests whether classes differ on accuracy and attention jointly
#      (Pillai's trace), with the residual IAS–IATS correlation.
# Modal assignment only: no off-the-shelf BCH equivalent of Pillai's test, so
# this analysis ignores classification error (noted in the export).
# ══════════════════════════════════════════════════════════════════════════════

# ── 11.1 Correlation per class ────────────────────────────────────────────────
cor_by_class <- bind_rows(
  lapply(levels(df_da_clean$LCA_Class), function(k)
    cor_ias_iats(df_da_clean[df_da_clean$LCA_Class == k, ], "DA", k)),
  lapply(levels(df_l_clean$LCA_Class), function(k)
    cor_ias_iats(df_l_clean[df_l_clean$LCA_Class == k, ], "L", k))
)
cor_all <- bind_rows(cor_overall, cor_by_class)
cat("\n═══ IAS–IATS correlation: overall and per LCA class ═══\n")
print(cor_all, row.names = FALSE)

# ── 11.2 Multivariate linear model ────────────────────────────────────────────
fit_joint <- function(df, label) {
  mlm <- lm(cbind(IAS_Totaal, IATS_Totaal) ~ LCA_Class, data = df)
  av  <- anova(mlm, test = "Pillai")
  data.frame(Dataset    = label,
             n          = nrow(residuals(mlm)),
             Pillai     = round(av["LCA_Class", "Pillai"], 3),
             approx_F   = round(av["LCA_Class", "approx F"], 3),
             num_df     = av["LCA_Class", "num Df"],
             den_df     = av["LCA_Class", "den Df"],
             p          = signif(av["LCA_Class", "Pr(>F)"], 3),
             residual_r = round(cor(residuals(mlm))[1, 2], 3),
             stringsAsFactors = FALSE)
}

joint_tbl <- bind_rows(fit_joint(df_da_clean, "DA"),
                       fit_joint(df_l_clean,  "L"))
cat("\n═══ Multivariate test: cbind(IAS, IATS) ~ LCA_Class ═══\n")
print(joint_tbl, row.names = FALSE)

writexl::write_xlsx(
  list(Correlations = cor_all,
       Multivariate = joint_tbl,
       Note = data.frame(Note = paste0(
         "Correlations: Pearson r between IAS and IATS total scores, overall ",
         "and within modal LCA classes. Multivariate: Pillai's trace for ",
         "LCA_Class in lm(cbind(IAS, IATS) ~ LCA_Class); residual_r = ",
         "IAS–IATS correlation of the model residuals. Both analyses use modal ",
         "class assignment and are not corrected for classification error ",
         "(unlike the BCH estimates in Sections 8–9); state this in the Methods."
       ))),
  path = file.path(base_path, "joint_accuracy_attention.xlsx")
)
cat("Joint analysis exported.\n")

cat("\nSection 11 complete.\n")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 12 — Sensitivity: LCA with Alternative Multimorbidity Definition (M8)
# ══════════════════════════════════════════════════════════════════════════════
# L dataset only. Disease_Cat_Alt (chronic conditions + risk factors) replaces
# Disease_Cat, and RiskFactor_Cat is dropped because Disease_Cat_Alt already
# counts the risk factors. Same pipeline as Sections 7–8, including BLRT.
# DA is not re-run: its alternative definition also absorbs BDI_Cat, leaving
# Disease_Cat_Alt + TAS_Cat (6 response patterns), which cannot identify K ≥ 2.
# DA uses the direct disease-status comparison in Section 10.3 instead.
# ══════════════════════════════════════════════════════════════════════════════

# ── 12.1 Class enumeration and selection ─────────────────────────────────────
# Same pre-specified procedure as Section 7.2 (enumerate_lca()).
enum_l_alt <- enumerate_lca(df_l_clean, lca_vars_l_alt, burden_levels_l_alt,
                            label = "L (alternative)", file_label = "L_alt",
                            seed = 789,
                            order_var = "Disease_Cat_Alt",
                            tiebreak_var = "Disease_Cat_Alt",
                            tiebreak_level = 3L)   # lowest P(Multimorbidity)

best_k_l_alt <- enum_l_alt$final_k
model_l_alt  <- enum_l_alt$model
class_l_alt  <- model_l_alt$predclass
post_l_alt   <- model_l_alt$posterior
cat("\nClass distribution — L (alternative):\n"); print(table(class_l_alt))

# ── 12.2 Labels, conditional probabilities and exports ────────────────────────
category_labels_l_alt <- c(
  list(Disease_Cat_Alt = c("No chronic conditions", "Single chronic condition",
                           "Multimorbidity")),
  category_labels_l[setdiff(lca_vars_l_alt, "Disease_Cat_Alt")]
)
var_labels_l_alt <- c(
  Disease_Cat_Alt = "Disease Status (incl. risk factors)",
  var_labels_l[setdiff(lca_vars_l_alt, "Disease_Cat_Alt")]
)

cond_probs_l_alt  <- extract_cond_probs(model_l_alt, lca_vars_l_alt,
                                        category_labels_l_alt, var_labels_l_alt,
                                        best_k_l_alt)
class_info_l_alt  <- build_class_info(model_l_alt, class_l_alt, best_k_l_alt)
class_names_l_alt <- paste("Class", LETTERS[seq_len(best_k_l_alt)])

export_lca_table(class_info_l_alt, cond_probs_l_alt, class_names_l_alt,
                 best_k_l_alt, "L alt",
                 file.path(base_path, "LCA_classtable_L_alt.xlsx"))

p_profile_l_alt <- make_profile_plot(cond_probs_l_alt, class_info_l_alt,
                                     class_names_l_alt, best_k_l_alt,
                                     var_labels_l_alt)
ggsave(file.path(base_path, "LCA_profileplot_L_alt.png"), plot = p_profile_l_alt,
       width = max(10, nrow(cond_probs_l_alt) * 0.45 + 7.5), height = 6, dpi = 300)

# ── 12.3 Reference class, class factor and local independence ─────────────────
# Methods rule; with no separate risk-factor indicator, ties are broken by the
# lowest P(Multimorbidity).
ref_class_l_alt <- select_reference_class(
  cond_probs_l_alt, class_names_l_alt,
  disease_var  = "Disease Status (incl. risk factors)",
  tiebreak_var = "Disease Status (incl. risk factors)",
  tiebreak_cat = "Multimorbidity",
  label        = "L (alternative)"
)

df_l_clean$LCA_Class_Alt <- relevel(
  factor(class_l_alt, levels = 1:best_k_l_alt, labels = class_names_l_alt),
  ref = ref_class_l_alt
)
cat("Distribution — L (alternative):\n"); print(table(df_l_clean$LCA_Class_Alt))

check_local_independence(df_l_clean, class_l_alt, lca_vars_l_alt,
                         best_k_l_alt, "L (alternative)")

# ── 12.4 Regressions: BCH (primary) and modal OLS ─────────────────────────────
export_bch_diagnostics(
  list(L_alt = list(post = post_l_alt, modal = class_l_alt,
                    cnames = class_names_l_alt)),
  file.path(base_path, "BCH_diagnostics_L_alt.xlsx")
)

alt_bch_list <- add_holm(list(
  L_IAS_alt  = fit_bch(df_l_clean, "IAS_Totaal",  post_l_alt, class_l_alt,
                       class_names_l_alt, ref_class_l_alt,
                       label = "L_IAS_alt",  class_col = "LCA_Class_Alt"),
  L_IATS_alt = fit_bch(df_l_clean, "IATS_Totaal", post_l_alt, class_l_alt,
                       class_names_l_alt, ref_class_l_alt,
                       label = "L_IATS_alt", class_col = "LCA_Class_Alt")
))

alt_ols_list <- add_holm(list(
  L_IAS_alt  = extract_ols_stats(lm(IAS_Totaal  ~ LCA_Class_Alt, data = df_l_clean),
                                 dep_var = "IAS_Totaal",  label = "L_IAS_alt",
                                 class_col = "LCA_Class_Alt"),
  L_IATS_alt = extract_ols_stats(lm(IATS_Totaal ~ LCA_Class_Alt, data = df_l_clean),
                                 dep_var = "IATS_Totaal", label = "L_IATS_alt",
                                 class_col = "LCA_Class_Alt")
))

cat("\n═══ Alternative-definition LCA: BCH three-step ═══\n")
for (nm in names(alt_bch_list)) print_ols_table(alt_bch_list[[nm]])
cat("\n═══ Alternative-definition LCA: OLS on modal class assignment ═══\n")
for (nm in names(alt_ols_list)) print_ols_table(alt_ols_list[[nm]])

alt_label <- paste0(
  "Sensitivity analysis (L): LCA with alternative multimorbidity definition ",
  "(chronic conditions + risk factors; RiskFactor_Cat dropped as indicator). ",
  "Model: outcome ~ LCA_Class_Alt; reference = class with the highest ",
  "P(No chronic conditions), ties broken by lowest P(Multimorbidity)."
)

export_ols_workbook(
  ols_list        = alt_bch_list,
  path            = file.path(base_path, "alt_LCA_regression_results_BCH.xlsx"),
  analysis_label  = paste0(alt_label, " Estimator: BCH three-step with ",
                           "person-clustered sandwich SEs."),
  class_term      = "LCA_Class_Alt",
  class_names_map = list(L = class_names_l_alt)
)
export_ols_workbook(
  ols_list        = alt_ols_list,
  path            = file.path(base_path, "alt_LCA_regression_results_modalOLS.xlsx"),
  analysis_label  = paste0(alt_label, " Estimator: OLS on modal class assignment."),
  class_term      = "LCA_Class_Alt",
  class_names_map = list(L = class_names_l_alt)
)

cat("\nSection 12 complete.\n")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 13 — Covariate Adjustment Strategies and Sex Differences (reviewer M4)
# ══════════════════════════════════════════════════════════════════════════════
# 13.1 Stepwise adjustment: step 0 unadjusted → +age → +sex → +SEP (→ +BDI, DA)
# 13.2 Separate adjustment: one covariate at a time
#      13.1–13.2 use the Section 9 analysis sample (all sensitivity covariates
#      observed), so every step and every covariate is compared on the same N.
# 13.3 Sex: class × sex counts (modal and posterior-weighted); models fitted
#      separately for men and women (BCH with the classification-error matrix
#      computed within each stratum; modal OLS); class × sex interaction
#      (BCH Wald test on the interaction terms; OLS nested F test). Sex models
#      do not adjust for SEP and use the full sample.
# Estimators: BCH three-step (primary) and OLS on modal assignment (comparison).
# Holm: one family per model specification (both datasets, both outcomes).
# BCH models that include BDI_Totaal (DA) are unstable because BDI_Cat defines
# the DA classes; they are reported but flagged.
# ══════════════════════════════════════════════════════════════════════════════

m4_complete <- list(DA = sensitivity_covariates_da, L = sensitivity_covariates_l)

run_m4_spec <- function(cov_by_sample, tag, samples = c("DA", "L")) {
  cov_by_sample <- modifyList(no_covariates_by_sample, cov_by_sample)
  bch <- fit_bch_models(cov_by_sample, m4_complete, suffix = paste0("_", tag))
  ols <- fit_ols_models(cov_by_sample, m4_complete, suffix = paste0("_", tag))
  keep <- sub("_.*", "", names(bch)) %in% samples
  list(bch = add_holm(bch[keep]), ols = add_holm(ols[keep]),
       covariates = cov_by_sample)
}

# Long tables of class contrasts and of all coefficients for one result list
m4_contrasts <- function(res_list, analysis, spec, estimator, covariates) {
  dplyr::bind_rows(lapply(res_list, function(r) {
    ds <- sub("_.*", "", r$label)
    cbind(data.frame(Analysis = analysis, Specification = spec,
                     Estimator = estimator, Dataset = ds, Outcome = r$dep_var,
                     Covariates = paste(covariates[[ds]], collapse = ", "),
                     n = r$n, stringsAsFactors = FALSE),
          dplyr::select(r$pairwise, -term_vs_ref))
  }))
}
m4_coefs <- function(res_list, analysis, spec, estimator) {
  dplyr::bind_rows(lapply(res_list, function(r)
    cbind(data.frame(Analysis = analysis, Specification = spec,
                     Estimator = estimator, Model = r$label, n = r$n,
                     stringsAsFactors = FALSE),
          dplyr::select(r$coef_table, term_clean, B, SE, t, p, CI_low, CI_high,
                        dplyr::any_of(c("std_beta", "cohens_d", "p_holm"))))))
}

# ── 13.1 Stepwise adjustment ──────────────────────────────────────────────────
m4_step_specs <- list(
  S0_unadj = list(cov = list(DA = character(0), L = character(0)),
                  samples = c("DA", "L")),
  S1_age   = list(cov = list(DA = "Leeftijd", L = "Leeftijd"),
                  samples = c("DA", "L")),
  S2_sex   = list(cov = list(DA = c("Leeftijd", "Geslacht"),
                             L  = c("Leeftijd", "Geslacht")),
                  samples = c("DA", "L")),
  S3_SEP   = list(cov = list(DA = c("Leeftijd", "Geslacht", "SEP_3Cat"),
                             L  = c("Leeftijd", "Geslacht", "SEP_3Cat")),
                  samples = c("DA", "L")),
  S4_BDI   = list(cov = list(DA = c("Leeftijd", "Geslacht", "SEP_3Cat", "BDI_Totaal")),
                  samples = "DA")
)
m4_step <- lapply(names(m4_step_specs), function(tag)
  run_m4_spec(m4_step_specs[[tag]]$cov, tag, m4_step_specs[[tag]]$samples))
names(m4_step) <- names(m4_step_specs)

# ── 13.2 Separate adjustment ──────────────────────────────────────────────────
m4_sep_specs <- list(
  sep_age = list(cov = list(DA = "Leeftijd", L = "Leeftijd"), samples = c("DA", "L")),
  sep_sex = list(cov = list(DA = "Geslacht", L = "Geslacht"), samples = c("DA", "L")),
  sep_SEP = list(cov = list(DA = "SEP_3Cat", L = "SEP_3Cat"), samples = c("DA", "L")),
  sep_BDI = list(cov = list(DA = "BDI_Totaal"),               samples = "DA")
)
m4_sep <- lapply(names(m4_sep_specs), function(tag)
  run_m4_spec(m4_sep_specs[[tag]]$cov, tag, m4_sep_specs[[tag]]$samples))
names(m4_sep) <- names(m4_sep_specs)

cat("\n═══ M4: stepwise and separate adjustment (BCH) ═══\n")
for (spec in c(m4_step, m4_sep))
  for (nm in names(spec$bch)) print_ols_table(spec$bch[[nm]])

# ── 13.3 Sex differences ──────────────────────────────────────────────────────
# (a) Class × sex counts: modal assignment and posterior-weighted (expected)
class_by_sex <- dplyr::bind_rows(lapply(names(lca_bch_inputs), function(ds) {
  inp <- lca_bch_inputs[[ds]]
  ok  <- !is.na(inp$df$Geslacht)
  sex <- droplevels(inp$df$Geslacht[ok])
  modal_tab <- table(Sex = sex, Class = factor(inp$cnames[inp$modal[ok]],
                                               levels = inp$cnames))
  exp_tab   <- rowsum(inp$post[ok, , drop = FALSE], sex)
  dplyr::bind_rows(lapply(levels(sex), function(s) data.frame(
    Dataset = ds, Sex = s, Class = inp$cnames,
    Modal_n = as.integer(modal_tab[s, ]),
    Modal_pct_within_sex = round(100 * as.numeric(modal_tab[s, ]) /
                                   sum(modal_tab[s, ]), 1),
    Expected_n = round(as.numeric(exp_tab[s, ]), 1),
    Expected_pct_within_sex = round(100 * as.numeric(exp_tab[s, ]) /
                                      sum(exp_tab[s, ]), 1),
    stringsAsFactors = FALSE)))
}))
cat("\n═══ M4: class × sex ═══\n")
print(class_by_sex, row.names = FALSE)

# (b) Sex-stratified models. Strata in which any class has fewer than
# m4_min_stratum_n modally assigned members are skipped (BCH weights unstable).
m4_min_stratum_n <- 20
m4_strata <- list()
for (sx in levels(df_da_clean$Geslacht)) {
  bch_s <- list(); ols_s <- list()
  for (nm in names(dep_var_map)) {
    ds  <- sub("_.*", "", nm)
    inp <- lca_bch_inputs[[ds]]
    idx <- which(inp$df$Geslacht == sx)
    lbl <- paste0(nm, "_", sx)
    n_k <- tabulate(inp$modal[idx], nbins = length(inp$cnames))
    if (any(n_k < m4_min_stratum_n)) {
      warning("[", lbl, "] stratum skipped: modal class sizes ",
              paste(n_k, collapse = "/"), " (< ", m4_min_stratum_n, ").")
      next
    }
    res_b <- tryCatch(
      fit_bch(inp$df[idx, ], dep_var_map[[nm]], inp$post[idx, , drop = FALSE],
              inp$modal[idx], inp$cnames, inp$ref, label = lbl),
      error = function(e) { warning("[", lbl, "] BCH failed: ", e$message); NULL })
    if (!is.null(res_b)) bch_s[[lbl]] <- res_b
    ols_s[[lbl]] <- extract_ols_stats(
      lm(as.formula(paste(dep_var_map[[nm]], "~ LCA_Class")), data = inp$df[idx, ]),
      dep_var = dep_var_map[[nm]], label = lbl, class_col = "LCA_Class")
  }
  m4_strata[[sx]] <- list(bch = add_holm(bch_s), ols = add_holm(ols_s),
                          covariates = no_covariates_by_sample)
}
cat("\n═══ M4: sex-stratified models (BCH) ═══\n")
for (st in m4_strata) for (nm in names(st$bch)) print_ols_table(st$bch[[nm]])

# (c) Class × sex interaction. Class terms = contrasts among men (reference
# sex); LCA_Class:GeslachtFemale = how much each contrast differs in women.
m4_int <- lapply(names(dep_var_map), function(nm) {
  ds  <- sub("_.*", "", nm)
  inp <- lca_bch_inputs[[ds]]
  dv  <- dep_var_map[[nm]]
  rb  <- fit_bch(inp$df, dv, inp$post, inp$modal, inp$cnames, inp$ref,
                 covariates = "Geslacht", extra_terms = "LCA_Class:Geslacht",
                 label = paste0(nm, "_int"))
  d   <- inp$df[complete.cases(inp$df[, c(dv, "Geslacht")]), ]
  m0  <- lm(as.formula(paste(dv, "~ LCA_Class + Geslacht")), data = d)
  m1  <- lm(as.formula(paste(dv, "~ LCA_Class * Geslacht")), data = d)
  list(nm = nm, ds = ds, dv = dv, bch = rb, m0 = m0, m1 = m1, n_ols = nrow(d))
})

m4_interaction <- dplyr::bind_rows(lapply(m4_int, function(x) {
  ix   <- grep(":", names(x$bch$beta))
  b    <- x$bch$beta[ix]
  wald <- as.numeric(t(b) %*% solve(x$bch$V[ix, ix, drop = FALSE]) %*% b)
  df1  <- length(ix)
  av   <- anova(x$m0, x$m1)
  data.frame(
    Dataset = x$ds, Outcome = x$dv,
    Estimator = c("BCH (Wald)", "OLS modal (nested F)"),
    n   = c(x$bch$n, x$n_ols),
    df1 = c(df1, av$Df[2]),
    df2 = c(x$bch$df_res, av$Res.Df[2]),
    F   = round(c(wald / df1, av$F[2]), 3),
    p   = signif(c(pf(wald / df1, df1, x$bch$df_res, lower.tail = FALSE),
                   av[["Pr(>F)"]][2]), 3),
    stringsAsFactors = FALSE)
}))
m4_interaction$p_holm <- NA_real_
for (est in unique(m4_interaction$Estimator)) {
  i <- m4_interaction$Estimator == est
  m4_interaction$p_holm[i] <- signif(p.adjust(m4_interaction$p[i], "holm"), 3)
}
cat("\n═══ M4: class × sex interaction tests ═══\n")
print(m4_interaction, row.names = FALSE)

m4_int_coefs <- dplyr::bind_rows(lapply(m4_int, function(x) {
  ct <- x$bch$coef_table
  rbind(
    data.frame(Model = x$nm, Estimator = "BCH", term = ct$term, B = ct$B,
               SE = ct$SE, p = ct$p, CI_low = ct$CI_low, CI_high = ct$CI_high),
    broom::tidy(x$m1, conf.int = TRUE) |>
      dplyr::transmute(Model = x$nm, Estimator = "OLS modal", term,
                       B = round(estimate, 4), SE = round(std.error, 4),
                       p = round(p.value, 4), CI_low = round(conf.low, 4),
                       CI_high = round(conf.high, 4)))
}))

# ── 13.4 Export ───────────────────────────────────────────────────────────────
m4_collect <- function(spec_list, analysis, what) {
  dplyr::bind_rows(lapply(names(spec_list), function(tag) {
    sp <- spec_list[[tag]]
    if (what == "contrasts")
      dplyr::bind_rows(
        m4_contrasts(sp$bch, analysis, tag, "BCH",       sp$covariates),
        m4_contrasts(sp$ols, analysis, tag, "OLS modal", sp$covariates))
    else
      dplyr::bind_rows(m4_coefs(sp$bch, analysis, tag, "BCH"),
                       m4_coefs(sp$ols, analysis, tag, "OLS modal"))
  }))
}
m4_contrast_tbl <- dplyr::bind_rows(
  m4_collect(m4_step,   "Stepwise",       "contrasts"),
  m4_collect(m4_sep,    "Separate",       "contrasts"),
  m4_collect(m4_strata, "Sex-stratified", "contrasts"))
m4_coef_tbl <- dplyr::bind_rows(
  m4_collect(m4_step,   "Stepwise",       "coefs"),
  m4_collect(m4_sep,    "Separate",       "coefs"),
  m4_collect(m4_strata, "Sex-stratified", "coefs"))
m4_contrast_tbl$p_display      <- fmt_p(m4_contrast_tbl$p)
m4_contrast_tbl$p_holm_display <- fmt_p(m4_contrast_tbl$p_holm)
m4_interaction$p_display       <- fmt_p(m4_interaction$p)

writexl::write_xlsx(
  list(Class_contrasts   = m4_contrast_tbl,
       Coefficients      = m4_coef_tbl,
       Class_by_sex      = class_by_sex,
       Interaction_tests = m4_interaction,
       Interaction_coefs = m4_int_coefs,
       Note = data.frame(Note = c(
         paste0("Stepwise (S0-S4) and separate (sep_*) models use the Section 9 ",
                "sample (all of ", paste(sensitivity_covariates_da, collapse = ", "),
                " observed in DA; ", paste(sensitivity_covariates_l, collapse = ", "),
                " in L). S0_unadj = unadjusted on that sample."),
         paste0("Sex-stratified and interaction models use the full sample (no SEP ",
                "adjustment). Stratified BCH computes the classification-error ",
                "matrix within each sex; strata with any modal class n < ",
                m4_min_stratum_n, " are skipped."),
         paste0("Interaction tests: BCH = Wald test of all LCA_Class:Geslacht terms ",
                "(F = Wald/df1); OLS = nested F test vs the additive model. ",
                "p_holm: Holm across the four tests per estimator."),
         paste0("Holm (p_holm in Class_contrasts): one family per specification ",
                "and estimator, across datasets and outcomes."),
         paste0("BCH models including BDI_Totaal (S4_BDI, sep_BDI) are unstable ",
                "because BDI_Cat defines the DA classes.")))),
  path = file.path(base_path, "M4_covariate_adjustment.xlsx"))
cat("M4 results exported.\n")

cat("\nSection 13 complete.\n")

# ── Regression residual diagnostics (all OLS models) ──────────────────────────
reg_diag <- dplyr::bind_rows(
  regression_diagnostics(ols_list, "Primary (modal OLS)"),
  if (exists("sens_ols_list")) regression_diagnostics(sens_ols_list,
                                                      "Sensitivity (modal OLS)"),
  if (exists("sens_ols_bdi_list")) regression_diagnostics(sens_ols_bdi_list,
                                                          "Sensitivity + BDI (modal OLS)"),
  regression_diagnostics(do.call(c, lapply(c(m4_step, m4_sep), `[[`, "ols")),
                         "M4 stepwise/separate (modal OLS)"),
  regression_diagnostics(do.call(c, lapply(m4_strata, `[[`, "ols")),
                         "M4 sex-stratified (modal OLS)"),
  regression_diagnostics(c(disease_unadj, disease_unadj_sameN, disease_adj,
                           disease_adj_bdi), "Disease status"),
  regression_diagnostics(c(disease_alt_unadj, disease_alt_unadj_sameN,
                           disease_alt_adj), "Disease status (alternative)"),
  regression_diagnostics(alt_ols_list, "Alternative LCA (modal OLS)")
)
cat("\n═══ Regression residual diagnostics ═══\n")
print(reg_diag, row.names = FALSE)
writexl::write_xlsx(
  list(Diagnostics = reg_diag,
       Note = data.frame(Note = paste0(
         "Residual skewness/kurtosis (acceptable: |skew| < 2, |kurtosis| < 7; ",
         "Kim, 2013), Shapiro–Wilk, and Koenker studentized Breusch–Pagan test ",
         "for heteroscedasticity. BCH models are checked via their modal-OLS ",
         "counterparts. With large n, SW/BP p-values flag small deviations; ",
         "judge them together with skewness/kurtosis."))),
  path = file.path(base_path, "regression_diagnostics.xlsx"))

# ── Session information (reproducibility) ─────────────────────────────────────
writeLines(capture.output(sessionInfo()),
           file.path(base_path, "sessionInfo.txt"))

cat("\n═══ ANALYSIS COMPLETE ═══\n")
cat("All outputs saved to:", base_path, "\n")