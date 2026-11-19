# data/

This folder holds the raw input data for the analysis. Its contents are
excluded from version control (see `.gitignore`) to protect participant
confidentiality.

---

## Expected files

Place the two SPSS data files in `data/raw/` before running `LCA_Analyses.R`:

|File|Description|
|-|-|
|`Part1_19082024_clean_JM_version2.sav`|DA sample — Dutch adults (cross-sectional survey)|
|`Part2_19082024_clean_JM_version2.sav`|L sample — Dutch adults (cross-sectional survey)|

The file paths are set at the top of `LCA_Analyses.R` (Section 1) and can be
changed there if your files have different names or locations.

---

## Data access

The raw data are not publicly available. They contain sensitive health and
demographic information collected under ethics approval. Access may be
requested from the corresponding author ([insert email]) subject to
institutional data-sharing agreements.

See the **Data Availability Statement** in the published manuscript for full
details.

---

## Folder structure

```
data/
├── README.md        ← this file (version-controlled)
└── raw/             ← place .sav files here (excluded from Git)
    ├── Part1_19082024_clean_JM_version2.sav
    └── Part2_19082024_clean_JM_version2.sav
```
