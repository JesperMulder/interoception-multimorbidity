# data/

This folder holds the raw input data. Git excludes the data (see `.gitignore`)
to protect participant confidentiality.

---

## Expected files

Put the two SPSS data files in `data/raw/` before running `LCA_Analyses.R`:

|File|Description|
|-|-|
|`Part1_19082024_clean_JM_version2.sav`|DA sample: Dutch adults (cross-sectional survey)|
|`Part2_19082024_clean_JM_version2.sav`|L sample: Dutch adults (cross-sectional survey)|

Section 1 of `LCA_Analyses.R` sets the file paths. Edit them there if your
files have different names or locations.

---

## Data access

The raw data are not publicly available. They contain sensitive health and
demographic information collected under ethics approval. You can request access
from the corresponding author (j.mulder6@lumc.nl), subject to institutional
data-sharing agreements. The Data Availability Statement in the published
manuscript has the full details.

---

## Folder structure

```
data/
├── README.md        # this file (tracked by Git)
└── raw/             # put the .sav files here (excluded from Git)
    ├── Part1_19082024_clean_JM_version2.sav
    └── Part2_19082024_clean_JM_version2.sav
```
