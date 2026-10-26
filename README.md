# Food affordability and the household burden of health spending in Türkiye

Analysis code, written analysis plan and aggregated outputs for the manuscript

> Yılmaz S, Günal AM. *Food affordability and the household burden of health spending in Türkiye.* Submitted to **Foods** (MDPI), Special Issue "Food Security, Nutrition, and Sustainability in Low- and Middle-Income Countries", 2026.

The study uses the 2023, 2024 and 2025 cross-sections of the Income and Living Conditions Survey (ILCS; the Turkish implementation of EU-SILC) of the Turkish Statistical Institute (TurkStat). The 2025 cross-section (25,835 households; 57,899 adults aged 18 years and older) carries the EU-SILC health module (financial burden of medical care, medicines and dental care; fruit and vegetable frequency) and a TurkStat item on meat, chicken or fish frequency.

## What is here and what is not

**Included**

| Folder | Content |
|---|---|
| `Codes/` | `analiz.R` (data preparation, validation checks, all models, bootstrap, tables), `simulasyon.R` (plasmode simulation), `lca.py` (latent class analysis with StepMix, BCH three-step), `figurler.py` and `figur_stili.py` (all figures), `xlsx_simge.py` (table export helper) |
| `AnalysisPlan/` | `analysis_plan.md`: the analysis plan written before model fitting (hypotheses, estimands, variable definitions, covariate tiers and selection rule, twelve-step model-building protocol, analysis stages, pre-specified sensitivity analyses, simulation design), translated into English for deposit, with the departures from the plan listed in its last section |
| `Results/` | Aggregated outputs: main tables (`Table1` to `Table3`), machine-readable results (`*.csv`, `sayilar.json`, `tablolar_tumu.xlsx`), covariate selection and protocol records, validation checks against published TurkStat and Eurostat figures, the data behind every figure (`figur_verileri/`), `sessionInfo.txt`, `python_ortam.txt`, `paket_atiflari.txt` |
| `Results/Supplementary/` | Supplementary tables as exported by the pipeline (`TableS1` to `TableS17`; numbering of the pipeline files differs from the numbering in the published Supplementary Material, see the mapping below) |
| `Figures/` | Main figures (`Figure1` to `Figure5`) and `Figures/Supplementary/` (`FigureS1` to `FigureS5`) with captions (`figur_altyazilari.md`) |

**Not included**

The ILCS microdata are owned by TurkStat and are released to researchers under a microdata user agreement; under Statistical Law No. 5429 (Articles 13 and 14) they cannot be transferred to third parties or reproduced. No microdata, no unit-level derived file and no bootstrap replicate file is in this repository. Researchers can obtain the microdata through TurkStat's microdata access procedure (https://www.tuik.gov.tr). All numbers in this repository are survey-weighted aggregates or unweighted counts of cells with at least the sample sizes reported in the paper.

## How to reproduce

1. Obtain the 2023, 2024 and 2025 ILCS cross-sectional microdata sets from TurkStat and place the workbook as described in `Codes/README_kodlar.md`.
2. R 4.6.1 with the packages listed in `Results/sessionInfo.txt`; Python 3.13 with the packages in `Results/python_ortam.txt` (StepMix 3.0.0, NumPy, SciPy, pandas, Matplotlib, pyarrow, python-docx, Pillow).
3. Run `Rscript Codes/analiz.R` from the project root. The script reads the source workbook once (then a Parquet copy), runs the validation checks, builds the analytic data sets, fits all models with the household bootstrap (replicates are cached in `R_Data ve Python Data/onbellek/`, which is not part of this repository), calls `Codes/lca.py` and `Codes/simulasyon.R` when their outputs are absent, writes all tables and `Results/sayilar.json`, and calls `Codes/xlsx_simge.py` for the workbook. Then run `python Codes/figurler.py`; the figures are drawn from `R_Data ve Python Data/figur_verileri/*.csv` (a copy is in `Results/figur_verileri/`) and recompute nothing. Parallel processing uses `future::plan(multisession, workers = 16)`. Seeds: bootstrap multiplier matrices 20251025, 20251026 (NUTS-2 pseudo-strata) and 20251027 (additional 2,000 replicates); simulation scenario seeds 20251101 + 1000 × scenario index. A clean run on an 18-core machine took about 51 minutes (latent class analysis 16 minutes, simulation 12 minutes).

## Mapping between pipeline file names and the published Supplementary Material

| Published | Pipeline file | Published | Pipeline file |
|---|---|---|---|
| Table S1 | TableS8 | Table S12 | TableS14 |
| Table S2 | TableS12 | Table S13 | TableS3 |
| Table S3 | TableS2 | Table S14 | (built from `lca_madde_olasiliklari.csv`, `lca_siniflama.csv`) |
| Table S4 | TableS7 | Table S15 | TableS4b |
| Table S5 | (software table) | Table S16 | TableS11 |
| Table S6 | TableS15 | Table S17 | TableS6 (Hypothesis 1 rows) |
| Table S7 | TableS1 | Table S18 | TableS17 |
| Table S8 | Table2 (main folder) | Table S19 | TableS4 |
| Table S9 | TableS6 (Hypothesis 2 rows) | Table S20 | TableS5 |
| Table S10 | TableS13 | Table S21 | TableS10 |
| Table S11 | TableS16 | Table S22 | TableS9 |
| Figure S1 | FigureS4 | Figure S4 | Figure5 (main folder) |
| Figure S2 | FigureS5 | Figure S5 | FigureS1 |
| Figure S3 | FigureS2 | Figure S6 | FigureS3 |

Main text: Table 1 = `Table1`, Table 2 = `Table3`; Figures 1 to 4 = `Figure1` to `Figure4`.

## Source acknowledgement

Source of the data: Turkish Statistical Institute (TurkStat), Income and Living Conditions Survey, 2023, 2024 and 2025 cross-sectional microdata sets. The authors thank TurkStat for providing access under its microdata user agreement.

## Licenses

Code (`Codes/`) is released under the MIT License (see `LICENSE`). Text, tables, figures and other non-code outputs are released under the Creative Commons Attribution 4.0 International License (CC BY 4.0).

## Authors

- Salim Yılmaz (corresponding author), Department of Healthcare Management, Faculty of Health Sciences, and Graduate School of Health Sciences, Acıbadem Mehmet Ali Aydınlar University, Istanbul, Türkiye. ORCID: 0000-0003-2405-5084. Email: salim.yilmaz@acibadem.edu.tr
- Ahmet Murat Günal, Department of Nutrition and Dietetics, Faculty of Health Sciences, Haliç University, Istanbul, Türkiye. ORCID: 0000-0001-9109-1080

## How to cite this repository

See `CITATION.cff`. The Zenodo DOI of the archived release is shown in the badge above once the release is published.
