# Salmon farming scoping review: corpus audit

This is Stage 1 of the coding workflow. It parses the 12,074 screened records, audits the bibliographic data, and inspects the hierarchical dictionary. It does **not** classify topics yet.

## 1. Create an RStudio project folder

Unzip this folder somewhere convenient. Copy these two input files into `data_raw/` without renaming them:

- `INCLUDES fixed abstracts.txt`
- `Salmon scoping review keywords - dictionary final.csv`

The supplied zip does not duplicate the large input files.

## 2. Open RStudio

Open the folder as an RStudio project, or set the working directory to the unzipped folder.

Run:

```r
source("scripts/00_setup.R")
```

Restart R if requested, then run:

```r
source("scripts/01_audit_corpus.R")
```

## 3. Outputs

The script writes the following to `outputs/stage_1_audit/`:

- `clean_records.csv`: one row per parsed record
- `authors_long.csv`: one row per record-author pairing
- `corpus_audit.xlsx`: summary tables, anomalies, duplicates and dictionary audit
- `dictionary_clean.csv`: dictionary with standardised column names and hierarchy IDs
- `abstract_length_distribution.png`
- `records_by_year.png`
- `session_info.txt`

## Important interpretation

A record can be included even when its title or abstract does not explicitly name the core farmed species. Species assignment will therefore include an `unspecified eligible salmon/rainbow trout` state and a manual-review flag. We will not infer Atlantic salmon solely from geography.

Core farmed species will later be restricted to Atlantic salmon, eligible Pacific salmon species, and rainbow trout/steelhead. Other trout and char species will be retained only as supplementary biological entities, never as the main exposure/intervention.

## Documentation

The annotation workflow is described in the following documents:

- `docs/annotation_workflow.md` — complete description of the automated annotation workflow.
- `docs/validation_report.md` — validation procedures and performance.
- `docs/manuscript_methods.md` — manuscript-ready methods text.