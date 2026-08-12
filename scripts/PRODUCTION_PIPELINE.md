# Canonical production pipeline

This document identifies the scripts that constitute the current reproducible workflow. Numbered experimental/superseded scripts remain in the repository until their dependencies have been audited and archived; they are not part of the production workflow merely because they remain in `scripts/`.

## Frozen historical master

Baseline: `data_current/salmon_evidence_map.csv` (12,043 records; archived baseline dated 2026-08-11).

The historical master was assembled by `scripts/07_build_historical_master.R` from the validated historical annotation outputs. The historical annotation work is retained for provenance.

## Living update workflow

1. `scripts/09_start_update.R` — initialise an update and archive the current master/incoming files.
2. Existing relevance/retraction workflow — `63_screen_new_search_results.R`, `64_llm_screen_uncertain_records.R`, `65_remove_retractions_and_notices.R`, and `66_rescreen_previous_llm_uncertain_v2.R` as applicable to the incoming batch.
3. Species annotation — `02_run_species_annotation.R`, with `03_validate_species_assignments.R`, `03b_validate_species_filters.R`, and `04_build_species_dataset.R` where applicable.
4. Combined species/geography adjudication — `05_llm_adjudicate_species_geography.R` followed by `06_validate_species_geography_adjudication.R`.
5. Geography — use the validated final geography outputs/pipeline established during historical validation; do not treat scripts 15–41 as separate production workflows. Those files are historical development lineage pending archival.
6. Topics — use the validated Topic V4 workflow and outputs; scripts for earlier topic generations are historical development lineage.
7. Final assembly — append only genuinely new, validated records to the archived previous master and promote the candidate master only after validation.

## Canonical data locations

- Current master: `data_current/`
- Incoming searches: `data_updates/incoming/`
- Archived masters and update artefacts: `data_archive/`
- Generated outputs: `outputs/`

## Versioning rule

Never overwrite the current master before the candidate update has passed validation. Archive the previous master and immutable incoming files first. Every update must retain a manifest sufficient to reconstruct PRISMA/ROSES identification, deduplication, pre-screening removal, screening, and final inclusion counts.

## Important

Do not move or delete historical scripts until their dependencies and reproducibility value have been checked. The repository currently contains development generations of geography and topic workflows; these are candidates for archival, not automatically disposable files.
