# Canonical production pipeline

This document identifies the current reproducible workflow. Superseded development generations are retained in Git history or archived only after their dependencies have been checked; they are not part of production merely because an old numbered file exists.

## Frozen historical master

Baseline: `data_current/salmon_evidence_map.csv` (12,043 records; archived baseline dated 2026-08-11).

The historical master was assembled by `scripts/07_build_historical_master.R` from validated historical annotation outputs.

## Historical annotation components retained for provenance

### Species

- `02_run_species_annotation.R`
- `03_validate_species_assignments.R`
- `03b_validate_species_filters.R`
- `04_build_species_dataset.R`

### Geography — final validated lineage

The final corrected geography workflow is:

- `25_add_continent_region_layer_FINAL.R`
- `26_run_global_geography_detection_v3_FINAL.R`
- `34_assign_primary_study_country_v2_FINAL.R`
- `41_rerun_final_geography.R` — orchestration wrapper

These implement the validated corrections including standalone directional-term blocking, Latin America handling, the New Brunswick -> Canada override, and strict title-country precedence. The superseded non-FINAL copies of scripts 25, 26 and 34 have been removed from the working tree and remain recoverable in Git history.

### Topics — validated V4 lineage

- `50_run_topic_v4_classifier.R` — classifier/validation runner
- `51_compare_topic_v4_validation.R` — validation comparison
- `52_run_topic_v4_full_corpus.R` — historical full-corpus application
- `53_sample_topic_v4_validation_100.R` and `54_create_interim_topic_v4_validation_100.R` — validation sampling/inspection

Earlier topic V2/V3 generations are historical development lineage and are not production classifiers.

## Living update workflow

1. `scripts/09_start_update.R` — initialise an update and archive the current master and immutable incoming search files.
2. Relevance/retraction workflow — `63_screen_new_search_results.R`, `64_llm_screen_uncertain_records.R`, `65_remove_retractions_and_notices.R`, and `66_rescreen_previous_llm_uncertain_v2.R` as applicable to the incoming batch.
3. Species annotation — use the validated species logic from scripts 02–04, operating on the update batch rather than the historical corpus.
4. Geography — use the final validated geography logic (25 FINAL -> 26 FINAL -> 34 FINAL), operating on the update batch rather than the historical corpus.
5. Combined species/geography adjudication — `05_llm_adjudicate_species_geography.R` followed by `06_validate_species_geography_adjudication.R`.
6. Topics — use the frozen Topic V4 ontology/prompt logic represented by the V4 workflow, operating on the update batch rather than the 12,043-record historical corpus.
7. Final assembly — append only genuinely new, validated records to the archived previous master and promote the candidate master only after validation.

## Canonical data locations

- Current master: `data_current/`
- Incoming searches: `data_updates/incoming/`
- Archived masters and update artefacts: `data_archive/`
- Generated outputs: `outputs/`

## Versioning rule

Never overwrite the current master before the candidate update has passed validation. Archive the previous master and immutable incoming files first. Every update must retain a manifest sufficient to reconstruct PRISMA/ROSES identification, deduplication, pre-screening removal, screening, and final inclusion counts.

## Important

The historical scripts remain valuable provenance. Do not delete additional development scripts until their dependencies and role in reproducing the frozen historical master have been checked. Future cleanup should prefer archival over deletion.
