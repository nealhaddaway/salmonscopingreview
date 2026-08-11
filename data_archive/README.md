# Living evidence map data versioning

## Canonical locations

- `data_current/salmon_evidence_map.csv` is the current validated master.
- `data_updates/incoming/` contains new database-search exports awaiting an update.
- `data_archive/masters/` contains immutable dated copies of previous masters.
- `data_archive/incoming/UPDATE_YYYY-MM-DD/` contains exact copies of incoming search files used for an update.
- `data_archive/updates/UPDATE_YYYY-MM-DD/` contains processed outputs for an update.
- `data_archive/manifests/` contains the PRISMA/ROSES-oriented numerical accounting for each update.

The incoming directory is deliberately kept separate from the archive: files remain there until the corresponding update has been completed and its provenance is secured in the archive.

## Update principle

An update is archive-first and validation-gated:

1. Archive the current master unchanged.
2. Archive every incoming search-result file unchanged.
3. Process the incoming batch without modifying the current master.
4. Create and validate a candidate new master.
5. Only after validation succeeds, promote the candidate to `data_current/salmon_evidence_map.csv`.

The previous master is never overwritten. Raw incoming search files are never replaced by normalised or processed versions.

## Reporting accounting

Each update manifest records, separately for each search source where applicable:

- records identified
- within-source duplicates
- cross-source duplicates
- duplicates against the previous master
- records remaining after deduplication
- retractions removed
- corrections/errata removed
- other predefined pre-screen exclusions
- records entering screening
- automated screening exclusions
- LLM screening exclusions
- human screening exclusions
- records retained after screening
- records added to the master
- final master size

This allows the update to be reconstructed into a PRISMA 2020 / ROSES flow diagram without recovering counts retrospectively from intermediate files.

## Provenance

Final records should retain `record_id` and update/source provenance wherever the schema permits. A record that reappears in a later database search should remain identifiable as an existing master record rather than being treated as newly added evidence.
