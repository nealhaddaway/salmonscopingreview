# Production pipeline consolidation manifest

This branch is a consolidation checkpoint. Do not delete or move existing scripts yet.

## Production analytical stages

1. Ingest and normalise incoming records
2. Deduplicate against the existing master corpus
3. Retraction/correction checking
4. Statistical relevance screening
5. LLM relevance adjudication of uncertain records
6. Human review of residual relevance uncertainty
7. Deterministic species annotation
8. Deterministic geography annotation
9. Combined LLM species + geography adjudication of deterministic review queues
10. Human review of residual species/geography uncertainty
11. Validated topic classification
12. Merge into the master evidence map

## Frozen components

- Topic classifier: validated V4 topic coding.
- Geography: corrected primary-country pipeline, including title precedence and the New Brunswick / Northwest / Latin America safeguards.
- Species: deterministic annotation is the primary extractor; LLM is an adjudicator only for review-flagged records.
- Relevance: statistical classifier followed by LLM uncertainty screening and human review where required.

## Consolidation rule

Files named `_FIXED`, `_FINAL`, `v2`, `v3`, etc. are development history until dependency tracing confirms which code produced the validated production outputs. They must not be deleted merely because a newer-looking filename exists.

## Immediate objective

Create one authoritative production script per stage, move validation scripts into `validation/`, move superseded development scripts into `archive/`, and retain reproducible outputs and prompts. Test the consolidated pipeline before merging this branch into `main`.
