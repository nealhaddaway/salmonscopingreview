# Annotation Workflow

## Automated Annotation Workflow for the Salmon Aquaculture Scoping Review

**Repository:** `salmonscopingreview`
**Document:** `docs/annotation_workflow.md`
**Status:** Technical documentation for the final annotation workflow accompanying the published systematic map.

---

# 1. Purpose

## Overview

This repository contains a fully reproducible workflow for automatically annotating the bibliographic records included within a global systematic map of salmon aquaculture research. The workflow was developed to support evidence mapping by generating structured metadata describing the principal farmed species investigated, the geographical settings referenced within publications, and the primary research topic addressed by each study.

The workflow was developed specifically for a corpus of **12,074 screened bibliographic records** retrieved during the systematic mapping process. Manual annotation of this corpus would have required many months of expert coding and would inevitably introduce variability between coders over time. The purpose of the workflow is therefore to automate those annotation tasks that can be implemented transparently and reproducibly while preserving complete auditability of every assignment.

The repository does **not** seek to replace expert judgement. Instead, it provides a reproducible computational framework that generates consistent first-pass annotations suitable for evidence mapping, exploratory analysis and interactive visualisation. Every annotation can be traced either to deterministic rules or to a documented language model prompt and ontology. Intermediate outputs are retained wherever practical so that users can inspect the reasoning underlying each stage of processing.

Three independent annotation layers are produced:

* farmed salmonid species;
* geographic mentions extracted from titles and abstracts;
* hierarchical research topics.

Each annotation layer was designed, implemented and validated independently before being integrated into a unified record-level dataset. This modular architecture allows individual components to be reused or replaced without requiring redevelopment of the entire workflow.

---

# 2. Design Philosophy

The annotation workflow was developed according to five overarching design principles. These principles governed every methodological decision throughout development and explain why different annotation tasks employ different computational approaches.

## 2.1 Transparency

Every annotation should be explainable.

Species and geography are implemented using deterministic rules because those decisions can be documented explicitly and reproduced exactly. Topic annotation employs a large language model because contextual interpretation proved essential for reliable thematic classification. Even in this case, transparency is maintained through publication of the complete topic ontology, annotation prompt, checkpoint outputs and validation procedures.

The objective was not simply to obtain accurate annotations, but to produce a workflow whose decisions could be understood, inspected and reproduced by other researchers.

---

## 2.2 Reproducibility

Running the workflow on the same corpus using the same repository version should produce identical outputs.

All deterministic components therefore rely upon fixed dictionaries, gazetteers and documented rule hierarchies rather than adaptive or stochastic learning procedures. Intermediate outputs are retained so that every stage can be regenerated independently.

The topic annotation module necessarily relies on an external language model. Reproducibility is therefore supported through fixed prompts, a fixed ontology, deterministic parsing of model outputs, automatic checkpointing and validation rather than by relying upon undocumented conversational interactions.

---

## 2.3 The Simplest Method That Solves the Problem

Each annotation task uses the simplest computational method capable of producing reliable results.

Species identification relies entirely upon deterministic dictionary matching and contextual rules because biological nomenclature is highly structured.

Geographic annotation similarly employs deterministic gazetteer matching because geographic entities can be recognised reliably without semantic interpretation.

Topic annotation, however, requires interpretation of scientific context that could not be reproduced satisfactorily using deterministic methods. Consequently, a large language model is used exclusively for this component.

This mixed methodology was adopted deliberately rather than by convenience. Deterministic methods were preferred wherever feasible because they maximise transparency and reproducibility.

---

## 2.4 Independent Annotation Modules

Species, geography and topic annotations were developed independently.

This modular design provides several advantages.

* Each module can be validated independently.
* Individual modules can be updated without modifying the remainder of the workflow.
* Researchers may adapt only those components relevant to their own evidence synthesis projects.
* Validation statistics remain interpretable because performance can be attributed to a single annotation task.

Integration occurs only after independent validation has been completed.

---

## 2.5 Evidence Preservation Rather Than Unsupported Inference

The workflow deliberately avoids making contextual inferences that cannot be implemented reproducibly.

This principle became particularly important during development of the geography module.

Several versions of the workflow attempted to identify the primary study country described by each publication. Although many assignments appeared plausible, manual validation demonstrated that reliable identification frequently depended upon contextual interpretation that could not be represented consistently using deterministic rules. Increasingly sophisticated heuristics improved some records while degrading others, ultimately reducing reproducibility without providing commensurate gains in performance.

The final workflow therefore extracts **geographic mentions** rather than inferred study locations. Although this approach retains some contextual country references, every retained geographic entity is explicitly supported by the source text and every filtering decision is deterministic.

Throughout the workflow, preserving extracted evidence is preferred to making unsupported assumptions.

---

# 3. Workflow Overview

The complete annotation workflow consists of four sequential stages.

```
Bibliographic records
        │
        ▼
Corpus preparation
        │
 ┌──────┼──────────┐
 │      │          │
 ▼      ▼          ▼
Species Geography Topics
 │      │          │
 └──────┼──────────┘
        ▼
Validation
        ▼
Integrated annotation dataset
```

Each annotation module operates independently before being merged into the final dataset.

---

## Stage 1. Corpus Preparation

Titles, abstracts and bibliographic metadata are imported into a standardised record-level dataset.

By this stage of the workflow:

* duplicate bibliographic records have already been removed;
* title-and-abstract screening has been completed;
* each publication possesses a unique record identifier;
* bibliographic fields have been standardised.

No annotation occurs during corpus preparation.

---

## Stage 2. Species Annotation

The species module identifies the principal cultured salmonid species investigated by each publication.

Primary outputs include:

* detected species mentions;
* filtered species mentions;
* assigned farmed species;
* validation datasets.

---

## Stage 3. Geographic Annotation

The geographic module detects geographic entities appearing within titles and abstracts.

Primary outputs include:

* geographic mention tables;
* record-level country summaries;
* record-level ISO summaries.

These outputs represent **geographic mentions**, not inferred study locations.

---

## Stage 4. Topic Annotation

The topic module assigns every publication to a single hierarchical research pathway.

Outputs include:

* hierarchical topic assignments;
* checkpoint datasets;
* failure logs;
* validation datasets.

---

## Final Integration

Following independent validation, the three annotation layers are merged into a unified record-level dataset used for downstream evidence mapping and visualisation.

---

# 4. Repository Structure

The repository is organised to separate source data, processing scripts, intermediate outputs and documentation.

```
salmonscopingreview/
│
├── data_raw/
│
├── R/
│
├── scripts/
│
├── outputs/
│
├── docs/
│   ├── annotation_workflow.md
│   ├── validation_report.md
│   ├── development_history.md
│   └── manuscript_methods.md
│
└── README.md
```

Intermediate outputs are retained wherever practical. This allows every annotation decision to be reconstructed from the original corpus without repeating the entire workflow.

---

# 5. Species Annotation

## Objective

The objective of the species annotation module is to identify the principal farmed salmonid species investigated by each publication. The annotation is intended to support evidence mapping rather than comprehensive biological indexing. Consequently, the workflow seeks to identify the cultured production system under investigation rather than every organism mentioned within a title or abstract.

---

## Biological Scope

The annotation focuses upon the principal cultured salmonid species represented within the evidence base. The workflow distinguishes target farmed species from incidental references to wild populations, non-target salmonids and broader taxonomic discussion.

This distinction is essential because review papers, ecological studies and introductory sections frequently mention multiple salmonid species despite focusing upon a single cultured production system.

---

## Methodological Rationale

Species annotation is implemented deterministically because biological nomenclature is highly structured.

Species names, scientific names, abbreviations and recognised synonyms can be represented explicitly within a curated dictionary, while contextual rules can distinguish genuine study species from background discussion with high reproducibility.

Validation demonstrated that deterministic rules provided accurate assignments while maintaining complete transparency of the decision process. Consequently, machine learning was considered unnecessary for this component.

---

## Implementation

### Primary scripts

| Script                                      | Purpose                              |
| ------------------------------------------- | ------------------------------------ |
| `scripts/00_setup.R`                        | Shared project setup and functions   |
| `scripts/01_audit_corpus.R`                 | Corpus preparation and auditing      |
| `scripts/02_run_species_annotations.R`      | Complete species annotation workflow |
| `scripts/03_validate_species_annotations.R` | Generation of validation datasets    |

Supporting functions are contained within the `R/` directory.

---

## Inputs

Species annotation requires:

* cleaned titles;
* cleaned abstracts;
* record identifiers;
* curated species dictionary.

The principal species dictionary is maintained separately from the code to permit independent updating without modifying the annotation logic.

---

## Annotation Workflow

Species annotation proceeds through a sequence of deterministic stages.

### Step 1. Dictionary Matching

Titles and abstracts are scanned against the curated species dictionary.

Detected matches include:

* scientific names;
* common names;
* recognised abbreviations;
* accepted spelling variants.

Every detection is retained within an intermediate dataset.

### Step 2. Context Filtering

Contextual rules remove detections that do not represent eligible farmed species.

Examples include:

* explicitly wild populations;
* non-target *Salmo* species;
* incidental background mentions;
* taxonomic discussion.

Filtering is entirely deterministic.

### Step 3. Record-Level Assignment

Remaining detections are ranked according to predefined decision rules.

Where one eligible cultured species remains, that species is assigned.

Where multiple eligible species receive equivalent support, multiple assignments are retained rather than forcing an unsupported decision.

Every assignment is accompanied by an assignment reason to facilitate auditing.

---

## Outputs

The species module generates the following principal outputs.

| Output                   | Description                      |
| ------------------------ | -------------------------------- |
| Species detections       | All dictionary matches           |
| Filtered detections      | Remaining eligible matches       |
| Record-level assignments | Final farmed species annotation  |
| Validation datasets      | Random samples for manual review |

Intermediate outputs are retained throughout development to maximise transparency and facilitate debugging and validation.

---

# 6. Geographic Annotation

## Objective

The objective of the geographic annotation module is to extract geographic information from the bibliographic record in a transparent, reproducible manner. Unlike many text-mining workflows, the objective is **not** to infer where the study was conducted. Instead, the module records all substantive geographic entities explicitly mentioned within the title or abstract after removal of non-content artefacts.

This distinction is fundamental. A publication may legitimately refer to several countries because it compares production systems, reviews international literature, discusses trade or supply chains, or places a local study within a broader global context. Attempting to identify a single "study country" requires contextual interpretation that cannot be implemented consistently using deterministic rules.

The final workflow therefore records geographic evidence rather than inferred study locations.

---

## Methodological Rationale

Development of the geography module underwent several iterations.

The initial implementation simply identified all geographic entities within titles and abstracts. This approach achieved very high recall but retained publisher locations, institutional affiliations, software references and other non-content geographic mentions.

Subsequent versions attempted deterministic inference of a primary study country through increasingly sophisticated rule hierarchies incorporating title weighting, study-site phrases, contextual ranking and candidate selection. Although these approaches appeared promising initially, repeated manual validation demonstrated that apparently simple decisions frequently depended upon semantic interpretation rather than explicit textual evidence. Improvements to one class of publications consistently introduced errors elsewhere. Comparative studies, review articles and papers containing extensive international background discussion proved particularly problematic.

Following repeated validation, these approaches were abandoned.

The final implementation therefore returns to a simpler philosophy: extract all genuine geographic mentions, remove deterministic false positives and preserve the remaining evidence without attempting unsupported inference.

This decision reflects the overarching design philosophy of the workflow. Where reliable contextual interpretation cannot be implemented reproducibly, the workflow preserves evidence rather than making assumptions.

---

## Implementation

### Primary scripts

| Script                                  | Purpose                                                |
| --------------------------------------- | ------------------------------------------------------ |
| `scripts/30_build_global_gazetteer.R`   | Build and standardise the gazetteer                    |
| `scripts/31_detect_geography.R`         | Detect geographic entities within titles and abstracts |
| `scripts/40_clean_geography_mentions.R` | Remove deterministic non-content geographic references |

Additional helper functions are located within the `R/` directory.

---

## Inputs

The geography module requires:

* record identifier;
* title;
* abstract;
* global gazetteer.

The gazetteer is maintained independently from the annotation code to permit updating without modifying the underlying workflow.

---

## Gazetteer Development

A custom gazetteer was assembled specifically for evidence synthesis rather than general geographic information retrieval.

The gazetteer includes:

* sovereign states;
* constituent countries;
* ISO country codes;
* country abbreviations;
* recognised country variants;
* demonyms;
* first-order administrative regions;
* selected lower administrative units;
* major populated places relevant to the corpus.

Development of the gazetteer was iterative. Entries producing systematic ambiguity were removed where appropriate, while additional variants encountered during validation were incorporated. The objective was not to maximise the number of place names but to maximise useful recall while maintaining deterministic behaviour.

---

## Geographic Detection

Titles and abstracts are scanned deterministically against the gazetteer.

For every successful match the workflow records:

* matched text;
* standardised country name;
* ISO code;
* match type;
* surrounding textual context;
* record identifier.

The surrounding context is retained because it supports subsequent deterministic filtering and facilitates manual validation.

No ranking or prioritisation occurs during this stage. Every detected geographic entity is retained for subsequent evaluation.

---

## Deterministic Cleaning

Raw geographic detection inevitably identifies locations unrelated to the scientific content of the publication. Rather than attempting semantic interpretation, the workflow removes only those contexts that can be recognised deterministically.

The final cleaning stage removes geographic references associated with:

* publishers;
* books and journal metadata;
* copyright notices;
* Creative Commons licences;
* software packages;
* translation widgets;
* manufacturers;
* suppliers;
* institutional affiliations;
* university addresses;
* research centres;
* laboratories;
* hospitals;
* international conventions;
* treaties;
* protocols.

Development of these filters was guided directly by manual inspection of approximately one thousand extracted geographic mentions. Rules were introduced only when they removed an entire class of deterministic false positives.

No filtering based upon subjective interpretation of scientific context is performed.

---

## Outputs

The geography module produces three principal outputs.

| Output                                         | Description                           |
| ---------------------------------------------- | ------------------------------------- |
| `global_geography_mentions_v3_clean.csv`       | Every retained geographic mention     |
| `record_country_annotations_v3_clean.csv`      | Record-level country summaries        |
| `global_geography_record_summary_v3_clean.csv` | Record-level ISO summaries and counts |

These outputs provide a transparent representation of the geographic evidence contained within the corpus.

---

## Validation

Validation was undertaken iteratively throughout development and culminated in manual inspection of approximately one thousand extracted geographic mentions.

Rather than estimating simple accuracy, validation sought recurring categories of deterministic error. Identified false positives were grouped according to their underlying cause, including publisher metadata, institutional addresses, software references, convention names and manufacturer locations. New rules were implemented only where a complete class of false positives could be removed without affecting genuine scientific content.

Following implementation of the final cleaning rules, remaining errors were predominantly associated with genuinely ambiguous place names and contexts requiring semantic interpretation rather than deterministic matching.

---

## Limitations

The geography module intentionally extracts geographic mentions rather than study locations.

Consequently, retained countries may represent:

* study locations;
* comparative examples;
* background literature;
* international trade;
* contextual discussion.

This behaviour is intentional and reflects the decision to preserve textual evidence rather than infer unsupported study locations.

---

# 7. Topic Annotation

## Objective

The objective of the topic annotation module is to assign each publication to a single hierarchical research pathway describing its principal scientific focus.

Unlike species and geography, research topics cannot be identified reliably through deterministic keyword matching because similar terminology appears across multiple conceptual domains. Topic annotation therefore employs a large language model constrained by a predefined ontology.

---

## Methodological Rationale

Early development explored deterministic keyword dictionaries and rule-based classification. These approaches proved inadequate because topic assignment requires interpretation of scientific context rather than recognition of isolated terms.

For example, identical keywords may appear in studies of nutrition, physiology, environmental impacts and economics while representing fundamentally different research questions.

A hierarchical ontology interpreted by a language model provided substantially more consistent conceptual classification while remaining constrained to predefined categories.

The language model therefore performs contextual interpretation, whereas the ontology provides standardisation and consistency.

---

## Implementation

### Primary scripts

| Script                              | Purpose                            |
| ----------------------------------- | ---------------------------------- |
| `scripts/50_topic_annotation.R`     | Complete LLM annotation workflow   |
| `scripts/51_retry_failed_records.R` | Retry failed API requests          |
| `scripts/52_validate_topics.R`      | Generate topic validation datasets |

Supporting prompt definitions and helper functions are stored within the `R/` directory.

---

## Inputs

The topic module requires:

* record identifier;
* title;
* abstract;
* hierarchical topic ontology;
* annotation prompt.

The ontology is maintained separately from the annotation code, allowing independent revision where required.

---

## Annotation Workflow

Each publication is submitted individually together with:

* the complete topic hierarchy;
* definitions of each category;
* detailed annotation instructions.

The model is instructed to identify the **single primary research focus** represented by the publication.

Only one hierarchical pathway is returned for each record.

Batch processing, checkpointing and automatic logging ensure that long-running annotation remains reproducible and recoverable following interruptions.

---

## Outputs

| Output              | Description                            |
| ------------------- | -------------------------------------- |
| Topic assignments   | Final hierarchical topic annotation    |
| Checkpoint files    | Intermediate progress during long runs |
| Failure log         | Records requiring retry                |
| Validation datasets | Random samples for manual assessment   |

---

## Validation

Validation is undertaken following completion of corpus annotation.

Random samples of publications are compared against expert assessment of the title and abstract. Validation focuses upon identifying systematic weaknesses in the ontology or prompt rather than isolated record disagreements.

Where recurring error categories are identified, modifications are applied to the workflow and the complete corpus is re-annotated.

---

## Limitations

Topic annotation depends upon bibliographic metadata rather than full-text articles. Consequently, some methodological or conceptual detail cannot be inferred from titles and abstracts alone.

Furthermore, publications spanning several research domains are necessarily assigned a single primary pathway despite legitimately addressing multiple themes.

---

# 8. Integration

Following validation, the three annotation layers are merged into a unified record-level dataset using the unique record identifier.

The integrated dataset contains:

* bibliographic metadata;
* species annotation;
* geographic annotations;
* hierarchical topic annotation.

This dataset forms the principal analytical product of the repository and underpins all subsequent evidence mapping, descriptive analyses and interactive visualisations.

---

# 9. Software Requirements

The workflow is implemented primarily in **R**.

Major dependencies include:

* tidyverse
* stringr
* readr
* here
* openxlsx
* httr2
* jsonlite

The topic annotation module additionally requires access to the configured language model API.

Package versions should be recorded using `renv` or an equivalent dependency management system to maximise long-term reproducibility.

---

# 10. Expected Runtime

Approximate runtimes will vary according to hardware and API performance.

| Module                |          Approximate runtime |
| --------------------- | ---------------------------: |
| Species annotation    |                      Minutes |
| Geographic annotation |                      Minutes |
| Topic annotation      | Several days (API dependent) |
| Integration           |                      Minutes |

The topic module dominates total execution time because each publication is processed independently.

---

# 11. Reproducing the Workflow

The complete workflow can be reproduced by executing the annotation modules sequentially.

1. Prepare the screened corpus.
2. Run the species annotation workflow.
3. Run the geographic annotation workflow.
4. Run the topic annotation workflow.
5. Retry any failed topic annotations.
6. Validate each annotation layer.
7. Merge validated outputs into the integrated dataset.

Intermediate outputs should be retained rather than overwritten to preserve complete provenance.

---

# 12. Relationship Between Repository Documents

This document describes **how** the annotation workflow operates.

Additional repository documents provide complementary information.

| Document                      | Purpose                                                             |
| ----------------------------- | ------------------------------------------------------------------- |
| `docs/validation_report.md`   | Detailed validation procedures and outcomes                         |
| `docs/development_history.md` | Design decisions, discarded approaches and methodological evolution |
| `docs/manuscript_methods.md`  | Condensed methods section suitable for journal submission           |

Together these documents provide complete documentation of the annotation workflow from implementation through validation to publication.

---

# 13. Known Limitations

No automated annotation workflow can perfectly reproduce expert interpretation of scientific literature.

The present workflow therefore prioritises:

* transparency;
* reproducibility;
* auditability;
* explicit methodological decisions.

Species annotation remains dependent upon explicit species references within bibliographic metadata.

Geographic annotation deliberately records geographic mentions rather than inferred study locations.

Topic annotation is necessarily constrained by the information available within titles and abstracts and by the structure of the predefined ontology.

These limitations represent conscious methodological choices rather than implementation deficiencies. The workflow consistently favours transparent and reproducible evidence extraction over increasingly complex inference procedures that cannot be validated reliably.

---

# 14. Citation

If this workflow or individual annotation modules are reused, users should cite both the accompanying systematic map manuscript and this repository. Users are also encouraged to cite the validation report when reporting annotation performance and the development history when discussing methodological decisions or adapting the workflow for other evidence synthesis projects.

---

# Appendix A. Repository Implementation

This appendix provides a complete mapping between the scientific workflow described in the main document and the scripts, intermediate outputs and final products contained within the repository. The appendix is intended primarily for researchers wishing to reproduce, audit or adapt the workflow.

The annotation workflow is modular. Each annotation layer can be executed independently, validated independently and reused independently.

---

# A1. Workflow Summary

| Stage                 | Purpose                                               | Primary outputs              |
| --------------------- | ----------------------------------------------------- | ---------------------------- |
| Corpus preparation    | Import and standardise screened bibliographic records | Clean corpus                 |
| Species annotation    | Identify principal cultured salmonid species          | Species assignments          |
| Geographic annotation | Extract geographic mentions                           | Geographic annotation tables |
| Topic annotation      | Assign hierarchical research topic                    | Topic hierarchy assignments  |
| Validation            | Independent validation of each annotation module      | Validation datasets          |
| Integration           | Merge annotation layers                               | Final annotated dataset      |

---

# A2. Species Annotation

## Primary scripts

| Script                                      | Purpose                                            |
| ------------------------------------------- | -------------------------------------------------- |
| `scripts/00_setup.R`                        | Shared project configuration and utility functions |
| `scripts/01_audit_corpus.R`                 | Audit and prepare corpus                           |
| `scripts/02_run_species_annotations.R`      | Execute complete species annotation workflow       |
| `scripts/03_validate_species_annotations.R` | Generate species validation datasets               |

Additional helper functions are contained within the `R/` directory.

---

## Primary inputs

| File                                                             | Purpose                       |
| ---------------------------------------------------------------- | ----------------------------- |
| `data_raw/INCLUDES fixed abstracts.txt`                          | Screened bibliographic corpus |
| `data_raw/Salmon scoping review keywords - dictionary final.csv` | Species dictionary            |

---

## Primary outputs

| Output                                            | Description                      |
| ------------------------------------------------- | -------------------------------- |
| `outputs/stage_2_species/species_mentions.csv`    | All detected species mentions    |
| `outputs/stage_2_species/species_annotations.csv` | Record-level species assignments |
| `outputs/stage_2_validation/`                     | Species validation datasets      |

---

# A3. Geographic Annotation

## Primary scripts

| Script                                  | Purpose                                                |
| --------------------------------------- | ------------------------------------------------------ |
| `scripts/30_build_global_gazetteer.R`   | Build geographic gazetteer                             |
| `scripts/31_detect_geography.R`         | Detect geographic entities                             |
| `scripts/40_clean_geography_mentions.R` | Remove deterministic non-content geographic references |

---

## Primary inputs

| File                                    | Purpose                                               |
| --------------------------------------- | ----------------------------------------------------- |
| `data_raw/INCLUDES fixed abstracts.txt` | Screened corpus                                       |
| Gazetteer                               | Curated global gazetteer used for geographic matching |

*(The gazetteer is generated and maintained separately from the annotation logic.)*

---

## Primary outputs

| Output                                                                                             | Description                       |
| -------------------------------------------------------------------------------------------------- | --------------------------------- |
| `outputs/stage_5_geography/global_detection_v3_clean/global_geography_mentions_v3_clean.csv`       | All retained geographic mentions  |
| `outputs/stage_5_geography/global_detection_v3_clean/record_country_annotations_v3_clean.csv`      | Record-level country summaries    |
| `outputs/stage_5_geography/global_detection_v3_clean/global_geography_record_summary_v3_clean.csv` | Record-level geographic summaries |

---

# A4. Topic Annotation

## Primary scripts

| Script                               | Purpose                               |
| ------------------------------------ | ------------------------------------- |
| `scripts/50_topic_annotation.R`*     | Execute hierarchical topic annotation |
| `scripts/51_retry_failed_records.R`* | Retry failed API requests             |
| `scripts/52_validate_topics.R`*      | Generate topic validation datasets    |

*Replace with the final filenames if they differ in the released repository.*

---

## Primary inputs

| File                                    | Purpose                          |
| --------------------------------------- | -------------------------------- |
| `data_raw/INCLUDES fixed abstracts.txt` | Screened corpus                  |
| Topic ontology                          | Hierarchical annotation ontology |
| Prompt template                         | LLM annotation instructions      |

---

## Primary outputs

| Output                   | Description                           |
| ------------------------ | ------------------------------------- |
| Topic annotation dataset | Record-level hierarchical assignments |
| Checkpoints              | Intermediate batch outputs            |
| Failure log              | Failed API requests                   |
| Validation datasets      | Random validation samples             |

---

# A5. Integration

## Primary script

| Script                   | Purpose                                                               |
| ------------------------ | --------------------------------------------------------------------- |
| Final integration script | Merge validated annotation layers into a unified record-level dataset |

---

## Final dataset

Each record contains:

* record identifier
* bibliographic metadata
* species annotation
* geographic annotations
* hierarchical topic annotation

The integrated dataset forms the principal analytical product of the repository.

---

# A6. Validation

Each annotation module is validated independently before integration.

| Module    | Validation approach                                                                                            |
| --------- | -------------------------------------------------------------------------------------------------------------- |
| Species   | Iterative manual assessment of random record samples                                                           |
| Geography | Manual inspection of approximately 1,000 geographic mentions and iterative refinement of deterministic filters |
| Topics    | Manual assessment of random topic assignments following completion of annotation                               |

Validation reports are provided separately in `docs/validation_report.md`.

---

# A7. Computational Requirements

| Requirement          | Notes                                                      |
| -------------------- | ---------------------------------------------------------- |
| Programming language | R                                                          |
| Major packages       | tidyverse, stringr, readr, openxlsx, httr2, jsonlite, here |
| External dependency  | Language model API for topic annotation                    |
| Version control      | Git                                                        |

The workflow has been developed to permit interruption and resumption through checkpointing. Species and geographic annotation complete within minutes on a modern desktop computer, whereas topic annotation is primarily limited by language model API throughput.

---

# A8. Repository Documentation

The repository contains four complementary documentation files.

| Document                      | Purpose                                                          |
| ----------------------------- | ---------------------------------------------------------------- |
| `docs/annotation_workflow.md` | Complete technical description of the annotation workflow        |
| `docs/validation_report.md`   | Validation methods, results and limitations                      |
| `docs/development_history.md` | Design decisions, iterative development and discarded approaches |
| `docs/manuscript_methods.md`  | Condensed manuscript-ready methods section                       |

Together, these documents provide a complete description of the scientific rationale, computational implementation, validation and evolution of the annotation workflow.