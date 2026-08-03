# Manuscript Methods

## Automated Annotation of Species, Geography and Research Topics

**Repository:** `salmonscopingreview`
**Document:** `docs/manuscript_methods.md`
**Purpose:** Draft methods section for adaptation into the manuscript.

---

# Automated Annotation

Following title-and-abstract screening, all included records were annotated using a reproducible computational workflow developed specifically for this systematic map. The workflow generated three independent annotation layers describing (i) the principal cultured salmonid species investigated, (ii) geographic entities mentioned within titles and abstracts, and (iii) the primary research topic represented by each publication. Annotation modules were developed independently, validated independently and subsequently integrated into a unified record-level dataset used for evidence mapping and downstream analyses.

The workflow was implemented in R using a combination of deterministic rule-based methods and large language model (LLM) classification. Deterministic methods were employed wherever annotation could be implemented transparently and reproducibly using explicit rules, whereas LLM-based annotation was reserved for tasks requiring contextual interpretation that could not be replicated satisfactorily using deterministic approaches. Intermediate outputs were retained throughout the workflow to maximise auditability and reproducibility. Complete implementation details, validation procedures and repository documentation are provided in the accompanying GitHub repository.

---

# Species Annotation

The objective of species annotation was to identify the principal cultured salmonid species investigated by each publication. Rather than attempting comprehensive extraction of all biological taxa mentioned within titles and abstracts, the workflow assigned the cultured production system representing the primary focus of each study.

Species annotation was implemented using a deterministic dictionary-based classifier. A curated species dictionary containing scientific names, common names, recognised abbreviations and accepted spelling variants was developed specifically for salmon aquaculture literature. Titles and abstracts were scanned against this dictionary to identify candidate species mentions before contextual filtering was applied.

Dictionary matching alone proved insufficient because many publications referred to multiple salmonid species despite investigating only one cultured species. Additional deterministic rules were therefore developed to distinguish target farmed salmonids from incidental references to wild populations, background taxonomic discussion and non-target members of *Salmo*. Remaining eligible detections were evaluated using a hierarchical decision framework to assign the principal cultured species. Where two eligible cultured species received equivalent support, multiple assignments were retained rather than forcing unsupported deterministic selection.

Species annotation was developed iteratively using repeated manual validation of randomly selected records. Validation focused on identifying recurring classes of error rather than isolated incorrect assignments. Modifications to the workflow were introduced only where systematic improvements could be achieved without reducing transparency or reproducibility.

---

# Geographic Annotation

Geographic annotation sought to identify geographic information contained within titles and abstracts while avoiding unsupported inference regarding study location.

A custom global gazetteer was developed containing sovereign states, constituent countries, first-order administrative regions, demonyms and recognised geographic variants. Titles and abstracts were scanned deterministically against this gazetteer, and every detected geographic entity was recorded together with its standardised country representation and associated metadata.

Development initially explored deterministic identification of primary study countries using contextual weighting of title and abstract mentions. However, repeated manual validation demonstrated that reliable study-location inference frequently required semantic interpretation that could not be implemented consistently using transparent deterministic rules. Increasing algorithmic complexity produced only marginal improvements while reducing reproducibility across different publication types, particularly review articles, comparative studies and publications containing extensive international background discussion.

The final workflow therefore retained geographic mentions rather than inferred study locations. Deterministic contextual filters removed geographic entities associated with publisher metadata, copyright notices, software references, translation widgets, manufacturers, institutional affiliations and international conventions. All remaining geographic entities were retained irrespective of whether they represented study locations, comparative examples or contextual discussion. This approach preserved explicitly reported geographic evidence while avoiding unsupported contextual inference.

---

# Topic Annotation

Unlike species and geography, assignment of research topics requires interpretation of scientific context rather than recognition of structured entities. Preliminary development explored deterministic keyword-based classification but demonstrated that similar terminology frequently occurred across conceptually distinct areas of salmon aquaculture research. Deterministic approaches therefore failed to distinguish reliably between neighbouring thematic categories.

Topic annotation was consequently implemented using a large language model constrained by a predefined hierarchical ontology. The ontology was developed specifically for salmon aquaculture literature and organised into multiple hierarchical levels progressing from broad thematic domains to increasingly specific research topics.

Each publication title and abstract was submitted together with the complete ontology and detailed annotation instructions. The language model was instructed to identify the single hierarchical pathway representing the principal research focus of the publication rather than multiple independent themes. Constraining model outputs to predefined ontology pathways ensured consistency across the corpus while allowing contextual interpretation unavailable through deterministic text matching.

Annotation was performed in batches using automated checkpointing, logging and failure recovery. Completed records were written periodically to intermediate outputs, permitting interrupted analyses to resume without repeating completed work. Failed API requests were logged separately and resubmitted following completion of the primary annotation run.

Ontology development, prompt refinement and workflow implementation proceeded iteratively throughout development. Validation focused primarily on recurring disagreements between expert assessment and automated classification, allowing ontology definitions and prompting strategies to be refined before final annotation of the complete corpus.

---

# Validation

Validation formed an integral component of workflow development rather than a final quality-control exercise. Each annotation module underwent repeated cycles of manual assessment, refinement and re-validation before integration into the final dataset.

Species annotation was validated using repeated random samples of record-level assignments throughout development. Errors were classified according to their underlying cause, including dictionary omissions, contextual disambiguation failures and ambiguous multi-species studies. Deterministic rule modifications were introduced only where recurring classes of error were identified.

Geographic annotation was validated through manual inspection of approximately one thousand extracted geographic mentions. Validation concentrated on identifying deterministic false positives rather than estimating study-location accuracy. Iterative refinement resulted in explicit filtering of publisher locations, institutional affiliations, software references, manufacturer locations and convention names while preserving genuine scientific geographic references.

Topic annotation was validated following completion of corpus annotation using randomly selected publications independently assessed against the hierarchical ontology. Validation distinguished genuinely incorrect classifications from ontology ambiguities and multidisciplinary publications, thereby informing refinement of ontology definitions and annotation prompts rather than isolated record-level corrections.

Across all modules, workflow refinement was guided by systematic error categories rather than individual examples. Development ceased when further increases in methodological complexity no longer produced meaningful improvements in reproducibility or annotation quality.

---

# Reproducibility and Availability

All annotation scripts, controlled vocabularies, gazetteers, ontologies, intermediate outputs, validation procedures and implementation documentation are available in the accompanying GitHub repository. The repository provides complete technical documentation describing workflow implementation (`annotation_workflow.md`), validation (`validation_report.md`) and methodological development (`development_history.md`), allowing the workflow to be reproduced or adapted for other evidence synthesis projects.
