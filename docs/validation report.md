# Validation Report

## Automated Annotation Workflow Validation Report

**Repository:** `salmonscopingreview`
**Document:** `docs/validation_report.md`
**Status:** Validation documentation accompanying the automated annotation workflow

---

# 1. Purpose

This document describes the validation procedures undertaken during development of the automated annotation workflow. It complements `annotation_workflow.md` by documenting how the annotation modules were evaluated, refined and ultimately accepted for inclusion within the final workflow.

Validation was considered an integral component of workflow development rather than a final assessment undertaken after implementation. Consequently, each annotation module underwent repeated cycles of manual evaluation and refinement before integration into the final dataset. Validation therefore served two distinct purposes:

1. to estimate whether the workflow was performing as intended; and
2. to identify systematic error classes that justified modification of the annotation rules or ontology.

Throughout development, emphasis was placed on identifying recurring sources of error rather than maximising apparent performance through ad hoc correction of individual records.

---

# 2. General Validation Philosophy

## Validation objectives

Three principles guided validation throughout the project.

### Systematic errors are more important than isolated errors

Large bibliographic corpora inevitably contain genuinely ambiguous publications that cannot be classified uniquely from titles and abstracts alone. Modifying workflows to accommodate isolated examples risks increasing overall complexity without improving general performance.

Validation therefore sought recurring categories of error rather than exceptional records.

---

### Rule changes require recurring evidence

No deterministic rule was introduced solely because it improved a single publication.

Instead, modifications were implemented only when manual validation demonstrated that an entire class of errors could be corrected consistently without introducing comparable errors elsewhere.

Examples include:

* publisher locations identified as geographic entities;
* institutional affiliation addresses;
* software references;
* manufacturer locations;
* explicit wild salmon contexts.

---

### Modules are validated independently

Species, geography and topic annotation were developed independently and therefore validated independently.

Independent validation allows performance to be attributed to individual components rather than to the integrated workflow and permits modules to be reused in other evidence synthesis projects.

---

# 3. Validation Strategy

Each annotation module followed the same general development cycle.

1. Develop an initial implementation.
2. Generate a random validation sample.
3. Compare automated annotations with expert judgement.
4. Categorise errors according to their underlying cause.
5. Revise deterministic rules or ontology where justified.
6. Repeat validation.
7. Freeze the module once further improvements became disproportionate to their complexity.

This iterative approach ensured that modifications were driven by empirical observations rather than anticipated failure modes.

---

# 4. Species Annotation Validation

## Objective

Species validation assessed whether the workflow correctly identified the principal cultured salmonid represented by each publication.

The purpose was not to recover every biological species mentioned within a publication, but to assign the cultured production system forming the primary focus of the study.

---

## Validation procedure

Following major revisions to the rule hierarchy, random samples of annotated publications were generated for manual inspection.

Each publication was assessed independently using its title and abstract.

The automated assignment was then classified as:

* correct;
* incorrect;
* ambiguous.

Incorrect assignments were further categorised according to their underlying cause.

---

## Error categories

Species validation identified several recurring classes of error during development.

### Dictionary omissions

Occasionally legitimate species names or recognised synonyms were absent from the dictionary.

Where recurring omissions were identified, dictionary entries were added rather than introducing additional contextual rules.

---

### Wild versus farmed ambiguity

Publications frequently discussed wild salmon populations within background sections despite focusing on farmed production systems.

Early versions occasionally retained these incidental references.

Development therefore introduced explicit contextual filters identifying wild populations, resulting in substantial improvements to assignment accuracy.

---

### Non-target salmonids

Certain publications discussed non-target members of the genus *Salmo* or related salmonids.

Validation demonstrated that these taxa should not contribute to assignment of the principal cultured species.

Explicit exclusion rules were therefore incorporated into the workflow.

---

### Multi-species studies

Some publications genuinely investigated multiple cultured salmonid species with equivalent emphasis.

Rather than forcing deterministic selection of a single species, the workflow retains multiple eligible assignments for subsequent review where appropriate.

This behaviour was considered preferable to unsupported inference.

---

## Outcomes

Repeated validation demonstrated that deterministic rules provided highly reproducible species annotation while remaining transparent and easily auditable.

The remaining limitations were largely associated with genuinely ambiguous publications rather than failures of deterministic implementation.

Species annotation was therefore considered complete once additional rule complexity ceased to produce meaningful improvements during validation.

---

# 5. Geographic Annotation Validation

## Objective

The objective of geographic validation was to assess whether the workflow correctly extracted substantive geographic entities from titles and abstracts while excluding non-content references.

Unlike many geographic text-mining studies, validation did **not** seek to estimate the accuracy of inferred study locations because the final workflow intentionally does not perform study-location inference.

Instead, validation assessed whether retained geographic mentions represented genuine geographic references relevant to the published text.

---

## Development history

Geographic annotation underwent the greatest methodological evolution of any module.

Development progressed through three broad phases.

### Phase 1

Raw geographic detection.

This implementation retained every geographic entity identified by the gazetteer.

Validation demonstrated excellent recall but poor precision because publisher metadata, institutional affiliations and other non-content references were also retained.

### Phase 2

Primary study-country inference.

Several increasingly sophisticated deterministic approaches attempted to identify the principal study country through title weighting, contextual ranking and evidence hierarchies.

Although these approaches improved some publications, repeated validation demonstrated substantial instability across different publication types.

The approaches were therefore abandoned.

### Phase 3

Deterministic cleaning of geographic mentions.

The final implementation returned to comprehensive geographic mention extraction combined with deterministic removal of identifiable non-content references.

This approach proved substantially more transparent and reproducible than contextual inference while preserving the geographic evidence contained within publications.

---

## Geographic Annotation Validation

### Validation procedure

Geographic validation was conducted iteratively throughout development rather than as a single end-point assessment. Each major revision of the workflow was followed by manual inspection of randomly selected records or extracted geographic mentions. Validation therefore informed subsequent development rather than simply measuring final performance.

Early validation focused primarily on record-level outputs because the workflow attempted to infer study countries. Following abandonment of that approach, validation shifted to the level of individual geographic mentions. This change reflected the revised objective of the module: to identify genuine geographic entities while excluding deterministic false positives.

The final validation consisted of manual inspection of approximately **1,000 extracted geographic mentions**. Each mention was evaluated within its surrounding textual context and classified according to whether it represented:

* a genuine geographic reference that should be retained;
* a deterministic false positive that could be removed through explicit rules; or
* an ambiguous case requiring contextual interpretation beyond the scope of the workflow.

Validation notes were recorded throughout this process, allowing recurring problems to be distinguished from isolated examples.

---

## Error categories

Manual validation identified a relatively small number of recurring deterministic error classes.

### Publisher and citation metadata

Many geographic references originated from publisher addresses, book publication locations or citation metadata rather than scientific content.

Typical examples included:

* publisher headquarters;
* book publication cities;
* ISBN and ISSN information;
* editorial information.

These contexts could be identified reliably using deterministic patterns and were removed during the final cleaning stage.

---

### Institutional affiliations

Author affiliations and institutional addresses represented one of the largest remaining sources of false positives.

Typical examples included:

* universities;
* research institutes;
* academies of sciences;
* departments;
* laboratories;
* hospitals.

These references identify author affiliations rather than study locations and were therefore removed using deterministic contextual filters.

---

### Software and manufacturer locations

Geographic references occasionally originated from software citations or equipment manufacturers rather than the scientific content of the publication.

These contexts were identified reliably and removed through explicit filtering rules.

---

### International conventions and treaties

Several publications referred to international agreements using geographic place names (for example, the Paris, Helsinki or Barcelona Conventions).

Although these references contain geographic entities, they do not describe locations relevant to the study itself.

Deterministic filtering rules were therefore introduced for convention and treaty contexts.

---

### Ambiguous place names

A small number of remaining errors resulted from genuinely ambiguous geographic names.

These cases generally involved place names occurring in multiple countries or administrative regions.

Where ambiguity could not be resolved deterministically from the surrounding text without introducing additional assumptions, the workflow retained the detected geographic mention rather than attempting unsupported disambiguation.

---

## Development outcomes

Validation substantially improved the precision of geographic annotation while maintaining high recall.

More importantly, validation demonstrated that attempts to infer primary study countries consistently required semantic interpretation beyond the scope of deterministic rules.

The final workflow therefore represents a deliberate methodological decision rather than a compromise.

Instead of attempting increasingly complex contextual inference, the workflow now provides a transparent and reproducible extraction of geographic evidence that can be interpreted appropriately during downstream analyses.

The geography module was considered complete once additional refinements yielded diminishing improvements while increasing methodological complexity.

---

# 6. Topic Annotation Validation

## Objective

Topic validation assessed whether the hierarchical pathway assigned to each publication accurately represented the principal research focus described within the title and abstract.

Unlike deterministic annotation, topic classification necessarily involves interpretation of scientific context. Validation therefore focused on agreement between automated classification and expert judgement rather than deterministic correctness.

---

## Validation procedure

Validation is undertaken following completion of the complete annotation run.

Random samples of annotated publications are generated from the full corpus.

Each sampled publication is assessed independently using its title and abstract together with the published topic ontology.

Reviewers determine whether the assigned pathway represents the principal focus of the publication.

Assignments are classified as:

* correct;
* partially correct;
* incorrect.

Accompanying notes describe the reason for disagreement where applicable.

These notes provide considerably more useful information than binary accuracy statistics because they identify recurring weaknesses in the ontology or prompting strategy.

---

## Error categories

Several classes of disagreement are anticipated during validation.

### Boundary ambiguity

Closely related branches of the hierarchy occasionally represent different interpretations of the same publication.

Such disagreements generally indicate opportunities to clarify ontology definitions rather than failures of the language model.

---

### Multidisciplinary publications

Some publications legitimately address several research themes simultaneously.

The workflow requires assignment of a single dominant pathway for evidence mapping purposes.

Consequently, disagreement may reflect differing interpretations of the principal contribution rather than an objectively incorrect classification.

---

### Ontology limitations

Validation may reveal research themes insufficiently distinguished within the hierarchy.

Where recurring ambiguity is identified across multiple publications, ontology revision is preferred over prompt modification because it improves both human and automated consistency.

---

### Prompt interpretation

Occasional disagreements may arise where the language model interprets the principal objective differently from the reviewer.

Only systematic examples justify modification of the annotation prompt.

---

## Outcomes

The topic module should not be judged solely by overall agreement.

Instead, validation should determine whether disagreements arise from:

* deficiencies within the ontology;
* deficiencies within the prompt;
* genuinely ambiguous publications.

This distinction is essential because only the first two categories justify modification of the workflow.

---

# 7. Module Completion Criteria

Each annotation module was considered complete when the following criteria were satisfied.

## Species

* No recurring deterministic error categories remained.
* Additional rule complexity produced negligible improvement.
* Remaining disagreements reflected genuine ambiguity.

---

## Geography

* Deterministic false positives had been removed.
* Manual validation no longer identified substantial recurring error classes.
* Remaining disagreements required contextual interpretation beyond deterministic methods.
* The workflow consistently extracted geographic evidence rather than unsupported study locations.

---

## Topics

* Validation demonstrates acceptable agreement with expert assessment.
* Remaining disagreements arise primarily from multidisciplinary or genuinely ambiguous publications.
* Ontology and prompt are stable across validation samples.

---

# 8. General Lessons Learned

Several general observations emerged during development.

First, validation is most valuable when undertaken iteratively throughout development rather than reserved until implementation is complete. Early identification of systematic error classes substantially reduced unnecessary complexity.

Second, transparent deterministic methods proved remarkably effective for structured annotation tasks such as species identification and geographic entity extraction. These modules benefited far more from careful rule design and validation than from increasingly sophisticated computational methods.

Third, the most appropriate computational method depended upon the nature of the annotation task. Structured entities such as species and geography could be extracted deterministically, whereas conceptual interpretation of research topics required a language model constrained by a carefully developed ontology.

Finally, the project demonstrated the importance of recognising when additional algorithmic complexity ceases to improve reproducibility. The decision to abandon deterministic study-country inference in favour of transparent geographic mention extraction illustrates this principle. Although the simpler workflow records more contextual geographic references, every retained annotation is explicitly supported by the source text and can be audited directly.

---

# 9. Remaining Limitations

Validation does not eliminate uncertainty.

Species annotation remains dependent upon explicit species references within bibliographic metadata.

Geographic annotation intentionally retains contextual geographic references alongside study locations because reliable deterministic distinction between these categories could not be achieved.

Topic annotation remains constrained by the information contained within titles and abstracts and by the requirement to assign a single dominant pathway to multidisciplinary publications.

These limitations should be interpreted as the boundaries of transparent automated annotation rather than shortcomings of the implementation.

---

# 10. Conclusions

Validation was treated as an integral component of workflow development rather than a final quality-control exercise. Each annotation module underwent repeated cycles of manual assessment, refinement and re-evaluation until further increases in methodological complexity no longer produced meaningful improvements.

The resulting workflow does not claim perfect annotation. Instead, it provides a transparent, reproducible and independently validated framework for automated annotation of large bibliographic corpora. By documenting both successful and discarded approaches, the repository aims to support future evidence synthesis projects seeking to balance automation with methodological transparency.
