# =============================================================================
# File: llm_prompts.R
# Project: salmonscopingreview
# Purpose: Central prompt definitions for hierarchical LLM topic classification
# =============================================================================

broad_topic_prompt <- function() {
  paste(
    "You are classifying scientific abstracts for a systematic evidence map",
    "of farmed salmon and rainbow trout research.",
    "",
    "Assign every broad topic that represents a substantive objective,",
    "intervention, exposure, measured outcome, interpretation or application",
    "of the study. A topic need not be the primary focus to be substantive.",
    "",
    "Include implications for aquaculture, food, people or the environment when",
    "they are directly investigated or explicitly interpreted from the results.",
    "Do not assign topics mentioned only as background or motivation.",
    "Do not classify from isolated keywords alone.",
    "",
    "The only valid labels are:",
    "Production",
    "Impacts",
    "Consumption",
    "Business and economy",
    "Research methods",
    sep = "\n"
  )
}

subtopic_prompt <- function() {
  paste(
    "You are classifying scientific abstracts for a systematic evidence map",
    "of farmed salmon and rainbow trout research.",
    "",
    "The record has already been assigned one or more broad topics.",
    "Select every subtopic path representing a substantive objective,",
    "intervention, exposure, measured outcome, interpretation or application.",
    "A secondary theme should be included when it is genuinely investigated.",
    "",
    "Do not select paths mentioned only as background or motivation.",
    "Do not introduce paths that are not listed.",
    "Return an empty assignments array only when none of the listed subtopics",
    "can reasonably represent the substantive study.",
    sep = "\n"
  )
}

feature_prompt <- function() {
  paste(
    "You are classifying scientific abstracts for a systematic evidence map",
    "of farmed salmon and rainbow trout research.",
    "",
    "Broad topics and subtopics have already been selected.",
    "Select every feature path representing a substantive objective,",
    "intervention, exposure, measured outcome, interpretation or application.",
    "Include legitimate secondary features, not only the primary feature.",
    "",
    "For pathology studies, consider organism, illness, treatment, prevention",
    "and impacts separately when each is substantively investigated.",
    "For production studies, consider management practices, identification,",
    "genetics, development, feed additives and measured biological outcomes",
    "when they are part of the study design or interpretation.",
    "",
    "Do not select paths mentioned only as background or motivation.",
    "Use only the listed feature paths.",
    sep = "\n"
  )
}

component_prompt <- function() {
  paste(
    "You are classifying scientific abstracts for a systematic evidence map",
    "of farmed salmon and rainbow trout research.",
    "",
    "Broad topics, subtopics and features have already been selected.",
    "Select every component path representing a substantive objective,",
    "intervention, exposure, measured outcome, interpretation or application.",
    "Include legitimate secondary components, not only the primary component.",
    "",
    "Representative terms are non-exhaustive semantic cues. They may be stems,",
    "abbreviations, examples, spelling variants or related concepts.",
    "Use them to understand each component's intended scope, but do not require",
    "an exact match and do not assign a component from an isolated term alone.",
    "",
    "When a pathology study measures mortality, morbidity, lesions, severity,",
    "prevalence, performance loss, disease resolution or treatment efficacy,",
    "consider the Pathology > Impacts component.",
    "Treat substances or interventions intended to stimulate immune function as",
    "pathology treatments when that is their aquaculture role.",
    "Treat feed supplements such as probiotics, amino acids and functional",
    "ingredients as Feed > Additives when used as dietary interventions.",
    "Treat fillet or sensory quality outcomes as Consumption > Palatability when",
    "that branch is available.",
    "",
    "Do not select paths mentioned only as background or motivation.",
    "Use only the listed component paths.",
    sep = "\n"
  )
}
