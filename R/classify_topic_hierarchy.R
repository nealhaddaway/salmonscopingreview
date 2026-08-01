# =============================================================================
# File: classify_topic_hierarchy.R
# Project: salmonscopingreview
# Purpose: Run the four-stage hierarchical LLM topic classifier
# =============================================================================

classify_topic_hierarchy <- function(
    title,
    abstract,
    ontology,
    model = "gpt-5-mini"
) {

  broad <- classify_broad_topics(
    title = title,
    abstract = abstract,
    model = model
  )

  broad_topics <- unlist(
    broad$broad_topics,
    use.names = FALSE
  )

  if (length(broad_topics) == 0L) {
    return(
      tibble::tibble(
        broad_topic = character(),
        subtopic = character(),
        feature = character(),
        component = character(),
        broad_review_required = logical(),
        subtopic_review_required = logical(),
        feature_review_required = logical(),
        component_review_required = logical(),
        review_required = logical()
      )
    )
  }

  subtopics <- classify_subtopics(
    title = title,
    abstract = abstract,
    broad_topics = broad_topics,
    ontology = ontology,
    model = model
  )

  if (nrow(subtopics) == 0L) {
    return(
      tibble::tibble(
        broad_topic = broad_topics,
        subtopic = NA_character_,
        feature = NA_character_,
        component = NA_character_,
        broad_review_required = broad$review_required,
        subtopic_review_required = TRUE,
        feature_review_required = NA,
        component_review_required = NA,
        review_required = TRUE
      )
    )
  }

  features <- classify_features(
    title = title,
    abstract = abstract,
    subtopics = subtopics,
    ontology = ontology,
    model = model
  )

  if (nrow(features) == 0L) {
    return(
      subtopics |>
        dplyr::transmute(
          broad_topic,
          subtopic,
          feature = NA_character_,
          component = NA_character_,
          broad_review_required = broad$review_required,
          subtopic_review_required = review_required,
          feature_review_required = TRUE,
          component_review_required = NA,
          review_required = TRUE
        )
    )
  }

  components <- classify_components(
    title = title,
    abstract = abstract,
    features = features,
    ontology = ontology,
    model = model
  )

  if (nrow(components) == 0L) {
    return(
      features |>
        dplyr::transmute(
          broad_topic,
          subtopic,
          feature,
          component = NA_character_,
          broad_review_required = broad$review_required,
          subtopic_review_required = FALSE,
          feature_review_required = review_required,
          component_review_required = TRUE,
          review_required = TRUE
        )
    )
  }

  components |>
    dplyr::transmute(
      broad_topic,
      subtopic,
      feature,
      component,
      broad_review_required = broad$review_required,
      subtopic_review_required = FALSE,
      feature_review_required = FALSE,
      component_review_required = review_required,
      review_required = (
        broad_review_required |
          subtopic_review_required |
          feature_review_required |
          component_review_required
      )
    )
}
