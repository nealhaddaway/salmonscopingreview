# =============================================================================
# File: assign_farmed_species.R
# Project: salmonscopingreview
# Purpose: Assign farmed species from detected species mentions
# =============================================================================

assign_farmed_species <- function(species_mentions) {
  
  required_columns <- c(
    "species_id",
    "preferred_name",
    "is_farmed_candidate"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(species_mentions)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "Species mentions are missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  empty_assignment <- data.frame(
    farmed_species_id = NA_character_,
    farmed_species = NA_character_,
    assignment_role = "unresolved",
    review_required = TRUE,
    assignment_reason = "No eligible farmed species detected",
    non_target_species = NA_character_,
    stringsAsFactors = FALSE
  )
  
  if (nrow(species_mentions) == 0) {
    return(empty_assignment)
  }
  
  species_mentions$is_farmed_candidate <-
    as.logical(species_mentions$is_farmed_candidate)
  
  unique_species <- unique(
    species_mentions[
      ,
      c(
        "species_id",
        "preferred_name",
        "is_farmed_candidate"
      )
    ]
  )
  
  farmed_candidates <- unique_species[
    unique_species$is_farmed_candidate %in% TRUE,
    ,
    drop = FALSE
  ]
  
  non_target_species <- unique_species[
    unique_species$is_farmed_candidate %in% FALSE,
    ,
    drop = FALSE
  ]
  
  non_target_names <- NA_character_
  
  if (nrow(non_target_species) > 0) {
    non_target_names <- paste(
      sort(unique(non_target_species$preferred_name)),
      collapse = "; "
    )
  }
  
  # If a specific farmed salmon species has been detected, remove the generic
  # "Unspecified farmed salmon" assignment.
  specific_salmon_detected <- any(
    farmed_candidates$species_id != "UNSPEC_SALMON" &
      farmed_candidates$species_id != "ONC_MYKISS"
  )
  
  if (
    specific_salmon_detected &&
    "UNSPEC_SALMON" %in% farmed_candidates$species_id
  ) {
    farmed_candidates <- farmed_candidates[
      farmed_candidates$species_id != "UNSPEC_SALMON",
      ,
      drop = FALSE
    ]
  }
  
  if (nrow(farmed_candidates) == 0) {
    
    empty_assignment$non_target_species <- non_target_names
    
    if (nrow(non_target_species) > 0) {
      empty_assignment$assignment_reason <-
        "Only non-target species detected"
    }
    
    return(empty_assignment)
  }
  
  if (nrow(farmed_candidates) == 1) {
    
    assignment_role <- "primary"
    assignment_reason <- "One eligible farmed species detected"
    
  } else {
    
    assignment_role <- "co-primary"
    assignment_reason <-
      paste0(
        nrow(farmed_candidates),
        " eligible farmed species detected; none treated as primary"
      )
  }
  
  results <- data.frame(
    farmed_species_id = farmed_candidates$species_id,
    farmed_species = farmed_candidates$preferred_name,
    assignment_role = rep(
      assignment_role,
      nrow(farmed_candidates)
    ),
    review_required = rep(
      FALSE,
      nrow(farmed_candidates)
    ),
    assignment_reason = rep(
      assignment_reason,
      nrow(farmed_candidates)
    ),
    non_target_species = rep(
      non_target_names,
      nrow(farmed_candidates)
    ),
    stringsAsFactors = FALSE
  )
  
  results <- results[
    order(results$farmed_species),
    ,
    drop = FALSE
  ]
  
  rownames(results) <- NULL
  
  results
}