# Species + geography LLM adjudication
source("scripts/00_setup.R")
source("R/read_corpus.R")

api_key <- Sys.getenv("OPENAI_API_KEY")
if (!nzchar(api_key)) stop("OPENAI_API_KEY was not found.")

species_file <- here::here("outputs","stage_2_species","species_review_queue.csv")
geo_summary_file <- here::here("outputs","stage_5_geography","primary_study_country_v2","primary_country_summary_v2.csv")
geo_rank_file <- here::here("outputs","stage_5_geography","primary_study_country_v2","country_evidence_ranking_v2.csv")
records_file <- here::here("data_raw","INCLUDES fixed abstracts.txt")
out_dir <- here::here("outputs","stage_2_5_annotation_adjudication")
fs::dir_create(out_dir)

stopifnot(file.exists(species_file), file.exists(geo_summary_file),
          file.exists(geo_rank_file), file.exists(records_file))

records <- read_corpus(records_file) |>
  dplyr::mutate(record_id=as.character(record_id),
                title=dplyr::coalesce(as.character(title),""),
                abstract=dplyr::coalesce(as.character(abstract),""))

sp <- readr::read_csv(species_file, show_col_types=FALSE,
                      col_types=readr::cols(record_id=readr::col_character())) |>
  dplyr::group_by(record_id) |>
  dplyr::summarise(
    species_review_required=TRUE,
    deterministic_species=paste(sort(unique(stats::na.omit(farmed_species))),collapse="; "),
    deterministic_species_ids=paste(sort(unique(stats::na.omit(farmed_species_id))),collapse="; "),
    species_reasons=paste(sort(unique(stats::na.omit(assignment_reason))),collapse=" | "),
    non_target_species=paste(sort(unique(stats::na.omit(non_target_species))),collapse="; "),
    .groups="drop"
  )

geo <- readr::read_csv(geo_summary_file, show_col_types=FALSE,
                       col_types=readr::cols(record_id=readr::col_character())) |>
  dplyr::filter(review_required) |>
  dplyr::select(record_id, geography_review_required=review_required,
                geography_review_reason=review_reason,
                deterministic_primary_countries=primary_countries,
                deterministic_primary_iso3c=primary_iso3c)

geo_candidates <- readr::read_csv(geo_rank_file, show_col_types=FALSE,
                                   col_types=readr::cols(record_id=readr::col_character())) |>
  dplyr::group_by(record_id) |>
  dplyr::summarise(
    geography_candidates=paste(unique(paste0(country_name," [",iso3c,"]; tier ",best_tier)),collapse="; "),
    .groups="drop"
  )

queue <- records |>
  dplyr::select(record_sequence,record_id,title,abstract) |>
  dplyr::left_join(sp,by="record_id") |>
  dplyr::left_join(geo,by="record_id") |>
  dplyr::left_join(geo_candidates,by="record_id") |>
  dplyr::mutate(
    species_review_required=dplyr::coalesce(species_review_required,FALSE),
    geography_review_required=dplyr::coalesce(geography_review_required,FALSE)
  ) |>
  dplyr::filter(species_review_required | geography_review_required)

if(nrow(queue)==0) stop("No species/geography review records found.")

system_prompt <- paste(
"You are adjudicating species and primary study geography for a salmon-farming scoping review.",
"Only adjudicate dimensions explicitly flagged for review. Do not change an unflagged dimension.",
"",
"SPECIES:",
"Eligible farmed salmonids are Atlantic salmon; Pacific salmon species including Chinook, coho, sockeye, chum, pink and masu salmon; rainbow trout; and genuinely unspecified farmed salmon.",
"Do not infer a species merely because salmon farming is common in that species. If the text only supports generic salmon, use UNSPECIFIED_FARMED_SALMON.",
"Do not treat wild salmon, fisheries, conservation populations or unrelated fish as farmed species.",
"Return all eligible farmed salmonids that are substantive study subjects.",
"",
"GEOGRAPHY:",
"Identify the primary study geography, not every place mentioned.",
"A single country explicitly named in the title overrides abstract country mentions.",
"Multiple countries explicitly co-named in the title may all be primary.",
"Do not use countries mentioned only as background, comparison, literature context, author affiliation, supplier/manufacturer or other incidental context.",
"If the title names only a continent/macro-region, do not infer a country from incidental abstract mentions.",
"Known safeguards: New Brunswick = CAN; Northwest alone is not a country; Latin America is not USA; North America is not automatically USA.",
"Use supplied candidate countries as evidence, but change them when the title/abstract clearly establishes a different location. Do not invent a location without textual evidence.",
"",
"For each dimension choose ACCEPT, CHANGE, or UNRESOLVED.",
"For species, return a semicolon-separated list, or UNSPECIFIED_FARMED_SALMON, or NONE.",
"For geography, return ISO3 country code(s), or NONE if no defensible country-level primary geography exists.",
"Give one concise reason per dimension. Use only the supplied title, abstract and deterministic evidence.",
sep="\n")

schema <- list(type="object",properties=list(
  species_decision=list(type="string",enum=c("ACCEPT","CHANGE","UNRESOLVED","NOT_REVIEWED")),
  species=list(type="string"), species_reason=list(type="string"),
  geography_decision=list(type="string",enum=c("ACCEPT","CHANGE","UNRESOLVED","NOT_REVIEWED")),
  primary_country_iso3c=list(type="string"), geography_reason=list(type="string")
),required=c("species_decision","species","species_reason","geography_decision",
             "primary_country_iso3c","geography_reason"),additionalProperties=FALSE)

extract_text <- function(x) {
  msgs <- x$output[vapply(x$output,function(z) identical(z$type,"message"),logical(1))]
  cont <- unlist(lapply(msgs,function(z) z$content),recursive=FALSE)
  txt <- cont[vapply(cont,function(z) identical(z$type,"output_text") && !is.null(z$text),logical(1))]
  if(!length(txt)) stop("No output_text returned.")
  txt[[1]]$text
}

ask_model <- function(row) {
  sp_block <- if(row$species_review_required) paste(
    "SPECIES FLAGGED:", 
    paste("Current species:",dplyr::coalesce(row$deterministic_species,"NONE"),
          "IDs:",dplyr::coalesce(row$deterministic_species_ids,"NONE"),
          "Reasons:",dplyr::coalesce(row$species_reasons,"NONE"),
          "Non-target:",dplyr::coalesce(row$non_target_species,"NONE"),sep="\n")) else "SPECIES NOT FLAGGED: do not change."
  geo_block <- if(row$geography_review_required) paste(
    "GEOGRAPHY FLAGGED:",
    paste("Reason:",dplyr::coalesce(row$geography_review_reason,"NONE"),
          "Current:",dplyr::coalesce(row$deterministic_primary_countries,"NONE"),
          "ISO3:",dplyr::coalesce(row$deterministic_primary_iso3c,"NONE"),
          "Candidates:",dplyr::coalesce(row$geography_candidates,"NONE"),sep="\n")) else "GEOGRAPHY NOT FLAGGED: do not change."
  user <- paste("TITLE",row$title,"","ABSTRACT",row$abstract,"",sp_block,"",geo_block,
                "", "Adjudicate the flagged dimension(s).",sep="\n")
  body <- list(model="gpt-5-mini",store=FALSE,reasoning=list(effort="low"),
               input=list(
                 list(role="system",content=list(list(type="input_text",text=system_prompt))),
                 list(role="user",content=list(list(type="input_text",text=user)))
               ),
               text=list(verbosity="low",format=list(type="json_schema",
                    name="species_geography_adjudication",strict=TRUE,schema=schema)))
  tryCatch({
    res <- httr2::request("https://api.openai.com/v1/responses") |>
      httr2::req_auth_bearer_token(api_key) |>
      httr2::req_body_json(body,auto_unbox=TRUE) |>
      httr2::req_timeout(120) |>
      httr2::req_retry(max_tries=4,backoff=~2^.x) |>
      httr2::req_perform() |> httr2::resp_body_json()
    x <- jsonlite::fromJSON(extract_text(res),simplifyVector=TRUE)
    tibble::tibble(record_id=row$record_id,species_decision=x$species_decision,
                   llm_species=x$species,species_reason=x$species_reason,
                   geography_decision=x$geography_decision,
                   llm_primary_country_iso3c=x$primary_country_iso3c,
                   geography_reason=x$geography_reason,llm_failed=FALSE,llm_error=NA_character_)
  },error=function(e) tibble::tibble(record_id=row$record_id,
    species_decision="UNRESOLVED",llm_species=NA_character_,species_reason=NA_character_,
    geography_decision="UNRESOLVED",llm_primary_country_iso3c=NA_character_,
    geography_reason=NA_character_,llm_failed=TRUE,llm_error=conditionMessage(e)))
}

checkpoint <- fs::path(out_dir,"species_geography_adjudication_checkpoint.rds")
out_csv <- fs::path(out_dir,"species_geography_adjudication.csv")
results <- if(file.exists(checkpoint)) readRDS(checkpoint) else tibble::tibble()
done <- unique(results$record_id)
remaining <- queue |> dplyr::filter(!record_id %in% done)

for(i in seq_len(nrow(remaining))) {
  row <- remaining[i,]
  message("Adjudicating ",i," / ",nrow(remaining),": ",row$record_id)
  results <- dplyr::bind_rows(results,ask_model(row))
  saveRDS(results,checkpoint)
  readr::write_csv(results,out_csv,na="")
}

final <- queue |> dplyr::left_join(results,by="record_id")
readr::write_csv(final,fs::path(out_dir,"species_geography_adjudication_full.csv"),na="")
readr::write_csv(final |> dplyr::filter(llm_failed | species_decision=="UNRESOLVED" |
                                         geography_decision=="UNRESOLVED"),
                 fs::path(out_dir,"species_geography_human_review.csv"),na="")
summary <- tibble::tibble(
 dimension=c("species","geography"),
 records_flagged=c(sum(queue$species_review_required),sum(queue$geography_review_required)),
 accept=c(sum(final$species_decision=="ACCEPT",na.rm=TRUE),sum(final$geography_decision=="ACCEPT",na.rm=TRUE)),
 change=c(sum(final$species_decision=="CHANGE",na.rm=TRUE),sum(final$geography_decision=="CHANGE",na.rm=TRUE)),
 unresolved=c(sum(final$species_decision=="UNRESOLVED"|final$llm_failed,na.rm=TRUE),
              sum(final$geography_decision=="UNRESOLVED"|final$llm_failed,na.rm=TRUE)))
readr::write_csv(summary,fs::path(out_dir,"species_geography_adjudication_summary.csv"),na="")
print(summary)
