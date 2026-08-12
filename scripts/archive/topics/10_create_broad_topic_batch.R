# =============================================================================
# File: 10_create_broad_topic_batch.R
# Project: salmonscopingreview
# Purpose: Create GPT Batch requests for broad topic classification
# =============================================================================

source("scripts/00_setup.R")

pilot_records <- readr::read_csv(
  here::here(
    "outputs",
    "stage_4_llm",
    "pilot",
    "llm_topic_pilot_records_20.csv"
  ),
  show_col_types = FALSE
)

output_dir <- here::here(
  "outputs",
  "stage_4_llm",
  "broad_topics"
)

fs::dir_create(output_dir)

system_prompt <- paste(
  "You are classifying scientific abstracts for a systematic evidence map.",
  "",
  "Classify ONLY the broad topics that represent substantive subjects",
  "of investigation.",
  "",
  "Do NOT classify background information.",
  "Do NOT classify incidental mentions.",
  "Do NOT classify based only on the appearance of keywords.",
  "",
  "The only valid labels are:",
  "- Production",
  "- Impacts",
  "- Consumption",
  "- Business and economy",
  "- Research methods",
  "",
  "Return valid JSON only.",
  sep = "\n"
)

schema <- list(
  
  type = "object",
  
  properties = list(
    
    broad_topics = list(
      
      type = "array",
      
      items = list(
        
        type = "string",
        
        enum = c(
          "Production",
          "Impacts",
          "Consumption",
          "Business and economy",
          "Research methods"
        )
        
      )
      
    ),
    
    review_required = list(
      type = "boolean"
    ),
    
    review_reason = list(
      type = c("string","null")
    )
    
  ),
  
  required = c(
    "broad_topics",
    "review_required",
    "review_reason"
  ),
  
  additionalProperties = FALSE
  
)

make_request <- function(
    record_sequence,
    record_id,
    title,
    abstract
){
  
  user_prompt <- paste0(
    
    "Title\n",
    title,
    
    "\n\nAbstract\n",
    abstract,
    
    "\n\nAssign one or more broad topics."
    
  )
  
  list(
    
    custom_id = paste0(
      "record_",
      record_sequence
    ),
    
    method = "POST",
    
    url = "/v1/responses",
    
    body = list(
      
      model = "gpt-5-mini",
      
      store = FALSE,
      
      input = list(
        
        list(
          
          role = "system",
          
          content = list(
            
            list(
              type="input_text",
              text=system_prompt
            )
            
          )
          
        ),
        
        list(
          
          role="user",
          
          content=list(
            
            list(
              type="input_text",
              text=user_prompt
            )
            
          )
          
        )
        
      ),
      
      text=list(
        
        format=list(
          
          type="json_schema",
          
          name="broad_topics",
          
          strict=TRUE,
          
          schema=schema
          
        )
        
      )
      
    )
    
  )
  
}

requests <- purrr::pmap(
  pilot_records,
  make_request
)

json_lines <- vapply(
  requests,
  jsonlite::toJSON,
  character(1),
  auto_unbox=TRUE,
  null="null",
  na="null"
)

jsonl_file <- fs::path(
  output_dir,
  "broad_topic_batch_20.jsonl"
)

writeLines(
  json_lines,
  jsonl_file,
  useBytes=TRUE
)

message(
  "Pilot records: ",
  nrow(pilot_records)
)

message(
  "Batch requests: ",
  length(requests)
)

message(
  "JSONL: ",
  jsonl_file
)