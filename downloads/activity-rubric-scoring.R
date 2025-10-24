# Syntax for "Activity: Scoring with Rubrics"


## Med Ed: Applying Analytic Rubric
source('claude_plus.R')
source('format_for_qmd.R')
ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")

osce_note <- "45-year-old male presents with Chest pain. Reports pressure-like chest discomfort that started this morning while walking up stairs. Some SOB. Recalls previous less severe episodes. H/O high blood pressure (takes atorvastatin). Current vitals positive for hypertension and tachycardia; other vitals unremarkable. Symptoms suggest possible angina; anxiety or GERD could also be considered."

analytic_rubric <- "For each of the following criteria, determine whether the element is Included or Not Included in the response.

1. Chief concern of chest pain
2. Episodic pattern of symptoms
3. Poorly controlled history of hypertension
4. Vitals indicate hypertension
5. Pain radiates to the back
6. Likely diagnosis of acute coronary syndrome (ACS), NSTEMI, or STEMI"

build_osce_analytic_prompt <- function(student_note, rubric){
  glue::glue("
  Using the rubric provided below, score the following OSCE post-encounter note. 
  For each criterion, indicate whether it was met and provide brief justification for your scoring decision. 
    
  Here is the student note:{student_note}
    
  Here is the rubric:{rubric}
    
  Return a completed rubric, indicating which criteria are met or not met and justifications for each component.
  ")
}

analytic_prompt <- build_osce_analytic_prompt(osce_note, analytic_rubric)

analytic_response <- claude_plus(analytic_prompt,
                                 temperature = 0)

analytic_response <- format_for_qmd(analytic_response)
knitr::asis_output(analytic_response)



### Med Ed: Applying Holistic Rubric
holistic_rubric <- "Each criterion should be rated as Insufficient (0 points), Developing (1 point), or Proficient (2 points) based on the quality of the response.

Chief complaint is clearly documented
- Insufficient: Missing or unclear
- Developing: Present but vague
- Proficient: Clear and specific

Relevant history is included
- Insufficient: Minimal or missing key details
- Developing: Some relevant information
- Proficient: Comprehensive and pertinent

Physical exam findings are documented
- Insufficient: Absent or incomplete
- Developing: Basic findings noted
- Proficient: Thorough and organized

Assessment includes appropriate differential diagnoses
- Insufficient: Missing or inappropriate
- Developing: Limited differential
- Proficient: Well-developed differential

Plan is reasonable and addresses the patient’s concerns
- Insufficient: Inadequate or missing
- Developing: Partially addresses concerns
- Proficient: Complete and appropriate

Documentation is organized
- Insufficient: Disorganized or difficult to follow
- Developing: Somewhat structured
- Proficient: Logical and clear structure"

build_osce_analytic_prompt <- function(student_note, rubric){
  glue::glue("
  Using the rubric provided below, score the following OSCE post-encounter note. 

  Here is the student note: {student_note}
    
  Here is the rubric: {rubric}
    
  Return a completed rubric, indicating justifications for each component.
  ")
}

holistic_prompt <- build_osce_analytic_prompt(osce_note, holistic_rubric)

holistic_response <- claude_plus(holistic_prompt,
                                 temperature = 0)

holistic_response <- format_for_qmd(holistic_response)
knitr::asis_output(holistic_response)



## Applying Holistic Rubrics

load('data/student_essays.Rdata')

focal_student <- subset(student_essays, id == 2002)
focal_prompt <- focal_student$prompt
focal_essay <- focal_student$essay

# Building a function to generate the prompt.

all_rubric_prompt <- function(prompt, essay){
  glue::glue("
You are an expert essay grader. Score the following student essay based on three criteria: Content, Organization, and Language. Each criterion should be scored from 1 to 5 in increments of 0.5 (e.g., 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5).

RUBRIC:

**Content (1-5):**
Paragraph is well-developed and relevant to the argument, supported with strong reasons and examples.

**Organization (1-5):**
The argument is very effectively structured and developed, making it easy for the reader to follow the ideas and understand how the writer is building the argument. Paragraphs use coherence devices effectively while focusing on a single main idea.
 
**Language (1-5):**
The writing displays sophisticated control of a wide range of vocabulary and collocations. The essay follows grammar and usage rules throughout the paper. Spelling and punctuation are correct throughout the paper.

PROMPT: {prompt}

ESSAY: {essay}

Please provide scores for each criterion and calculate the total score. Format your response as follows:
Content: [score]
Organization: [score]
Language: [score]
Total: [sum of three scores]

Provide a brief justification (1-2 sentences) for each score.
")
}

one_rep_essay <- claude_plus(prompt = all_rubric_prompt(focal_prompt, focal_essay))
one_rep_essay <- format_for_qmd(one_rep_essay)



## Applying Holistic Rubric (Loop Scoring)

all_rubric_prompt <- function(prompt, essay){
  glue::glue("
You are an expert essay grader. Score the following student essay based on three criteria: Content, Organization, and Language. Each criterion should be scored from 1 to 5 in increments of 0.5.

RUBRIC:

**Content (1-5):**
- 5: Exceptionally well-developed and highly relevant, with strong reasons and compelling examples.
- 4: Well-developed and relevant, with clear reasons and good examples.
- 3: Adequately developed with acceptable reasons and examples.
- 2: Underdeveloped with weak or insufficient reasons and examples.
- 1: Poorly developed and largely irrelevant.

**Organization (1-5):**
- 5: Exceptionally well-structured, very easy to follow, expert use of coherence devices.
- 4: Effectively structured, easy to follow, good use of coherence devices.
- 3: Adequate structure, generally followable.
- 2: Weak structure, sometimes difficult to follow.
- 1: Lacks clear structure, confusing.

**Language (1-5):**
- 5: Sophisticated vocabulary, grammar and spelling correct throughout.
- 4: Good vocabulary control, generally correct with minor errors.
- 3: Adequate vocabulary, acceptable with some errors.
- 2: Limited vocabulary, frequent errors that sometimes impede understanding.
- 1: Very limited vocabulary, numerous errors.

PROMPT: {prompt}

ESSAY: {essay}

Format your response as:
Content: [score]
Organization: [score]
Language: [score]
Total: [sum of three scores]
")
}

# Score extraction function with debug capability
extract_scores <- function(gpt_output, debug = FALSE){
  if(debug){
    cat("=== DEBUG: Extracting scores ===\n")
    cat("Input text (first 300 chars):\n")
    cat(substr(gpt_output, 1, 300), "\n\n")
  }
  
  # Extract Content score - try multiple patterns
  content_match <- stringr::str_match(gpt_output, "Content:?\\s*([0-9.]+)")
  if(is.na(content_match[1, 2])){
    # Try alternate pattern
    content_match <- stringr::str_match(gpt_output, "\\*\\*Content:?\\*\\*[^0-9]*([0-9.]+)")
  }
  content <- as.numeric(content_match[1, 2])
  
  # Extract Organization score
  org_match <- stringr::str_match(gpt_output, "Organization:?\\s*([0-9.]+)")
  if(is.na(org_match[1, 2])){
    org_match <- stringr::str_match(gpt_output, "\\*\\*Organization:?\\*\\*[^0-9]*([0-9.]+)")
  }
  organization <- as.numeric(org_match[1, 2])
  
  # Extract Language score
  lang_match <- stringr::str_match(gpt_output, "Language:?\\s*([0-9.]+)")
  if(is.na(lang_match[1, 2])){
    lang_match <- stringr::str_match(gpt_output, "\\*\\*Language:?\\*\\*[^0-9]*([0-9.]+)")
  }
  language <- as.numeric(lang_match[1, 2])
  
  # Extract Total score
  total_match <- stringr::str_match(gpt_output, "Total:?\\s*([0-9.]+)")
  if(is.na(total_match[1, 2])){
    total_match <- stringr::str_match(gpt_output, "\\*\\*Total:?\\*\\*[^0-9]*([0-9.]+)")
  }
  total <- as.numeric(total_match[1, 2])
  
  if(debug){
    cat("Extracted values:\n")
    cat("Content:", content, "\n")
    cat("Organization:", organization, "\n")
    cat("Language:", language, "\n")
    cat("Total:", total, "\n\n")
  }
  
  # Return as a named vector
  c(content = content, 
    organization = organization, 
    language = language, 
    total = total)
}


# Returns a data frame (useful for binding multiple results)
extract_scores_df <- function(gpt_output){
  scores <- extract_scores(gpt_output)
  data.frame(
    content = scores["content"],
    organization = scores["organization"],
    language = scores["language"],
    total = scores["total"]
  )
}

essay_score_df <- extract_scores_df(one_rep_essay)

for(i in 1:19){
  
  essay_score_again <- claude_plus(prompt = all_rubric_prompt(focal_prompt, focal_essay))
  essay_score_one <- extract_scores_df(essay_score_again)
  essay_score_df <- rbind(essay_score_df, essay_score_one)
  
}

rownames(essay_score_df) <- NULL

# Function to get summary statistics for multiple scored essays
summarize_scores <- function(score_df){
  
  # Calculate summary statistics
  summary_stats <- data.frame(
    criterion = c("Content", "Organization", "Language", "Total"),
    mean = c(mean(score_df$content, na.rm = TRUE),
             mean(score_df$organization, na.rm = TRUE),
             mean(score_df$language, na.rm = TRUE),
             mean(score_df$total, na.rm = TRUE)),
    sd = c(sd(score_df$content, na.rm = TRUE),
           sd(score_df$organization, na.rm = TRUE),
           sd(score_df$language, na.rm = TRUE),
           sd(score_df$total, na.rm = TRUE)),
    min = c(min(score_df$content, na.rm = TRUE),
            min(score_df$organization, na.rm = TRUE),
            min(score_df$language, na.rm = TRUE),
            min(score_df$total, na.rm = TRUE)),
    max = c(max(score_df$content, na.rm = TRUE),
            max(score_df$organization, na.rm = TRUE),
            max(score_df$language, na.rm = TRUE),
            max(score_df$total, na.rm = TRUE))
  )
  
  return(summary_stats)
}

score_summary <- summarize_scores(essay_score_df)
score_summary



## Applying Holistic Rubric (Low Temp; Loop Scoring)

low_temp_essay <- claude_plus(prompt = all_rubric_prompt(focal_prompt, focal_essay),
                              temperature = 0)
low_temp_df <- extract_scores_df(low_temp_essay)

for(i in 1:19){
  
  low_temp_score_again <- claude_plus(prompt = all_rubric_prompt(focal_prompt, focal_essay),
                                      temperature = 0)
  low_temp_score_one <- extract_scores_df(low_temp_score_again)
  low_temp_df <- rbind(low_temp_df, low_temp_score_one)
  
}

rownames(low_temp_df) <- NULL
low_temp_summary <- summarize_scores(low_temp_df)
low_temp_summary



## Loop Scoring with Detailed Rubric

claude_all_rubric_prompt <- function(prompt, essay){
  glue::glue("
You are an expert essay grader. Score the following student essay based on three criteria: Content, Organization, and Language. Each criterion should be scored from 1 to 5 in increments of 0.5 (e.g., 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5).

RUBRIC:

**Content (1-5):**
- 5: Paragraph is exceptionally well-developed and highly relevant to the argument, supported with strong, specific reasons and compelling examples.
- 4: Paragraph is well-developed and relevant to the argument, supported with clear reasons and good examples.
- 3: Paragraph is adequately developed with some relevance to the argument, supported with acceptable reasons and examples.
- 2: Paragraph is underdeveloped or somewhat irrelevant to the argument, with weak or insufficient reasons and examples.
- 1: Paragraph is poorly developed and largely irrelevant to the argument, lacking meaningful reasons and examples.

**Organization (1-5):**
- 5: The argument is exceptionally well-structured and developed, making it very easy for the reader to follow ideas and understand how the argument is built. Paragraphs use coherence devices expertly while maintaining clear focus on main ideas.
- 4: The argument is effectively structured and developed, making it easy for the reader to follow ideas. Paragraphs use coherence devices well and focus on main ideas.
- 3: The argument has adequate structure and development. The reader can follow most ideas. Paragraphs use some coherence devices and generally focus on main ideas.
- 2: The argument has weak structure and development. Ideas are sometimes difficult to follow. Paragraphs use few coherence devices and may lose focus.
- 1: The argument lacks clear structure and development. Ideas are confusing and hard to follow. Paragraphs lack coherence devices and focus.

**Language (1-5):**
- 5: The writing displays sophisticated control of a wide range of vocabulary and collocations. Grammar and usage are correct throughout. Spelling and punctuation are correct throughout.
- 4: The writing displays good control of vocabulary and collocations. Grammar and usage are generally correct with only minor errors. Spelling and punctuation are generally correct.
- 3: The writing displays adequate vocabulary with some variety. Grammar and usage are acceptable with some errors that do not impede understanding. Spelling and punctuation are mostly correct.
- 2: The writing displays limited vocabulary with little variety. Grammar and usage contain frequent errors that sometimes impede understanding. Spelling and punctuation errors are noticeable.
- 1: The writing displays very limited vocabulary. Grammar and usage contain numerous errors that significantly impede understanding. Spelling and punctuation errors are frequent.

PROMPT: {prompt}

ESSAY: {essay}

Please provide scores for each criterion and calculate the total score. Format your response as follows:
Content: [score]
Organization: [score]
Language: [score]
Total: [sum of three scores]

Provide a brief justification (1-2 sentences) for each score.
")
}

claude_rubric_score <- claude_plus(prompt = claude_all_rubric_prompt(focal_prompt, focal_essay),
                                   temperature = 0)
claude_rubric_df <- extract_scores_df(claude_rubric_score)

for(i in 1:19){
  
  claude_rubric_score_again <- claude_plus(prompt = claude_all_rubric_prompt(focal_prompt, focal_essay),
                                           temperature = 0)
  claude_rubric_one <- extract_scores_df(claude_rubric_score_again)
  claude_rubric_df <- rbind(claude_rubric_df, claude_rubric_one)
  
}

rownames(claude_rubric_df) <- NULL
claude_rubric_summary <- summarize_scores(claude_rubric_df)
claude_rubric_summary



## Batch Scoring

# Step 1: Read essays and create batch requests
create_batch_requests <- function(essays_df, n_repetitions = 20){
  
  # Create a list to store batch requests
  batch_requests <- list()
  request_counter <- 1
  
  # Create n_repetitions requests for each essay (grouped by essay)
  for(i in 1:nrow(essays_df)){
    essay_prompt <- claude_all_rubric_prompt(essays_df$prompt[i], essays_df$essay[i])
    
    # Create 20 requests for this essay
    for(rep in 1:n_repetitions){
      request <- list(
        custom_id = paste0("essay_", essays_df$id[i], "_rep_", rep),
        params = list(
          model = "claude-sonnet-4-20250514",
          max_tokens = 1024,
          messages = list(
            list(
              role = "user",
              content = essay_prompt
            )
          )
        )
      )
      
      batch_requests[[request_counter]] <- request
      request_counter <- request_counter + 1
    }
  }
  
  cat("Created", length(batch_requests), "batch requests for", nrow(essays_df), "essays\n")
  
  return(batch_requests)
}

essay_batch_requests <- create_batch_requests(student_essays)

# Step 2: Write requests to JSONL file
write_batch_file <- function(batch_requests, output_file = "batch_requests.jsonl"){
  # Write each request as a JSON line
  jsonl_lines <- sapply(batch_requests, function(req){
    toJSON(req, auto_unbox = TRUE)
  })
  
  writeLines(jsonl_lines, output_file)
  return(output_file)
}

essay_batch_jsonl <- write_batch_file(essay_batch_requests)

# Step 3: Submit batch job to Anthropic
submit_batch <- function(jsonl_file){
  # Read and parse each line of the JSONL file
  jsonl_lines <- readLines(jsonl_file)
  
  # Parse each JSON line into a list
  requests_list <- lapply(jsonl_lines, function(line){
    fromJSON(line, simplifyVector = FALSE)
  })
  
  # Create batch
  response <- POST(
    url = "https://api.anthropic.com/v1/messages/batches",
    add_headers(
      "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ),
    body = toJSON(list(
      requests = requests_list
    ), auto_unbox = TRUE),
    encode = "json"
  )
  
  if(status_code(response) != 200){
    stop("Error submitting batch: ", content(response, "text"))
  }
  
  result <- content(response, "parsed")
  cat("Batch submitted successfully!\n")
  cat("Batch ID:", result$id, "\n")
  
  return(result)
}

essay_batch_info <- submit_batch(essay_batch_jsonl)

# Step 4: Check batch status
check_batch_status <- function(batch_id){
  response <- GET(
    url = paste0("https://api.anthropic.com/v1/messages/batches/", batch_id),
    add_headers(
      "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01"
    )
  )
  
  if(status_code(response) != 200){
    stop("Error checking batch status: ", content(response, "text"))
  }
  
  result <- content(response, "parsed")
  return(result)
}

# Periodically check how it's going
essay_status <- check_batch_status(essay_batch_info$id)

# Can use the following to check how long it took to complete the task 
library(lubridate)
batch_start_time <- ymd_hms(essay_status$created_at)
batch_stop_time <- ymd_hms(essay_status$ended_at)
batch_run_time <- as.numeric(batch_stop_time - batch_start_time)

# Step 5: Saving batch results

get_batch_results <- function(batch_id){
  response <- GET(
    url = paste0("https://api.anthropic.com/v1/messages/batches/", batch_id, "/results"),
    add_headers(
      "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01"
    )
  )
  
  if(status_code(response) != 200){
    stop("Error retrieving results: ", content(response, "text"))
  }
  
  # Parse JSONL results
  results_text <- content(response, "text", encoding = "UTF-8")
  results_lines <- strsplit(results_text, "\n")[[1]]
  results_list <- lapply(results_lines[results_lines != ""], fromJSON)
  
  return(results_list)
}

essay_batch_results <- get_batch_results(essay_batch_info$id)

# Step 6: Extract scores from batch results
extract_batch_scores <- function(results_list, debug = FALSE){
  scores_df <- data.frame(
    essay_id = character(),
    repetition = integer(),
    content = numeric(),
    organization = numeric(),
    language = numeric(),
    total = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(i in seq_along(results_list)){
    result <- results_list[[i]]
    
    if(result$result$type == "succeeded"){
      # Extract the response text - handle different possible structures
      message_content <- result$result$message$content
      
      # Debug: Print structure if requested
      if(debug && i == 1){
        cat("=== DEBUG: First result structure ===\n")
        cat("Content class:", class(message_content), "\n")
        cat("Content length:", length(message_content), "\n")
        if(is.list(message_content) && length(message_content) > 0){
          cat("First element class:", class(message_content[[1]]), "\n")
          print(str(message_content[[1]]))
        }
      }
      
      # Extract text from various possible structures
      response_text <- NULL
      
      if(is.data.frame(message_content)){
        # If it's a data frame, look for a 'text' column
        if("text" %in% names(message_content)){
          response_text <- message_content$text[1]
        }
      } else if(is.list(message_content) && length(message_content) > 0){
        first_elem <- message_content[[1]]
        
        if(is.list(first_elem)){
          # Check for $text field
          if(!is.null(first_elem$text)){
            response_text <- first_elem$text
          } else if(!is.null(first_elem$type) && first_elem$type == "text" && !is.null(first_elem$text)){
            response_text <- first_elem$text
          }
        } else if(is.character(first_elem)){
          response_text <- first_elem
        }
      } else if(is.character(message_content)){
        response_text <- message_content
      }
      
      # If we still don't have text, try to extract it more aggressively
      if(is.null(response_text) || length(response_text) == 0){
        # Try to find any 'text' field in the structure
        if(is.list(message_content)){
          text_fields <- unlist(lapply(message_content, function(x) {
            if(is.list(x) && "text" %in% names(x)) x$text else NULL
          }))
          if(length(text_fields) > 0) response_text <- text_fields[1]
        }
      }
      
      if(is.null(response_text) || length(response_text) == 0){
        warning("Could not extract text for ", result$custom_id)
        if(debug){
          cat("=== Full structure for ", result$custom_id, " ===\n")
          print(str(message_content))
        }
        next
      }
      
      # Debug: Print first response text
      if(debug && i == 1){
        cat("\n=== First response text (first 500 chars) ===\n")
        cat(substr(response_text, 1, 500), "\n\n")
      }
      
      # Extract scores using our previous function
      scores <- extract_scores(response_text, debug = debug && i == 1)
      
      # Debug: Check if scores were extracted
      if(debug && i == 1){
        cat("=== Extracted scores ===\n")
        print(scores)
      }
      
      # Parse custom_id to extract essay_id and repetition
      # Format: essay_2001_rep_1
      custom_id_parts <- strsplit(result$custom_id, "_")[[1]]
      essay_id <- custom_id_parts[2]
      repetition <- as.integer(custom_id_parts[4])
      
      scores_df <- rbind(scores_df, data.frame(
        essay_id = essay_id,
        repetition = repetition,
        content = scores["content"],
        organization = scores["organization"],
        language = scores["language"],
        total = scores["total"]
      ))
    } else {
      warning("Request ", result$custom_id, " failed: ", result$result$error$message)
    }
  }
  
  return(scores_df)
}

essay_batch_df <- extract_batch_scores(essay_batch_results, debug = TRUE)

# Step 7: View Batch Results

library(dplyr)
library(DT)

essay_summary_detailed <- essay_batch_df %>%
  group_by(essay_id) %>%
  summarise(
    n = n(),
    # Content
    content_mean = mean(content, na.rm = TRUE),
    content_sd = sd(content, na.rm = TRUE),
    content_min = min(content, na.rm = TRUE),
    content_max = max(content, na.rm = TRUE),
    # Organization
    org_mean = mean(organization, na.rm = TRUE),
    org_sd = sd(organization, na.rm = TRUE),
    org_min = min(organization, na.rm = TRUE),
    org_max = max(organization, na.rm = TRUE),
    # Language
    lang_mean = mean(language, na.rm = TRUE),
    lang_sd = sd(language, na.rm = TRUE),
    lang_min = min(language, na.rm = TRUE),
    lang_max = max(language, na.rm = TRUE),
    # Total
    total_mean = mean(total, na.rm = TRUE),
    total_sd = sd(total, na.rm = TRUE),
    total_min = min(total, na.rm = TRUE),
    total_max = max(total, na.rm = TRUE),
  ) %>%
  arrange(total_mean)

essay_summary_detailed <- essay_summary_detailed %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

datatable(essay_summary_detailed, 
          options = list(pageLength = 10, scrollX = TRUE),
          caption = "Summary Statistics by Essay (20 repetitions each)")



### Batch Processing with Temp = 0

create_lowtemp_requests <- function(essays_df, n_repetitions = 20){
  
  # Create a list to store batch requests
  batch_requests <- list()
  request_counter <- 1
  
  # Create n_repetitions requests for each essay (grouped by essay)
  for(i in 1:nrow(essays_df)){
    essay_prompt <- claude_all_rubric_prompt(essays_df$prompt[i], essays_df$essay[i])
    
    # Create 20 requests for this essay
    for(rep in 1:n_repetitions){
      request <- list(
        custom_id = paste0("essay_", essays_df$id[i], "_rep_", rep),
        params = list(
          model = "claude-sonnet-4-20250514",
          max_tokens = 1024,
          temperature = 0, # added low temp
          messages = list(
            list(
              role = "user",
              content = essay_prompt
            )
          )
        )
      )
      
      batch_requests[[request_counter]] <- request
      request_counter <- request_counter + 1
    }
  }
  
  cat("Created", length(batch_requests), "batch requests for", nrow(essays_df), "essays\n")
  
  return(batch_requests)
}

lowtemp_batch_requests <- create_lowtemp_requests(student_essays)
lowtemp_batch_jsonl <- write_batch_file(lowtemp_batch_requests)
lowtemp_batch_info <- submit_batch(lowtemp_batch_jsonl)
# Batch submitted successfully!
# Batch ID: msgbatch_0184nJ7Kui9v6WVVh47939mn 

lowtemp_status <- check_batch_status(lowtemp_batch_info$id)
lowtemp_start_time <- ymd_hms(lowtemp_status$created_at)
lowtemp_stop_time <- ymd_hms(lowtemp_status$ended_at)
lowtemp_run_time <- as.numeric(lowtemp_stop_time - lowtemp_start_time)
# Time difference of 1.473892 mins

lowtemp_batch_results <- get_batch_results(lowtemp_batch_info$id)
lowtemp_batch_df <- extract_batch_scores(lowtemp_batch_results)

lowtemp_summary_detailed <- lowtemp_batch_df %>%
  group_by(essay_id) %>%
  summarise(
    n = n(),
    # Content
    content_mean = mean(content, na.rm = TRUE),
    content_sd = sd(content, na.rm = TRUE),
    content_min = min(content, na.rm = TRUE),
    content_max = max(content, na.rm = TRUE),
    # Organization
    org_mean = mean(organization, na.rm = TRUE),
    org_sd = sd(organization, na.rm = TRUE),
    org_min = min(organization, na.rm = TRUE),
    org_max = max(organization, na.rm = TRUE),
    # Language
    lang_mean = mean(language, na.rm = TRUE),
    lang_sd = sd(language, na.rm = TRUE),
    lang_min = min(language, na.rm = TRUE),
    lang_max = max(language, na.rm = TRUE),
    # Total
    total_mean = mean(total, na.rm = TRUE),
    total_sd = sd(total, na.rm = TRUE),
    total_min = min(total, na.rm = TRUE),
    total_max = max(total, na.rm = TRUE),
  ) %>%
  arrange(total_mean)

lowtemp_summary_detailed <- lowtemp_summary_detailed %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

datatable(lowtemp_summary_detailed, 
          options = list(pageLength = 10, scrollX = TRUE),
          caption = "Summary Statistics by Essay (20 repetitions each) - Temperature = 0")

