# MCQ Generator Shiny App
# Single-file app with field locking and AI-powered regeneration

library(shiny)
library(bslib)
library(httr2)

# ==============================================================================
# AI HELPER FUNCTIONS
# ==============================================================================

#' Generate a complete MCQ item via AI
#'
#' @param grade Grade level (e.g., "1st", "3rd", "5th", "7th")
#' @param subject Subject (e.g., "Geography", "Science", "Social Studies")
#' @return Named list with: question, optionA, optionB, optionC, optionD, optionE, correctAnswer
generate_item_via_ai <- function(grade, subject) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") {
    stop("ANTHROPIC_API_KEY not found in .Renviron")
  }
  
  system_prompt <- "You are an expert assessment item writer. Produce age-appropriate MCQs with exactly five options (A-E) and a single correct answer. Follow the output format strictly."
  
  user_prompt <- sprintf(
    "Context: Grade = %s, Subject = %s\n\nGenerate a multiple-choice question suitable for this grade level and subject.\n\nOutput Format (required):\nQuestion: <text>\nOption A: <text>\nOption B: <text>\nOption C: <text>\nOption D: <text>\nOption E: <text>\nCorrect Answer: Option <A|B|C|D|E>: <repeat the correct option text verbatim>\n\nEnsure the correct answer key and text match one of A-E. Keep language at the specified grade level.",
    grade, subject
  )
  
  response <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |>
    req_body_json(list(
      model = "claude-sonnet-4-20250514",
      max_tokens = 1024,
      system = system_prompt,
      messages = list(
        list(role = "user", content = user_prompt)
      )
    )) |>
    req_perform() |>
    resp_body_json()
  
  content_text <- response$content[[1]]$text
  return(parse_ai_output(content_text))
}

#' Re-generate an MCQ item via AI with locked fields
#'
#' @param grade Grade level
#' @param subject Subject
#' @param instructions Free-text instructions for regeneration
#' @param locked_values Named list of locked fields (subset of: question, optionA-E, correctAnswer)
#' @return Named list with all 7 fields (locked fields unchanged)
regenerate_item_via_ai <- function(grade, subject, instructions, locked_values) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") {
    stop("ANTHROPIC_API_KEY not found in .Renviron")
  }
  
  system_prompt <- "You are an expert assessment item writer. Produce age-appropriate MCQs with exactly five options (A-E) and a single correct answer. Follow the output format strictly."
  
  locked_fields_text <- ""
  if (length(locked_values) > 0) {
    locked_fields_text <- "\n\nLocked fields (echo these unchanged):\n"
    for (field_name in names(locked_values)) {
      locked_fields_text <- paste0(locked_fields_text, field_name, ": ", locked_values[[field_name]], "\n")
    }
  }
  
  instructions_text <- ""
  if (!is.null(instructions) && nchar(trimws(instructions)) > 0) {
    instructions_text <- paste0("\n\nInstructions: ", instructions)
  }
  
  user_prompt <- sprintf(
    "Context: Grade = %s, Subject = %s%s%s\n\nRe-generate a multiple-choice question. Echo all locked fields unchanged. Generate new content for unlocked fields that is consistent with locked fields, grade, subject, and instructions.\n\nOutput Format (required):\nQuestion: <text>\nOption A: <text>\nOption B: <text>\nOption C: <text>\nOption D: <text>\nOption E: <text>\nCorrect Answer: Option <A|B|C|D|E>: <repeat the correct option text verbatim>\n\nReturn all fields. Ensure the correct answer key and text match one of A-E. Keep language at the specified grade level.",
    grade, subject, locked_fields_text, instructions_text
  )
  
  response <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |>
    req_body_json(list(
      model = "claude-sonnet-4-20250514",
      max_tokens = 1024,
      system = system_prompt,
      messages = list(
        list(role = "user", content = user_prompt)
      )
    )) |>
    req_perform() |>
    resp_body_json()
  
  content_text <- response$content[[1]]$text
  parsed <- parse_ai_output(content_text)
  
  # Validate locked fields
  for (field_name in names(locked_values)) {
    if (!identical(parsed[[field_name]], locked_values[[field_name]])) {
      warning(paste("AI violated lock on field:", field_name))
      parsed[[field_name]] <- locked_values[[field_name]]
    }
  }
  
  return(parsed)
}

#' Parse AI output text into structured format
parse_ai_output <- function(text) {
  lines <- strsplit(text, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]
  
  result <- list()
  
  for (line in lines) {
    if (grepl("^Question:", line, ignore.case = TRUE)) {
      result$question <- trimws(sub("^Question:", "", line, ignore.case = TRUE))
    } else if (grepl("^Option A:", line, ignore.case = TRUE)) {
      result$optionA <- trimws(sub("^Option A:", "", line, ignore.case = TRUE))
    } else if (grepl("^Option B:", line, ignore.case = TRUE)) {
      result$optionB <- trimws(sub("^Option B:", "", line, ignore.case = TRUE))
    } else if (grepl("^Option C:", line, ignore.case = TRUE)) {
      result$optionC <- trimws(sub("^Option C:", "", line, ignore.case = TRUE))
    } else if (grepl("^Option D:", line, ignore.case = TRUE)) {
      result$optionD <- trimws(sub("^Option D:", "", line, ignore.case = TRUE))
    } else if (grepl("^Option E:", line, ignore.case = TRUE)) {
      result$optionE <- trimws(sub("^Option E:", "", line, ignore.case = TRUE))
    } else if (grepl("^Correct Answer:", line, ignore.case = TRUE)) {
      result$correctAnswer <- trimws(sub("^Correct Answer:", "", line, ignore.case = TRUE))
    }
  }
  
  return(result)
}

# ==============================================================================
# VALIDATION FUNCTIONS
# ==============================================================================

validate_item <- function(item) {
  errors <- c()
  
  # Check question
  if (is.null(item$question) || nchar(trimws(item$question)) == 0) {
    errors <- c(errors, "Question cannot be empty")
  }
  
  # Check all options
  required_options <- c("optionA", "optionB", "optionC", "optionD", "optionE")
  for (opt in required_options) {
    if (is.null(item[[opt]]) || nchar(trimws(item[[opt]])) == 0) {
      errors <- c(errors, paste("Option", substr(opt, 7, 7), "cannot be empty"))
    }
  }
  
  # Check correct answer format
  if (is.null(item$correctAnswer) || nchar(trimws(item$correctAnswer)) == 0) {
    errors <- c(errors, "Correct Answer cannot be empty")
  } else {
    # Extract key (A-E)
    correct_key <- extract_correct_key(item$correctAnswer)
    if (is.na(correct_key)) {
      errors <- c(errors, "Correct Answer must start with 'Option A:', 'Option B:', etc.")
    } else {
      # Verify the text matches the option
      option_field <- paste0("option", correct_key)
      expected_text <- item[[option_field]]
      actual_text <- extract_correct_text(item$correctAnswer)
      
      if (!identical(trimws(expected_text), trimws(actual_text))) {
        errors <- c(errors, paste0("Correct Answer text must match Option ", correct_key, " exactly"))
      }
    }
  }
  
  if (length(errors) > 0) {
    return(list(valid = FALSE, errors = errors))
  } else {
    return(list(valid = TRUE, errors = NULL))
  }
}

extract_correct_key <- function(correct_answer) {
  match <- regexpr("Option ([A-E]):", correct_answer, ignore.case = TRUE)
  if (match > 0) {
    key_text <- regmatches(correct_answer, match)
    key <- sub("Option ([A-E]):.*", "\\1", key_text, ignore.case = TRUE)
    return(toupper(key))
  }
  return(NA)
}

extract_correct_text <- function(correct_answer) {
  text <- sub("^Option [A-E]:\\s*", "", correct_answer, ignore.case = TRUE)
  return(trimws(text))
}

# ==============================================================================
# UI
# ==============================================================================

ui <- page_sidebar(
  title = "MCQ Generator",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    width = 300,
    h4("Configuration"),
    selectInput(
      "grade",
      "Grade Level",
      choices = c("1st", "3rd", "5th", "7th"),
      selected = "3rd"
    ),
    selectInput(
      "subject",
      "Subject",
      choices = c("Geography", "Science", "Social Studies"),
      selected = "Science"
    ),
    actionButton(
      "generate",
      "Generate Item",
      icon = icon("wand-magic-sparkles"),
      class = "btn-primary w-100 mb-3"
    ),
    hr(),
    h4("Re-generation"),
    textAreaInput(
      "regen_instructions",
      "Re-generation Instructions (optional)",
      placeholder = "e.g., focus the question on lava; make distractors conceptually distinct",
      rows = 3
    ),
    actionButton(
      "regen_unlocked",
      "Re-generate Unlocked Fields",
      icon = icon("rotate"),
      class = "btn-warning w-100"
    ),
    helpText("Locked fields will remain unchanged. Unlocked fields will be re-generated.")
  ),
  
  # Main content
  div(
    class = "container-fluid",
    h3("Edit Item"),
    helpText("Change the item as desired. Lock fields you want to preserve before re-generation."),
    
    # Question
    div(
      class = "row mb-3",
      div(
        class = "col-md-10",
        textAreaInput(
          "question",
          "Question",
          value = "",
          rows = 3,
          width = "100%"
        )
      ),
      div(
        class = "col-md-2",
        br(),
        checkboxInput("lock_question", "Lock", FALSE)
      )
    ),
    
    # Options A-E
    lapply(LETTERS[1:5], function(letter) {
      div(
        class = "row mb-2",
        div(
          class = "col-md-10",
          textInput(
            paste0("option", letter),
            paste("Option", letter),
            value = "",
            width = "100%"
          )
        ),
        div(
          class = "col-md-2",
          br(),
          checkboxInput(paste0("lock_option", letter), "Lock", FALSE)
        )
      )
    }),
    
    # Correct Answer
    div(
      class = "row mb-3",
      div(
        class = "col-md-10",
        textInput(
          "correctAnswer",
          "Correct Answer (format: Option X: text)",
          value = "",
          width = "100%",
          placeholder = "e.g., Option A: Pacific Ocean"
        )
      ),
      div(
        class = "col-md-2",
        br(),
        checkboxInput("lock_correctAnswer", "Lock", FALSE)
      )
    ),
    
    # Action buttons
    div(
      class = "row mb-4",
      div(
        class = "col-md-6",
        actionButton(
          "update_item",
          "Update Item",
          icon = icon("check"),
          class = "btn-success w-100"
        )
      ),
      div(
        class = "col-md-6",
        actionButton(
          "save_item",
          "Save Item",
          icon = icon("save"),
          class = "btn-info w-100"
        )
      )
    ),
    
    hr(),
    
    # Feedback area
    h3("Saved Items"),
    uiOutput("feedback_message"),
    tableOutput("saved_items_table")
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # Reactive values for current item
  current_item <- reactiveValues(
    grade = NULL,
    subject = NULL,
    question = "",
    optionA = "",
    optionB = "",
    optionC = "",
    optionD = "",
    optionE = "",
    correctAnswer = "",
    timestamp = NULL
  )
  
  # Saved items list
  saved_items <- reactiveVal(list())
  
  # Load saved items on startup
  observe({
    if (file.exists("saved_items.rds")) {
      tryCatch({
        loaded <- readRDS("saved_items.rds")
        saved_items(loaded)
        showNotification(
          paste("Loaded", length(loaded), "saved items"),
          type = "message"
        )
      }, error = function(e) {
        showNotification(
          paste("Error loading saved items:", e$message),
          type = "error"
        )
      })
    }
  })
  
  # Generate Item
  observeEvent(input$generate, {
    tryCatch({
      showNotification("Generating item...", type = "message", duration = 2)
      
      item <- generate_item_via_ai(input$grade, input$subject)
      
      # Update UI fields
      updateTextAreaInput(session, "question", value = item$question)
      updateTextInput(session, "optionA", value = item$optionA)
      updateTextInput(session, "optionB", value = item$optionB)
      updateTextInput(session, "optionC", value = item$optionC)
      updateTextInput(session, "optionD", value = item$optionD)
      updateTextInput(session, "optionE", value = item$optionE)
      updateTextInput(session, "correctAnswer", value = item$correctAnswer)
      
      # Update current_item
      current_item$grade <- input$grade
      current_item$subject <- input$subject
      current_item$question <- item$question
      current_item$optionA <- item$optionA
      current_item$optionB <- item$optionB
      current_item$optionC <- item$optionC
      current_item$optionD <- item$optionD
      current_item$optionE <- item$optionE
      current_item$correctAnswer <- item$correctAnswer
      current_item$timestamp <- Sys.time()
      
      showNotification("Item generated successfully!", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error generating item:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Re-generate Unlocked Fields
  observeEvent(input$regen_unlocked, {
    tryCatch({
      showNotification("Re-generating unlocked fields...", type = "message", duration = 2)
      
      # Build locked_values from checkbox states
      locked_values <- list()
      
      if (input$lock_question) {
        locked_values$question <- input$question
      }
      if (input$lock_optionA) {
        locked_values$optionA <- input$optionA
      }
      if (input$lock_optionB) {
        locked_values$optionB <- input$optionB
      }
      if (input$lock_optionC) {
        locked_values$optionC <- input$optionC
      }
      if (input$lock_optionD) {
        locked_values$optionD <- input$optionD
      }
      if (input$lock_optionE) {
        locked_values$optionE <- input$optionE
      }
      if (input$lock_correctAnswer) {
        locked_values$correctAnswer <- input$correctAnswer
      }
      
      # Call regeneration
      item <- regenerate_item_via_ai(
        input$grade,
        input$subject,
        input$regen_instructions,
        locked_values
      )
      
      # Validate that locked fields were respected
      violated_locks <- c()
      for (field in names(locked_values)) {
        if (!identical(trimws(item[[field]]), trimws(locked_values[[field]]))) {
          violated_locks <- c(violated_locks, field)
          # Restore locked value
          item[[field]] <- locked_values[[field]]
        }
      }
      
      if (length(violated_locks) > 0) {
        showNotification(
          paste("Warning: AI violated locks on:", paste(violated_locks, collapse = ", "), ". Values restored."),
          type = "warning",
          duration = 5
        )
      }
      
      # Update UI fields
      updateTextAreaInput(session, "question", value = item$question)
      updateTextInput(session, "optionA", value = item$optionA)
      updateTextInput(session, "optionB", value = item$optionB)
      updateTextInput(session, "optionC", value = item$optionC)
      updateTextInput(session, "optionD", value = item$optionD)
      updateTextInput(session, "optionE", value = item$optionE)
      updateTextInput(session, "correctAnswer", value = item$correctAnswer)
      
      # Update current_item
      current_item$grade <- input$grade
      current_item$subject <- input$subject
      current_item$question <- item$question
      current_item$optionA <- item$optionA
      current_item$optionB <- item$optionB
      current_item$optionC <- item$optionC
      current_item$optionD <- item$optionD
      current_item$optionE <- item$optionE
      current_item$correctAnswer <- item$correctAnswer
      current_item$timestamp <- Sys.time()
      
      showNotification("Item re-generated successfully!", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error re-generating item:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Update Item
  observeEvent(input$update_item, {
    item <- list(
      question = input$question,
      optionA = input$optionA,
      optionB = input$optionB,
      optionC = input$optionC,
      optionD = input$optionD,
      optionE = input$optionE,
      correctAnswer = input$correctAnswer
    )
    
    validation <- validate_item(item)
    
    if (!validation$valid) {
      showNotification(
        paste("Validation errors:", paste(validation$errors, collapse = "; ")),
        type = "error",
        duration = 5
      )
    } else {
      current_item$grade <- input$grade
      current_item$subject <- input$subject
      current_item$question <- item$question
      current_item$optionA <- item$optionA
      current_item$optionB <- item$optionB
      current_item$optionC <- item$optionC
      current_item$optionD <- item$optionD
      current_item$optionE <- item$optionE
      current_item$correctAnswer <- item$correctAnswer
      current_item$timestamp <- Sys.time()
      
      showNotification("Item updated successfully!", type = "message")
    }
  })
  
  # Save Item
  observeEvent(input$save_item, {
    item <- list(
      grade = input$grade,
      subject = input$subject,
      question = input$question,
      optionA = input$optionA,
      optionB = input$optionB,
      optionC = input$optionC,
      optionD = input$optionD,
      optionE = input$optionE,
      correctAnswer = input$correctAnswer
    )
    
    validation <- validate_item(item)
    
    if (!validation$valid) {
      showNotification(
        paste("Cannot save. Validation errors:", paste(validation$errors, collapse = "; ")),
        type = "error",
        duration = 5
      )
    } else {
      # Add timestamp
      item$timestamp <- Sys.time()
      
      # Append to saved items
      current_saved <- saved_items()
      current_saved[[length(current_saved) + 1]] <- item
      saved_items(current_saved)
      
      # Save to file
      tryCatch({
        saveRDS(current_saved, "saved_items.rds")
        showNotification(
          paste("Item saved successfully! Total items:", length(current_saved)),
          type = "message",
          duration = 3
        )
      }, error = function(e) {
        showNotification(
          paste("Error saving to file:", e$message),
          type = "error",
          duration = 5
        )
      })
    }
  })
  
  # Render saved items table
  output$saved_items_table <- renderTable({
    items <- saved_items()
    
    if (length(items) == 0) {
      return(data.frame(
        Message = "No items saved yet"
      ))
    }
    
    # Create summary table
    summary_df <- do.call(rbind, lapply(seq_along(items), function(i) {
      item <- items[[i]]
      data.frame(
        Index = i,
        Grade = item$grade,
        Subject = item$subject,
        Question = substr(item$question, 1, 60),
        Timestamp = as.character(item$timestamp),
        stringsAsFactors = FALSE
      )
    }))
    
    summary_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Feedback message
  output$feedback_message <- renderUI({
    items <- saved_items()
    if (length(items) > 0) {
      div(
        class = "alert alert-info",
        icon("circle-info"),
        paste("Total saved items:", length(items))
      )
    }
  })
}

# ==============================================================================
# RUN APP
# ==============================================================================

shinyApp(ui = ui, server = server)