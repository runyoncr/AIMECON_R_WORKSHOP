library(httr)

# Function to call the GPT-5 API
call_gpt5_api <- function(content, 
                          bearer_token, 
                          api_key, 
                          url, 
                          model = "gpt-5",
                          reasoning_effort = "medium",
                          verbosity = "medium",
                          max_completion_tokens = 5000) {
  
  headers <- add_headers(
    Authorization = paste("Bearer", bearer_token),
    `Content-Type` = "application/json",
    `x-api-key` = api_key
  )
  
  body <- list(
    model = model,
    messages = list(
      list(
        role = "user",
        content = content
      )
    ),
    reasoning_effort = reasoning_effort,
    verbosity = verbosity,
    max_completion_tokens = max_completion_tokens
  )
  
  response <- POST(
    url,
    headers,
    body = body,
    encode = "json"
  )
  
  if (status_code(response) == 200) {
    return(content(response))
  } else {
    return(list(error = paste0("Status ", status_code(response), ": ", content(response, "text"))))
  }
}

# Main function to interact with GPT-5
gpt5 <- function(gpt_prompt, 
                 reasoning_effort = "medium",
                 verbosity = "medium",
                 max_completion_tokens = 5000,
                 model = "gpt-5") {
  
  # Get fresh credentials
  secure_access_token <- get_access_token()
  api_key <- Sys.getenv("API_KEY_ENV_VAR")
  endpoint_url <- Sys.getenv("ENDPOINT_GPT5_ENV_VAR")
  
  # Debug: check if credentials are valid (uncomment if needed)
  # cat("Token starts with:", substr(secure_access_token, 1, 10), "\n")
  # cat("API key starts with:", substr(api_key, 1, 10), "\n")
  # cat("URL:", endpoint_url, "\n")
  
  # Call API
  prompt_response <- call_gpt5_api(
    content = gpt_prompt,
    bearer_token = secure_access_token,
    api_key = api_key,
    url = endpoint_url,
    model = model,
    reasoning_effort = reasoning_effort,
    verbosity = verbosity,
    max_completion_tokens = max_completion_tokens
  )
  
  # Extract response with error handling
  if ("error" %in% names(prompt_response)) {
    stop("API Error: ", prompt_response$error)
  }
  
  gpt_response <- prompt_response$choices[[1]]$message$content
  return(gpt_response)
}

# Usage examples:
# Basic usage with defaults (medium reasoning & verbosity)
# result <- gpt5("Explain quantum computing")

# Quick response with minimal reasoning
# result <- gpt5("What is 2+2?", reasoning_effort = "minimal", verbosity = "low")

# Deep analysis with high reasoning
# result <- gpt5("Analyze this complex problem...", reasoning_effort = "high", verbosity = "high")

# Using GPT-5 mini for cost savings
# result <- gpt5("Simple question", model = "gpt-5-mini")

# Using GPT-5 nano for fastest response
# result <- gpt5("Quick query", model = "gpt-5-nano")