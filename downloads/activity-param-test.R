# Syntax for "Activity: Generation Parameter Testing"

library(httr)
library(jsonlite)

# Updated call_claude to send parameters to the model
claude_plus <- function(prompt,
                        model = "claude-sonnet-4-5-20250929",
                        temperature = NULL,
                        top_p = NULL,
                        top_k = NULL,
                        max_tokens = 1024) {
  
  # Check if both temperature and top_p are supplied
  # if (!is.null(temperature) && !is.null(top_p)) {
  #   warning("Both temperature and top_p arguments are supplied. The Anthropic API does not support using both simultaneously. Only temperature will be used.")
  # }
  
  # Get API key from environment
  # You will need to change this to your own API key after workshop
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  # Convert text prompt to required message format
  messages <- list(list(role = "user", content = prompt))
  
  # Build request body
  request_body <- list(
    model = model,
    messages = messages,
    max_tokens = max_tokens # Required; will be an argument in other functions
  )
  
  # Add temperature if provided (takes precedence over top_p)
  # if (!is.null(temperature)) {
  request_body$temperature <- temperature
  # } else if (!is.null(top_p)) {
  #   # Only add top_p if temperature is not provided
  request_body$top_p <- top_p
  # }
  # 
  # Add top_k if provided
  if (!is.null(top_k)) {
    request_body$top_k <- top_k
  }
  
  # Set up headers
  headers <- add_headers(
    "x-api-key" = api_key,
    "anthropic-version" = "2023-06-01",
    "content-type" = "application/json"
  )
  
  # Make the API request
  response <- POST(
    url = "https://api.anthropic.com/v1/messages",
    headers,
    body = toJSON(request_body, auto_unbox = TRUE)
  )
  
  # Check if request was successful
  if (http_status(response)$category != "Success") {
    stop(paste("API request failed:", http_status(response)$message, 
               "\nDetails:", content(response, "text", encoding = "UTF-8")))
  }
  
  # Parse response and extract text content
  result <- fromJSON(content(response, "text", encoding = "UTF-8"))
  return(as.character(result$content)[2])
}


# Wrapper function to complete several tests in succession
# n_reps range: 5 to 20

claude_param_test <- function(prompt,
                              temperature = NULL,
                              top_p = NULL,
                              top_k = NULL,
                              max_tokens = 1024,
                              n_reps = 5) {
  
  # Check if n_reps is within allowed range
  if (n_reps > 20) {
    warning("n_reps exceeds maximum allowed value of 20. Setting n_reps to 20.")
    n_reps <- 20
  }
  
  if (n_reps < 5) {
    warning("n_reps is below minimum value of 5. Setting n_reps to 5.")
    n_reps <- 5
  }
  
  # Initialize results dataframe
  results <- data.frame(
    rep_n = integer(),
    temp = numeric(),
    top_p = numeric(),
    top_k = integer(),
    output = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop through n_reps
  for (i in 1:n_reps) {
    # Call claude_plus
    output <- claude_plus(
      prompt = prompt,
      temperature = temperature,
      top_p = top_p,
      top_k = top_k,
      max_tokens = max_tokens
    )
    
    # Add to results
    results <- rbind(results, data.frame(
      rep_n = i,
      temp = ifelse(is.null(temperature), NA, temperature),
      top_p = ifelse(is.null(top_p), NA, top_p),
      top_k = ifelse(is.null(top_k), NA, top_k),
      output = output,
      stringsAsFactors = FALSE
    ))
    
    # Sleep between calls (except after last call)
    if (i < n_reps) {
      Sys.sleep(0.2)
    }
  }
  
  return(results)
}

# High temperature pizza prompt test
high_pizza <- claude_param_test("In 15 words or fewer, tell me why pizza is so good.",
                                temperature = 1,
                                n_reps = 10)
library(knitr)
kable(high_pizza)

# Low temperature pizza prompt test
low_pizza <- claude_param_test("In 15 words or fewer, tell me why pizza is so good.",
                               temperature = 0,
                               n_reps = 10)
kable(low_pizza)

# Ed Measurement Essay prompt test
edmeasure_low <- claude_param_test("Write a short essay about the importance of educational measurement.",
                                   temperature = 0,
                                   n_reps = 5,
                                   max_tokens = 2048)
edmeasure_low$output[1]
edmeasure_low$output[3]
edmeasure_low$output[5]

# High top_p pizza prompt test
highp_pizza <- claude_param_test("In 15 words or fewer, tell me why pizza is so good.",
                                 top_p = .90,
                                 n_reps = 10)
kable(highp_pizza)

# Low top_p pizza prompt test
lowp_pizza <- claude_param_test("In 15 words or fewer, tell me why pizza is so good.",
                                top_p = .10,
                                n_reps = 10)
kable(lowp_pizza)

