library(httr)
library(jsonlite)

call_claude <- function(prompt,
                        model = "claude-sonnet-4-5-20250929") {
  
  # Get API key from environment
  # You will need to change this to your own API key after workshop
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")

  # Convert text prompt to required message format
  messages <- list(list(role = "user", content = prompt))
  
  # Build request body
  request_body <- list(
    model = model,
    messages = messages,
    max_tokens = 1024 # Required; will be an argument in other functions
  )
  
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