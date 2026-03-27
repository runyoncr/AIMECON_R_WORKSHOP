library(httr)
library(jsonlite)

claude_plus <- function(prompt,
                        model = "claude-sonnet-4-5-20250929",
                        system = NULL,
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
  
  # Add system prompt if provided
  if (!is.null(system)) {
    request_body$system <- system
  }
  
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