library(httr)
library(jsonlite)

call_claude <- function(prompt,
                        model = "claude-sonnet-5",
                        system = NULL,
                        max_tokens = 4096,
                        effort = "low") {
  
  # Get API key from environment
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  
  # Convert text prompt to required message format
  messages <- list(list(role = "user", content = prompt))
  
  # Build request body
  # Note: temperature/top_k/top_p are deprecated for models released after
  # Claude Opus 4.6 (incl. claude-sonnet-5) and will cause a 400 error if set
  # to anything other than their defaults, so they are intentionally omitted.
  # Use `effort` (low/medium/high/xhigh/max) to control response behavior instead.
  request_body <- list(
    model = model,
    messages = messages,
    max_tokens = max_tokens,
    output_config = list(effort = effort)
  )
  
  # Add system prompt if provided
  if (!is.null(system)) {
    request_body$system <- system
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
  return(result$content$text)
  
}
