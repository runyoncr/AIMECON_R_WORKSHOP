library(httr)
library(jsonlite)

call_gpt52 <- function(prompt,
                       model = "gpt-5.2",
                       system = NULL,
                       max_tokens = 4096,
                       reasoning_effort = "low") {  # "none", "low", "medium", "high", "xhigh"
  
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") {
    stop("OpenAI API key is not set. Please set OPENAI_API_KEY in your environment.")
  }
  
  # Build messages list
  messages <- list(list(role = "user", content = prompt))
  
  # Add system message if provided
  if (!is.null(system)) {
    messages <- c(list(list(role = "system", content = system)), messages)
  }
  
  # Build request body
  body_list <- list(
    model = model,
    messages = messages,
    max_completion_tokens = max_tokens,
    reasoning = list(effort = reasoning_effort)
  )
  
  # Set up headers
  headers <- add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )
  
  # Make the API request
  resp <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    headers,
    body = toJSON(body_list, auto_unbox = TRUE)
  )
  
  # Check if request was successful
  if (http_status(resp)$category != "Success") {
    stop(paste0("API request failed (", http_status(resp)$message, "): ",
                content(resp, "text", encoding = "UTF-8")))
  }
  
  # Parse response and extract text content
  res <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = TRUE)
  return(res$choices[[1]]$message$content)
  
}

# Example usages:
# 1. Default call
# cat(call_gpt52("Explain item response theory in simple terms."))

# 2. Lowest reasoning effort
# cat(call_gpt52("Explain item response theory in simple terms.",
#                reasoning_effort = "none"))

# 3. Highest reasoning effort
# cat(call_gpt52("Explain item response theory in simple terms.",
#                reasoning_effort = "xhigh"))

# 4. With a system prompt
# cat(call_gpt52("Explain item response theory in simple terms.",
#                system = "You are an expert in educational measurement. Respond concisely."))